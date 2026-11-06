# R/fetch_challenges.R
# Pull every ABS challenge of the 2026 regular season from the MLB Stats API
# game feed, then validate against each game's official challenge totals.
#
#   gameData.absChallenges -> official per-team totals (used for validation)
#   reviewDetails          -> one per challenge, found in TWO places:
#     1. allPlays[].playEvents[].reviewDetails  (challenge mid at-bat)
#     2. allPlays[].reviewDetails               (challenge on the at-bat's final
#        pitch, e.g. strike three / ball four; attached to the play, not the pitch)
# reviewType "MJ" = ABS ball/strike challenge (other codes = video replay).
#
#   at_bat_number = atBatIndex + 1     (feed is 0-indexed, Statcast 1-indexed)
#   pitch_number  = pitchNumber

library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(arrow)

RAW_DIR     <- file.path("data", "raw", "challenges")
SEASON_FILE <- file.path("data", "statcast_2026.parquet")
OUT_CHAL    <- file.path("data", "challenges_2026.parquet")
OUT_SUMM    <- file.path("data", "challenge_summary_2026.parquet")

FEED_FIELDS <- paste(c(
  "gameData", "absChallenges", "hasChallenges", "usedSuccessful", "usedFailed",
  "remaining", "teams", "home", "away", "id",
  "liveData", "plays", "allPlays", "about", "atBatIndex", "inning",
  "isTopInning", "playEvents", "pitchNumber", "isPitch", "reviewDetails",
  "isOverturned", "reviewType", "challengeTeamId", "player", "details", "code"
), collapse = ",")

feed_url <- function(pk) {
  paste0("https://statsapi.mlb.com/api/v1.1/game/", pk,
         "/feed/live?fields=", FEED_FIELDS)
}

raw_path <- function(pk) file.path(RAW_DIR, paste0(pk, ".json.gz"))

# ---- Fetch (network) --------------------------------------------------------

fetch_game <- function(pk, retries = 3) {
  path <- raw_path(pk)
  if (file.exists(path)) return(invisible("cached"))
  
  for (attempt in seq_len(retries)) {
    txt <- tryCatch(
      paste(readLines(url(feed_url(pk)), warn = FALSE), collapse = ""),
      error = function(e) NULL
    )
    ok <- !is.null(txt) && !inherits(try(parse_json(txt), silent = TRUE), "try-error")
    if (ok) {
      con <- gzfile(path, "w"); writeLines(txt, con); close(con)
      return(invisible("fetched"))
    }
    Sys.sleep(5 * attempt)
  }
  
  warning("Failed after ", retries, " tries: game ", pk, call. = FALSE)
  invisible(NA)
}

fetch_all_challenges <- function() {
  dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
  pks <- read_parquet(SEASON_FILE, col_select = "game_pk")$game_pk |> unique() |> sort()
  
  for (i in seq_along(pks)) {
    status <- fetch_game(pks[i])
    if (i %% 50 == 0 || i == length(pks)) {
      message(sprintf("[%4d/%d] game %s", i, length(pks), pks[i]))
    }
    if (identical(status, "fetched")) Sys.sleep(0.3)
  }
}

# ---- Parse (offline) --------------------------------------------------------

read_feed <- function(pk) {
  fromJSON(paste(readLines(gzfile(raw_path(pk)), warn = FALSE), collapse = ""),
           simplifyVector = FALSE)
}

empty_challenges <- tibble(
  game_pk = integer(), at_bat_number = integer(), pitch_number = integer(),
  inning = integer(), is_top_inning = logical(), overturned = logical(),
  challenge_team_id = integer(), challenger_id = integer(),
  final_code = character(), review_level = character()
)

# One challenge -> one row. `ev` is the pitch event the challenge applies to;
# `rd` is the reviewDetails object (from the pitch OR the play).
review_row <- function(pk, play, ev, rd, level) {
  tibble(
    game_pk           = as.integer(pk),
    at_bat_number     = as.integer(play[["about"]][["atBatIndex"]] + 1),
    pitch_number      = as.integer(ev[["pitchNumber"]] %||% NA),
    inning            = as.integer(play[["about"]][["inning"]] %||% NA),
    is_top_inning     = play[["about"]][["isTopInning"]] %||% NA,
    overturned        = rd[["isOverturned"]] %||% NA,
    challenge_team_id = as.integer(rd[["challengeTeamId"]] %||% NA),
    challenger_id     = as.integer(rd[["player"]][["id"]] %||% NA),
    final_code        = as.character(ev[["details"]][["code"]] %||% NA),
    review_level      = level
  )
}

is_abs <- function(rd) identical(rd[["reviewType"]], "MJ")

parse_feed <- function(feed, pk) {
  plays <- feed[["liveData"]][["plays"]][["allPlays"]] %||% list()
  
  rows <- plays |>
    map(\(p) {
      evs <- p[["playEvents"]] %||% list()
      
      # 1. Challenges attached to a specific pitch
      pitch_rows <- evs |>
        keep(\(e) is_abs(e[["reviewDetails"]])) |>
        map(\(e) review_row(pk, p, e, e[["reviewDetails"]], "pitch"))
      
      # 2. Challenge attached to the play -> applies to the at-bat's last pitch
      play_rows <- list()
      if (is_abs(p[["reviewDetails"]])) {
        pitches <- keep(evs, \(e) isTRUE(e[["isPitch"]]))
        if (length(pitches) > 0) {
          play_rows <- list(review_row(pk, p, pitches[[length(pitches)]],
                                       p[["reviewDetails"]], "play"))
        }
      }
      c(pitch_rows, play_rows)
    }) |>
    list_flatten()
  
  challenges <- if (length(rows) == 0) empty_challenges else
    list_rbind(rows) |>
    # If the same review shows up at both levels, keep the pitch-level copy.
    distinct(game_pk, at_bat_number, pitch_number, challenge_team_id,
             .keep_all = TRUE)
  
  abs   <- feed[["gameData"]][["absChallenges"]]
  teams <- feed[["gameData"]][["teams"]]
  summary <- tibble(
    game_pk        = as.integer(pk),
    has_challenges = abs[["hasChallenges"]] %||% NA,
    home_team_id   = as.integer(teams[["home"]][["id"]] %||% NA),
    away_team_id   = as.integer(teams[["away"]][["id"]] %||% NA),
    home_success   = as.integer(abs[["home"]][["usedSuccessful"]] %||% NA),
    home_failed    = as.integer(abs[["home"]][["usedFailed"]] %||% NA),
    away_success   = as.integer(abs[["away"]][["usedSuccessful"]] %||% NA),
    away_failed    = as.integer(abs[["away"]][["usedFailed"]] %||% NA)
  )
  
  list(challenges = challenges, summary = summary)
}

combine_challenges <- function() {
  pks <- list.files(RAW_DIR, pattern = "\\.json\\.gz$") |>
    sub(pattern = "\\.json\\.gz$", replacement = "")
  
  parts <- imap(pks, \(pk, i) {
    if (i %% 250 == 0) message(sprintf("parsed %d/%d", i, length(pks)))
    parse_feed(read_feed(pk), pk)
  })
  
  challenges <- map(parts, "challenges") |> list_rbind()
  summary    <- map(parts, "summary")    |> list_rbind()
  
  # Validation: per-team won/lost from individual challenges must equal the
  # game's official absChallenges totals.
  official <- bind_rows(
    summary |> transmute(game_pk, team_id = home_team_id,
                         won_official = home_success, lost_official = home_failed),
    summary |> transmute(game_pk, team_id = away_team_id,
                         won_official = away_success, lost_official = away_failed)
  )
  ours <- challenges |>
    group_by(game_pk, team_id = challenge_team_id) |>
    summarise(won = sum(overturned, na.rm = TRUE),
              lost = sum(!overturned, na.rm = TRUE), .groups = "drop")
  
  check <- official |>
    left_join(ours, by = c("game_pk", "team_id")) |>
    mutate(across(c(won, lost), \(v) replace_na(v, 0L))) |>
    filter(won != won_official | lost != lost_official)
  
  write_parquet(challenges, OUT_CHAL)
  write_parquet(summary, OUT_SUMM)
  
  message(sprintf(
    "Saved %s challenges from %d games | overturn rate %.1f%% | %d team-games mismatched",
    format(nrow(challenges), big.mark = ","), nrow(summary),
    100 * mean(challenges$overturned, na.rm = TRUE), nrow(check)
  ))
  print(count(challenges, review_level))
  invisible(list(challenges = challenges, summary = summary, mismatches = check))
}