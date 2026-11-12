# R/fetch_statcast.R
# Pull every 2026 regular-season pitch from Baseball Savant, one day at a time,
# and cache to parquet.

library(baseballr)
library(dplyr)
library(purrr)
library(arrow)

# Buffered on both ends on purpose: spring training and postseason days get
# filtered out by game_type == "R" in combine_season().
SEASON_START <- as.Date("2026-03-20")
SEASON_END   <- as.Date("2026-10-01")

RAW_DIR  <- file.path("data", "raw", "statcast")
OUT_FILE <- file.path("data", "statcast_2026.parquet")

CHR_COLS <- c("game_date", "game_type", "player_name", "stand", "p_throws",
              "home_team", "away_team", "inning_topbot", "description", "type",
              "pitch_type", "events", "bb_type")
NUM_COLS <- c(
  # keys
  "game_pk", "at_bat_number", "pitch_number", "batter", "pitcher", "fielder_2",
  # game state (for run values / leverage)
  "inning", "outs_when_up", "balls", "strikes", "on_1b", "on_2b", "on_3b",
  "bat_score", "fld_score", "home_score", "away_score",
  "delta_run_exp", "delta_home_win_exp",
  # location + ABS zone
  "plate_x", "plate_z", "sz_top", "sz_bot", "zone",
  # pitch physics
  "release_speed", "release_spin_rate", "pfx_x", "pfx_z",
  # batted ball (for the "Robbed" bonus tab)
  "launch_speed", "launch_angle", "hit_distance_sc", "hc_x", "hc_y",
  "estimated_ba_using_speedangle"
)

normalise_types <- function(df) {
  df |>
    select(any_of(c(CHR_COLS, NUM_COLS))) |>
    mutate(
      across(any_of(CHR_COLS), as.character),
      across(any_of(NUM_COLS), as.numeric)
    )
}

fetch_day <- function(day, retries = 3) {
  path <- file.path(RAW_DIR, paste0(day, ".parquet"))
  if (file.exists(path)) return(invisible("cached"))
  
  for (attempt in seq_len(retries)) {
    res <- tryCatch(
      suppressMessages(
        statcast_search(start_date = day, end_date = day, player_type = "batter")
      ),
      error = function(e) e
    )
    
    if (!inherits(res, "error")) {
      df <- normalise_types(as_tibble(res))
      write_parquet(df, path)   # 0-row days (All-Star break) get cached too
      return(invisible(nrow(df)))
    }
    Sys.sleep(5 * attempt)      # back off: 5s, 10s, 15s
  }
  
  warning("Failed after ", retries, " tries: ", day, call. = FALSE)
  invisible(NA)
}

fetch_season <- function(start = SEASON_START, end = SEASON_END) {
  dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
  days <- as.character(seq(start, end, by = "day"))
  
  for (i in seq_along(days)) {
    n <- fetch_day(days[i])
    message(sprintf("[%3d/%d] %s  %s", i, length(days), days[i], n))
    if (!identical(n, "cached")) Sys.sleep(1)   # be polite to Savant
  }
}

combine_season <- function() {
  files <- list.files(RAW_DIR, pattern = "\\.parquet$", full.names = TRUE)
  
  season <- files |>
    map(read_parquet) |>
    list_rbind() |>
    filter(game_type == "R") |>
    mutate(game_date = as.Date(game_date)) |>
    distinct(game_pk, at_bat_number, pitch_number, .keep_all = TRUE) |>
    arrange(game_date, game_pk, at_bat_number, pitch_number)
  
  write_parquet(season, OUT_FILE)
  message(sprintf("Saved %s pitches from %s games -> %s",
                  format(nrow(season), big.mark = ","),
                  n_distinct(season$game_pk), OUT_FILE))
  invisible(season)
}