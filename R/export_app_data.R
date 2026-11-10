# R/export_app_data.R

library(dplyr)
library(purrr)
library(arrow)

options(arrow.use_altrep = FALSE)

APP_DATA <- file.path("app", "app_data")
GROUP_LEVELS <- c("Catchers, catching", "Catchers, batting", "Other batters")

# ---- "Who Sees the Zone" ----------------------------------------------------
# Same read-noise probit as model.R, fitted per GROUP rather than per role.
# The key comparison: catchers BEHIND the plate vs the same catchers AT it.
# sigma = 1 / slope on win_dist_in; 95% CI via the slope's standard error.

fit_sigma <- function(d) {
  fit <- glm(challenged_here ~ win_dist_in + z_breakeven,
             family = binomial(link = "probit"), data = d)
  est <- summary(fit)$coefficients["win_dist_in", ]
  b <- est[["Estimate"]]; se <- est[["Std. Error"]]
  tibble(
    sigma_in = 1 / b,
    sigma_lo = 1 / (b + 1.96 * se),
    sigma_hi = 1 / (b - 1.96 * se),
    n_opps   = nrow(d),
    n_chal   = sum(d$challenged_here),
    n_players = n_distinct(d$decider_id)
  )
}

add_groups <- function(opps, grades) {
  catcher_ids <- grades |>
    filter(role == "catcher", opps >= 100) |>
    pull(decider_id)
  
  opps |>
    mutate(
      z_breakeven = qnorm(pmin(pmax(breakeven_conf, 0.01), 0.99)),
      group = case_when(
        role == "catcher"            ~ "Catchers, catching",
        decider_id %in% catcher_ids  ~ "Catchers, batting",
        TRUE                         ~ "Other batters"
      ),
      group = factor(group, levels = GROUP_LEVELS)
    )
}

# Observed behaviour: % of opportunities challenged, by how wrong the call was.
# Raw data, no model: the steeper the curve at 0, the sharper the read.
read_curves <- function(opps_g) {
  opps_g |>
    filter(between(win_dist_in, -6, 6)) |>
    mutate(bin = round(win_dist_in * 2) / 2) |>           # half-inch bins
    group_by(group, bin) |>
    summarise(n = n(), challenge_rate = mean(challenged_here), .groups = "drop") |>
    filter(n >= 40)
}

# ---- "Robbed" helpers -------------------------------------------------------
# In-play outs (sac flies count: the batter still lost a likely hit) and hits.
# Reached on error counts as a non-hit for the luck table, as in batting average.
OUT_EVENTS <- c("field_out", "force_out", "grounded_into_double_play", "double_play",
                "fielders_choice_out", "triple_play", "sac_fly", "sac_fly_double_play")
HIT_EVENTS <- c("single", "double", "triple", "home_run")

# Statcast's "Guerrero Jr., Vladimir" -> "Vladimir Guerrero Jr."
clean_name <- function(x) {
  parts <- strsplit(x, ", ", fixed = TRUE)
  vapply(parts, \(p) if (length(p) == 2) paste(p[2], p[1]) else p[1], character(1))
}

event_label <- function(e, bb_type = NA_character_) {
  # A plain field_out is named by how the ball was hit
  by_type <- c(line_drive = "Lineout", fly_ball = "Flyout",
               ground_ball = "Groundout", popup = "Popout")
  labels <- c(field_out = "Out", force_out = "Force out",
              grounded_into_double_play = "Double play", double_play = "Double play",
              fielders_choice_out = "Fielder's choice", triple_play = "Triple play",
              sac_fly = "Sac fly", sac_fly_double_play = "Sac fly DP")
  out <- unname(labels[e])
  typed <- e == "field_out" & bb_type %in% names(by_type)
  out[typed] <- unname(by_type[bb_type[typed]])
  out
}

# Ballpark outlines from GeomMLBStadiums (Ben Dilday, MIT licence)
STADIUM_CODES <- c(
  angels = "LAA", astros = "HOU", athletics = "ATH", blue_jays = "TOR", braves = "ATL",
  brewers = "MIL", cardinals = "STL", cubs = "CHC", diamondbacks = "AZ", dodgers = "LAD",
  giants = "SF", indians = "CLE", guardians = "CLE", mariners = "SEA", marlins = "MIA",
  mets = "NYM", nationals = "WSH", orioles = "BAL", padres = "SD", phillies = "PHI",
  pirates = "PIT", rangers = "TEX", rays = "TB", red_sox = "BOS", reds = "CIN",
  rockies = "COL", royals = "KC", tigers = "DET", twins = "MIN", white_sox = "CWS",
  yankees = "NYY"
)

stadium_paths <- function() {
  empty <- tibble(park = character(), segment = character(),
                  x = numeric(), y = numeric(), order = integer())
  if (!requireNamespace("GeomMLBStadiums", quietly = TRUE)) {
    message("GeomMLBStadiums not installed: every park uses the generic field.")
    return(empty)
  }

  d <- tryCatch(getExportedValue("GeomMLBStadiums", "MLBStadiumsPathData"),
                error = function(e) NULL)
  if (is.null(d)) {
    e <- new.env()
    suppressWarnings(utils::data(list = c("MLBStadiaPathData", "MLBStadiumsPathData"),
                                 package = "GeomMLBStadiums", envir = e))
    d <- e$MLBStadiumsPathData
  }
  if (is.null(d) || !all(c("team", "segment", "x", "y") %in% names(d))) {
    message("Couldn't read GeomMLBStadiums' path data: every park uses the generic field.")
    return(empty)
  }
  as_tibble(d) |>
    mutate(park = unname(STADIUM_CODES[team]), order = row_number()) |>
    filter(!is.na(park)) |>
    transmute(park, segment, x = as.numeric(x), y = as.numeric(y), order)
}

export_app_data <- function() {
  dir.create(APP_DATA, recursive = TRUE, showWarnings = FALSE)
  
  grades <- read_parquet(file.path("data", "player_grades_2026.parquet"))
  params <- read_parquet(file.path("data", "model_params_2026.parquet"))
  opps   <- read_parquet(file.path("data", "opportunities_scored_2026.parquet"))
  
  # Only the pitches a player-view needs: every challenge, plus every spot
  # where he SHOULD have challenged but didn't.
  decisions <- opps |>
    filter(challenged_here | should_challenge) |>
    mutate(
      outcome = case_when(
        won_here        ~ "Won",
        challenged_here ~ "Lost",
        TRUE            ~ "Missed"
      ),
      # Every batter has his own zone height. Rescale vertical location onto
      # one standard 1.5-3.5 ft zone so pitches vs different batters overlay.
      z_std = 1.5 + 2 * (plate_z - sz_bot) / (sz_top - sz_bot)
    ) |>
    mutate(pitch_id = row_number()) |>
    select(pitch_id, role, decider_id, game_date, balls, strikes, outs_when_up,
           bases, original_call, true_call, plate_x, z_std, p_win, ev,
           overturn_value, outcome)
  
  opps_g <- add_groups(opps, grades)
  sigma_groups <- opps_g |>
    group_split(group) |>
    map(\(d) fit_sigma(d) |> mutate(group = d$group[1], .before = 1)) |>
    list_rbind()
  curves <- read_curves(opps_g)
  
  # "Should He Have Tapped?": run value of an overturn for every game state
  # The full 288-state table from our RE288 (build_dataset.R), so every state
  # in the calculator has a value, including ones with no opportunities.
  overturn_values <- read_parquet(file.path("data", "overturn_values_2026.parquet")) |>
    select(balls, strikes, outs_when_up, bases, overturn_value)
  saveRDS(overturn_values, file.path(APP_DATA, "overturn_values.rds"))
  
  dir.create(file.path("app", "R"), showWarnings = FALSE)
  file.copy(file.path("R", "abs_zone.R"), file.path("app", "R", "abs_zone.R"),
            overwrite = TRUE)
  
  # "Robbed": batted balls that should have been hits but were caught.
  # xBA (estimated_ba_using_speedangle) = how often a ball with this exit
  # velocity + launch angle falls for a hit, league-wide.
  bbe <- read_parquet(file.path("data", "statcast_2026.parquet"),
                      col_select = c("game_date", "batter", "player_name", "inning_topbot",
                                     "home_team", "away_team", "events", "bb_type",
                                     "launch_speed", "launch_angle", "hit_distance_sc",
                                     "hc_x", "hc_y", "estimated_ba_using_speedangle")) |>
    filter(!is.na(launch_speed), !is.na(estimated_ba_using_speedangle),
           events %in% c(OUT_EVENTS, HIT_EVENTS, "field_error")) |>
    mutate(
      name = clean_name(player_name),
      team = if_else(inning_topbot == "Top", away_team, home_team),
      opp  = if_else(inning_topbot == "Top", home_team, away_team),
      xba  = estimated_ba_using_speedangle,
      hit  = events %in% HIT_EVENTS
    )
  
  robbed <- bbe |>
    filter(events %in% OUT_EVENTS, xba >= 0.5) |>
    transmute(game_date = as.Date(game_date), batter, name, team, opp, park = home_team,
              ev = launch_speed, la = launch_angle, dist = hit_distance_sc,
              hc_x, hc_y, xba, result = event_label(events, bb_type), bb_type)
  
  luck <- bbe |>
    group_by(batter) |>
    summarise(name = first(name),
              team = names(which.max(table(team))),
              bbe  = n(), hits = sum(hit), x_hits = sum(xba), .groups = "drop") |>
    filter(bbe >= 200) |>
    mutate(luck = hits - x_hits, bacon = hits / bbe, xbacon = x_hits / bbe)
  
  park_bbe <- bbe |> count(park = home_team, name = "bbe")
  stadiums <- stadium_paths()
  missing  <- setdiff(park_bbe$park, stadiums$park)
  message(sprintf("Ballpark outlines: %d of %d 2026 parks%s",
                  length(intersect(park_bbe$park, stadiums$park)), nrow(park_bbe),
                  if (length(missing)) paste0(" (generic field for: ", paste(missing, collapse = ", "), ")") else ""))
  
  saveRDS(robbed,   file.path(APP_DATA, "robbed.rds"))
  saveRDS(luck,     file.path(APP_DATA, "luck.rds"))
  saveRDS(park_bbe, file.path(APP_DATA, "park_bbe.rds"))
  saveRDS(stadiums, file.path(APP_DATA, "stadiums.rds"))
  
  takes <- read_parquet(file.path("data", "takes_2026.parquet"),
                        col_select = c("game_pk", "missed_call"))
  chal  <- read_parquet(file.path("data", "challenges_2026.parquet"))
  league <- list(
    n_pitches     = nrow(read_parquet(file.path("data", "statcast_2026.parquet"),
                                      col_select = "game_pk")),
    n_games       = n_distinct(takes$game_pk),
    n_takes       = nrow(takes),
    ump_accuracy  = mean(!takes$missed_call, na.rm = TRUE),
    n_challenges  = nrow(chal),
    overturn_rate = mean(chal$overturned, na.rm = TRUE)
  )
  saveRDS(league, file.path(APP_DATA, "league.rds"))
  
  saveRDS(grades,       file.path(APP_DATA, "grades.rds"))
  saveRDS(params,       file.path(APP_DATA, "params.rds"))
  saveRDS(decisions,    file.path(APP_DATA, "decisions.rds"))
  saveRDS(sigma_groups, file.path(APP_DATA, "sigma_groups.rds"))
  saveRDS(curves,       file.path(APP_DATA, "read_curves.rds"))
  
  print(sigma_groups |> mutate(across(where(is.double), \(x) round(x, 2))))

  check <- callr::r(function(dir) {
    files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
    vapply(files, function(f) {
      x <- readRDS(f)
      if (is.data.frame(x)) all(lengths(x) == nrow(x)) else TRUE
    }, logical(1))
  }, args = list(dir = APP_DATA),
  libpath = .Library)   # base R only: no {arrow}, just like the server
  if (!all(check)) stop("Not self-contained: ", paste(basename(names(check)[!check]), collapse = ", "))
  message("✓ All app data loads cleanly without {arrow}")
  
  sizes <- file.size(list.files(APP_DATA, full.names = TRUE)) / 1024
  message(sprintf("Exported %d players, %s decisions -> %s (%.0f KB total)",
                  nrow(grades), format(nrow(decisions), big.mark = ","),
                  APP_DATA, sum(sizes)))
}