# R/export_app_data.R
# Copy the small, app-ready slices of the analysis into app/app_data/.
#
# Why a separate export step:
#   - data/ is gitignored and huge; the app only needs a few thousand rows.
#   - The app must be self-contained to deploy (shinyapps.io bundles app/ only).
#   - Saved as .rds, not parquet, so the deployed app doesn't need {arrow},
#     which is slow and fragile to build on a deploy server.

library(dplyr)
library(purrr)
library(arrow)

# By default {arrow} returns ALTREP columns: R vectors still backed by Arrow
# memory. saveRDS() then writes them AS Arrow objects, and a machine without
# {arrow} (the deploy server) reads every column back as length 0.
# Force plain R vectors so the .rds files are self-contained.
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

# ---- Export -----------------------------------------------------------------

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
  
  # Shiny auto-loads app/R/*.R, so ship the SAME zone-geometry code the
  # analysis uses (one source of truth, already unit-tested).
  dir.create(file.path("app", "R"), showWarnings = FALSE)
  file.copy(file.path("R", "abs_zone.R"), file.path("app", "R", "abs_zone.R"),
            overwrite = TRUE)
  
  # League-wide headline numbers (tiny; keeps the app free of the big tables)
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
  
  # Prove the files load WITHOUT arrow, the way the deploy server will.
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