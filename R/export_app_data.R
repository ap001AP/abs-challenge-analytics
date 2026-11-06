# R/export_app_data.R
# Copy the small, app-ready slices of the analysis into app/app_data/.
#
# Why a separate export step:
#   - data/ is gitignored and huge; the app only needs a few thousand rows.
#   - The app must be self-contained to deploy (shinyapps.io bundles app/ only).
#   - Saved as .rds, not parquet, so the deployed app doesn't need {arrow},
#     which is slow and fragile to build on a deploy server.

library(dplyr)
library(arrow)

APP_DATA <- file.path("app", "app_data")

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
  
  saveRDS(grades,    file.path(APP_DATA, "grades.rds"))
  saveRDS(params,    file.path(APP_DATA, "params.rds"))
  saveRDS(decisions, file.path(APP_DATA, "decisions.rds"))
  
  sizes <- file.size(list.files(APP_DATA, full.names = TRUE)) / 1024
  message(sprintf("Exported %d players, %s decisions -> %s (%.0f KB total)",
                  nrow(grades), format(nrow(decisions), big.mark = ","),
                  APP_DATA, sum(sizes)))
}