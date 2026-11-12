# R/build_dataset.R
# Join Statcast pitches + ABS challenges into one analysis table of TAKES
# (called pitches), one row per pitch, with:
#   - the ABS "true" call from zone geometry, and how far the ball missed the edge
#   - the umpire's ORIGINAL call (Statcast stores the post-challenge call)
#   - challenges remaining for each team before the pitch
#   - challenge opportunity flags (could this side have challenged?)
#   - overturn value in runs, and the breakeven confidence needed to challenge

library(dplyr)
library(tidyr)
library(arrow)

source("R/abs_zone.R")
source("R/challenge_state.R")
source("R/run_expectancy.R")

SEASON_FILE <- file.path("data", "statcast_2026.parquet")
CHAL_FILE   <- file.path("data", "challenges_2026.parquet")
SUMM_FILE   <- file.path("data", "challenge_summary_2026.parquet")
OUT_FILE    <- file.path("data", "takes_2026.parquet")

# Value of a challenge in hand, in runs. Losing one is the cost of a failed
# challenge. 0.2 is the figure Savant's ABS documentation uses:
#   breakeven confidence = 0.2 / (0.2 + run value of the overturn)
CHALLENGE_VALUE_RUNS <- 0.2

RE_FILE <- file.path("data", "re288_2026.parquet")
OV_FILE <- file.path("data", "overturn_values_2026.parquet")

# Statcast descriptions that are called pitches (no swing), and their call.
# Excludes automatic_ball / automatic_strike (pitch-clock violations), which
# can't be challenged.
TAKE_CODES <- c(called_strike = "S", ball = "B", blocked_ball = "B")

# ---- Run value of an overturn -----------------------------------------------
# From our own RE288 table (R/run_expectancy.R), NOT Statcast's delta_run_exp,
# which is nearly context-free at the end of a plate appearance and valued a
# bases-loaded full count like a bases-empty one.
add_overturn_value <- function(takes, ov_table) {
  takes |>
    left_join(ov_table |> select(balls, strikes, outs_when_up, bases, overturn_value),
              by = c("balls", "strikes", "outs_when_up", "bases")) |>
    mutate(breakeven_conf = CHALLENGE_VALUE_RUNS / (CHALLENGE_VALUE_RUNS + overturn_value))
}

# ---- Main -------------------------------------------------------------------

build_dataset <- function() {
  keys <- c("game_pk", "at_bat_number", "pitch_number")
  
  season <- read_parquet(SEASON_FILE) |>
    mutate(across(all_of(keys), as.integer))
  chal <- read_parquet(CHAL_FILE) |>
    select(all_of(keys), challenge_team_id, challenger_id, overturned)
  summ <- read_parquet(SUMM_FILE) |>
    select(game_pk, home_team_id, away_team_id)
  
  # Every pitch, with challenges attached and challenge state tracked.
  #    (All pitches, not just takes, so innings/order are complete.)
  pitches <- season |>
    stable_zone() |>
    left_join(summ, by = "game_pk") |>
    left_join(chal, by = keys) |>
    mutate(
      challenged       = !is.na(challenge_team_id),
      bases            = paste0(as.integer(!is.na(on_1b)),
                                as.integer(!is.na(on_2b)),
                                as.integer(!is.na(on_3b))),
      batting_team_id  = if_else(inning_topbot == "Top", away_team_id, home_team_id),
      fielding_team_id = if_else(inning_topbot == "Top", home_team_id, away_team_id)
    ) |>
    arrange(game_pk, at_bat_number, pitch_number) |>
    group_by(game_pk) |>
    mutate(
      home_rem = remaining_before(inning, challenge_team_id, overturned, first(home_team_id)),
      away_rem = remaining_before(inning, challenge_team_id, overturned, first(away_team_id))
    ) |>
    ungroup() |>
    mutate(
      bat_rem = if_else(inning_topbot == "Top", away_rem, home_rem),
      fld_rem = if_else(inning_topbot == "Top", home_rem, away_rem)
    )
  
  # Run expectancy from every pitch of the season -> value of each flip
  re_data  <- runs_to_inning_end(pitches)
  re288    <- compute_re288(re_data)
  ov_table <- overturn_values_from_re(re288)
  write_parquet(re288, RE_FILE)
  write_parquet(ov_table, OV_FILE)
  
  # Takes only, with calls, geometry and opportunity flags.
  takes <- pitches |>
    filter(description %in% names(TAKE_CODES)) |>
    mutate(
      final_call    = unname(TAKE_CODES[description]),
      original_call = if_else(challenged & coalesce(overturned, FALSE),
                              if_else(final_call == "S", "B", "S"),
                              final_call),
      edge_dist_in  = edge_distance_in(plate_x, plate_z, sz_top_abs, sz_bot_abs),
      true_call     = abs_call(plate_x, plate_z, sz_top_abs, sz_bot_abs),
      missed_call   = original_call != true_call,
      
      # Who the original call went against, and could they challenge?
      side_against  = if_else(original_call == "S", "batting", "fielding"),
      rem_for_side  = if_else(side_against == "batting", bat_rem, fld_rem),
      opportunity   = rem_for_side > 0,
      winnable_opp  = opportunity & missed_call,   # a challenge here would win
      
      challenge_side = case_when(
        !challenged                            ~ NA_character_,
        challenge_team_id == batting_team_id   ~ "batting",
        challenge_team_id == fielding_team_id  ~ "fielding",
        TRUE                                   ~ "unknown"
      ),
      challenger_role = case_when(
        !challenged                  ~ NA_character_,
        challenger_id == batter      ~ "batter",
        challenger_id == pitcher     ~ "pitcher",
        challenger_id == fielder_2   ~ "catcher",
        TRUE                         ~ "other"
      )
    ) |>
    add_overturn_value(ov_table) |>
    select(
      all_of(keys), game_date, home_team, away_team, inning, inning_topbot,
      batter, pitcher, catcher = fielder_2, player_name, stand, p_throws,
      balls, strikes, outs_when_up, bases, bat_score, fld_score,
      pitch_type, release_speed, plate_x, plate_z,
      sz_top = sz_top_abs, sz_bot = sz_bot_abs, edge_dist_in,
      final_call, original_call, true_call, missed_call,
      batting_team_id, fielding_team_id, bat_rem, fld_rem,
      side_against, rem_for_side, opportunity, winnable_opp,
      challenged, overturned, challenge_team_id, challenger_id,
      challenger_role, challenge_side,
      overturn_value, breakeven_conf, delta_run_exp
    )
  
  write_parquet(takes, OUT_FILE)
  validate_dataset(takes, chal, re288, ov_table, re_data)
  invisible(takes)
}

# ---- Validation -------------------------------------------------------------
# Each line should be close to 100% / 0. Anything else is a bug to chase.

validate_dataset <- function(takes, chal, re288, ov_table, re_data) {
  ch  <- filter(takes, challenged)
  pct <- \(x) sprintf("%.2f%%", 100 * mean(x, na.rm = TRUE))
  
  cat("\n== build_dataset validation ==\n")
  cat("Takes:                                  ", format(nrow(takes), big.mark = ","), "\n")
  cat("Challenges matched to a take:           ", nrow(ch), "/", nrow(chal), "\n")
  cat("Geometry agrees w/ ABS on challenges:   ", pct(ch$true_call == ch$final_call), "\n")
  cat("Challenges by the side the call hurt:   ", pct(ch$challenge_side == ch$side_against), "\n")
  cat("Challenges made with 0 remaining:       ", sum(ch$rem_for_side == 0, na.rm = TRUE), "\n")
  cat("Umpire accuracy on all takes:           ", pct(!takes$missed_call), "\n")
  cat("Challenge opportunities:                ", format(sum(takes$opportunity, na.rm = TRUE), big.mark = ","), "\n")
  cat("  ...of which winnable (missed calls):  ", format(sum(takes$winnable_opp, na.rm = TRUE), big.mark = ","), "\n")
  cat("Challenger roles:\n"); print(count(ch, challenger_role))
  
  cat("\n== Run expectancy ==\n")
  cat("Pitches used:", format(nrow(re_data), big.mark = ","), "\n")
  cat("RE24 (runs to end of inning at a 0-0 count). Typical MLB: empty ~0.5/0.27/0.1,",
      "loaded ~2.3/1.5/0.75\n")
  re288 |>
    filter(balls == 0, strikes == 0) |>
    mutate(re = round(re, 2)) |>
    select(bases, outs_when_up, re) |>
    pivot_wider(names_from = outs_when_up, values_from = re, names_prefix = "outs_") |>
    print()
  cat("Overturn value range:", sprintf("%.3f to %.3f", min(ov_table$overturn_value),
                                       max(ov_table$overturn_value)),
      "(must be >= 0)\n")
  adj <- ov_table |> mutate(change = overturn_value - raw_value)
  cat("Monotone constraint: raw negatives", sum(adj$raw_value < 0), "of", nrow(adj),
      "| states moved > 0.02 runs:", sum(abs(adj$change) > 0.02),
      "| largest move:", sprintf("%.3f", max(abs(adj$change))), "\n")
  cat("Check: 3-2 values (walk vs strikeout):\n")
  ov_table |>
    filter(balls == 3, strikes == 2) |>
    mutate(overturn_value = round(overturn_value, 2)) |>
    select(bases, outs_when_up, overturn_value) |>
    pivot_wider(names_from = outs_when_up, values_from = overturn_value, names_prefix = "outs_") |>
    print()
  cat("\nMean overturn value (runs) by count:\n")
  takes |>
    group_by(balls, strikes) |>
    summarise(v = round(mean(overturn_value, na.rm = TRUE), 3), .groups = "drop") |>
    pivot_wider(names_from = strikes, values_from = v, names_prefix = "strikes_") |>
    print()
}