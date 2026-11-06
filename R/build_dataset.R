# R/build_dataset.R
# Join Statcast pitches + ABS challenges into one analysis table of TAKES
# (called pitches), one row per pitch, with:
#   - the ABS "true" call from zone geometry, and how far the ball missed the edge
#   - the umpire's ORIGINAL call (Statcast stores the post-challenge call)
#   - challenges remaining for each team before the pitch
#   - challenge opportunity flags (could this side have challenged?)
#   - overturn value in runs, and the breakeven confidence needed to challenge
#
# Output: data/takes_2026.parquet

library(dplyr)
library(tidyr)
library(arrow)

source("R/abs_zone.R")
source("R/challenge_state.R")

SEASON_FILE <- file.path("data", "statcast_2026.parquet")
CHAL_FILE   <- file.path("data", "challenges_2026.parquet")
SUMM_FILE   <- file.path("data", "challenge_summary_2026.parquet")
OUT_FILE    <- file.path("data", "takes_2026.parquet")

# Value of a challenge in hand, in runs. Losing one is the cost of a failed
# challenge. 0.2 is the figure Savant's ABS documentation uses:
#   breakeven confidence = 0.2 / (0.2 + run value of the overturn)
CHALLENGE_VALUE_RUNS <- 0.2

# Minimum takes per (count, outs, bases, call) cell before trusting its
# run-value estimate; sparser cells fall back to (count, outs) only.
MIN_CELL <- 20L

# Statcast descriptions that are called pitches (no swing), and their call.
# Excludes automatic_ball / automatic_strike (pitch-clock violations), which
# can't be challenged.
TAKE_CODES <- c(called_strike = "S", ball = "B", blocked_ball = "B")

# ---- Run value of an overturn -----------------------------------------------
# For each game state, the run value of a ball vs a strike is estimated from
# Statcast's delta_run_exp (change in batting-team run expectancy on that
# pitch, including walks/strikeouts). Overturn value = E[ball] - E[strike].

re_table <- function(takes, keys) {
  takes |>
    filter(!is.na(delta_run_exp)) |>
    group_by(across(all_of(c(keys, "final_call")))) |>
    summarise(re = mean(delta_run_exp), n = n(), .groups = "drop") |>
    pivot_wider(names_from = final_call, values_from = c(re, n),
                values_fill = list(n = 0L))
}

add_overturn_value <- function(takes) {
  state_fine   <- c("balls", "strikes", "outs_when_up", "bases")
  state_coarse <- c("balls", "strikes", "outs_when_up")
  
  fine <- re_table(takes, state_fine) |>
    rename(re_B_f = re_B, re_S_f = re_S, n_B_f = n_B, n_S_f = n_S)
  coarse <- re_table(takes, state_coarse) |>
    transmute(across(all_of(state_coarse)), re_B_c = re_B, re_S_c = re_S)
  
  takes |>
    left_join(fine,   by = state_fine) |>
    left_join(coarse, by = state_coarse) |>
    mutate(
      use_fine = coalesce(n_B_f, 0L) >= MIN_CELL & coalesce(n_S_f, 0L) >= MIN_CELL,
      overturn_value = if_else(use_fine, re_B_f - re_S_f, re_B_c - re_S_c),
      breakeven_conf = CHALLENGE_VALUE_RUNS / (CHALLENGE_VALUE_RUNS + overturn_value)
    ) |>
    select(-c(re_B_f, re_S_f, n_B_f, n_S_f, re_B_c, re_S_c, use_fine))
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
  
  # 1. Every pitch, with challenges attached and challenge state tracked.
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
  
  # 2. Takes only, with calls, geometry and opportunity flags.
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
    add_overturn_value() |>
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
  validate_dataset(takes, chal)
  invisible(takes)
}

# ---- Validation -------------------------------------------------------------
# Each line should be close to 100% / 0. Anything else is a bug to chase.

validate_dataset <- function(takes, chal) {
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
  
  cat("\nMean overturn value (runs) by count:\n")
  takes |>
    group_by(balls, strikes) |>
    summarise(v = round(mean(overturn_value, na.rm = TRUE), 3), .groups = "drop") |>
    pivot_wider(names_from = strikes, values_from = v, names_prefix = "strikes_") |>
    print()
}