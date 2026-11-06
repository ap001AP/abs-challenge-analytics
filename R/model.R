# R/model.R
# "Should he have challenged?" — score every challenge opportunity and grade
# every batter and catcher.
#
# The catch: ABS is deterministic. Given the exact pitch location, whether a
# challenge wins is a yes/no fact, so a model trained on exact edge distance
# would just learn that fact. The real question is what a player AT THE PLATE
# should have believed, and players can't see the location to the millimetre.
#
# Approach (perception-noise model):
#   1. win_dist_in = how far the pitch was on the "winning" side of the zone
#      edge (inches). > 0 means the umpire missed it and a challenge would win.
#   2. A player perceives win_dist with noise of sigma inches. A calibrated
#      player's belief that he'd win is then  P(win) = pnorm(win_dist / sigma).
#   3. sigma is ESTIMATED from behaviour: a probit of "did they challenge?" on
#      win_dist (and the stakes). The slope on win_dist is 1 / sigma.
#      Fitted separately for batters and catchers: who reads the zone better?
#   4. EV of challenging = P(win) * overturn_value - (1 - P(win)) * 0.2 runs.
#      Should challenge  <=>  EV > 0  <=>  P(win) > breakeven confidence.
#
# Grading is RELATIVE to league behaviour on the same pitches, so a player
# who sees lots of borderline 3-2 pitches isn't flattered or punished for it:
#   aggression  = actual challenges / challenges a league-average player
#                 would make on the same opportunities (> 1 = more aggressive)
#   selectivity = mean EV of his challenges - mean EV a league-average player
#                 would get from his opportunities (> 0 = picks better spots)
#
#   Brave    = aggressive + selective     Reckless = aggressive + unselective
#   Chicken  = passive    + selective     Lost     = passive    + unselective

library(dplyr)
library(tidyr)
library(purrr)
library(arrow)
library(jsonlite)

TAKES_FILE   <- file.path("data", "takes_2026.parquet")
OPPS_OUT     <- file.path("data", "opportunities_scored_2026.parquet")
GRADES_OUT   <- file.path("data", "player_grades_2026.parquet")
PARAMS_OUT   <- file.path("data", "model_params_2026.parquet")
PLAYERS_FILE <- file.path("data", "players.parquet")

CHALLENGE_VALUE_RUNS <- 0.2
MIN_OPPS <- 100L   # opportunities needed to be graded
MIN_CHAL <- 15L    # challenges needed to be graded (8 let 9-challenge
# batters top the leaderboard on luck)

# ---- 1. Opportunities -------------------------------------------------------
# One row per called pitch where the side it hurt had a challenge left.
# Batting-side decisions belong to the batter, fielding-side to the catcher
# (pitchers made <2% of challenges; their few calls are credited to the catcher).

prep_opportunities <- function(takes) {
  takes |>
    filter(opportunity, !is.na(edge_dist_in), !is.na(overturn_value)) |>
    mutate(
      role          = if_else(side_against == "batting", "batter", "catcher"),
      decider_id    = as.integer(if_else(role == "batter", batter, catcher)),
      decider_team  = case_when(
        role == "batter"  & inning_topbot == "Top" ~ away_team,
        role == "batter"                           ~ home_team,
        inning_topbot == "Top"                     ~ home_team,
        TRUE                                       ~ away_team
      ),
      win_dist_in   = if_else(side_against == "batting", edge_dist_in, -edge_dist_in),
      z_breakeven   = qnorm(pmin(pmax(breakeven_conf, 0.01), 0.99)),
      challenged_here = challenged & coalesce(challenge_side == side_against, FALSE),
      won_here        = challenged_here & coalesce(overturned, FALSE)
    )
}

# ---- 2. Read-noise model (one per role) -------------------------------------

fit_read_model <- function(opps_role) {
  fit <- glm(challenged_here ~ win_dist_in + z_breakeven,
             family = binomial(link = "probit"), data = opps_role)
  b <- coef(fit)
  list(
    params = tibble(
      role          = opps_role$role[1],
      n_opps        = nrow(opps_role),
      sigma_in      = 1 / b[["win_dist_in"]],
      intercept     = b[["(Intercept)"]],
      leverage_coef = b[["z_breakeven"]]
    ),
    # League-average probability of a challenge on each of these pitches
    p_chal_expected = unname(predict(fit, newdata = opps_role, type = "response"))
  )
}

score_opportunities <- function(opps) {
  pieces <- opps |>
    group_split(role) |>
    map(\(d) {
      m <- fit_read_model(d)
      d <- d |>
        mutate(
          p_chal_expected = m$p_chal_expected,
          p_win = pnorm(win_dist_in / m$params$sigma_in),
          ev    = p_win * overturn_value - (1 - p_win) * CHALLENGE_VALUE_RUNS,
          should_challenge = ev > 0
        )
      list(opps = d, params = m$params)
    })
  
  list(
    opps   = map(pieces, "opps")   |> list_rbind(),
    params = map(pieces, "params") |> list_rbind()
  )
}

# ---- 3. Player names (MLB Stats API, cached) --------------------------------

lookup_names <- function(ids) {
  ids <- unique(as.integer(ids[!is.na(ids)]))
  cached <- if (file.exists(PLAYERS_FILE)) read_parquet(PLAYERS_FILE) else
    tibble(id = integer(), name = character())
  
  todo <- setdiff(ids, cached$id)
  if (length(todo) > 0) {
    new <- split(todo, ceiling(seq_along(todo) / 100)) |>
      map(\(chunk) {
        url <- paste0("https://statsapi.mlb.com/api/v1/people?personIds=",
                      paste(chunk, collapse = ","))
        fromJSON(url)$people |>
          as_tibble() |>
          transmute(id = as.integer(id), name = fullName)
      }) |>
      list_rbind()
    cached <- bind_rows(cached, new)
    write_parquet(cached, PLAYERS_FILE)
  }
  cached
}

# ---- 4. Player grades -------------------------------------------------------

mode_chr <- function(v) names(which.max(table(v)))

grade_players <- function(opps) {
  opps |>
    group_by(role, decider_id) |>
    summarise(
      team            = mode_chr(decider_team),
      opps            = n(),
      winnable        = sum(win_dist_in > 0),
      should          = sum(should_challenge),
      challenges      = sum(challenged_here),
      wins            = sum(won_here),
      x_challenges    = sum(p_chal_expected),
      missed_good     = sum(should_challenge & !challenged_here),
      bad_challenges  = sum(challenged_here & !should_challenge),
      ev_captured     = sum(ev[challenged_here]),
      ev_available    = sum(pmax(ev, 0)),
      realized_runs   = sum(if_else(won_here, overturn_value, 0)) -
        CHALLENGE_VALUE_RUNS * sum(challenged_here & !won_here),
      mean_ev_chal    = if (challenges > 0) mean(ev[challenged_here]) else NA_real_,
      x_mean_ev_chal  = sum(p_chal_expected * ev) / sum(p_chal_expected),
      .groups = "drop"
    ) |>
    mutate(
      overturn_rate = wins / challenges,
      aggression    = challenges / x_challenges,
      selectivity   = mean_ev_chal - x_mean_ev_chal,
      regret_runs   = ev_captured - ev_available,   # <= 0; closer to 0 is better
      # Season totals grow with opportunities, so a busy everyday catcher
      # looks worse than a backup. Rank on the per-100-opportunity rate.
      regret_per_100 = 100 * regret_runs / opps,
      graded        = opps >= MIN_OPPS & challenges >= MIN_CHAL,
      persona = case_when(
        !graded                              ~ "Not enough data",
        aggression >= 1 & selectivity >= 0   ~ "Brave",
        aggression >= 1                      ~ "Reckless",
        selectivity >= 0                     ~ "Chicken",
        TRUE                                 ~ "Lost"
      )
    )
}

# ---- Main -------------------------------------------------------------------

run_model <- function() {
  takes  <- read_parquet(TAKES_FILE)
  scored <- score_opportunities(prep_opportunities(takes))
  opps   <- scored$opps
  params <- scored$params
  
  names_tbl <- lookup_names(opps$decider_id)
  grades <- grade_players(opps) |>
    left_join(names_tbl, by = c("decider_id" = "id")) |>
    relocate(name, .after = decider_id)
  
  # Flag everyone who also catches regularly, so batter-role rows can be
  # compared: do catchers make better challengers when THEY are hitting?
  catcher_ids <- grades |>
    filter(role == "catcher", opps >= MIN_OPPS) |>
    pull(decider_id)
  grades <- grades |> mutate(is_catcher = decider_id %in% catcher_ids)
  
  write_parquet(params, PARAMS_OUT)
  write_parquet(grades, GRADES_OUT)
  write_parquet(
    opps |> select(game_pk, at_bat_number, pitch_number, game_date, role,
                   decider_id, decider_team, balls, strikes, outs_when_up, bases,
                   plate_x, plate_z, sz_top, sz_bot, edge_dist_in, win_dist_in,
                   original_call, true_call, overturn_value, breakeven_conf,
                   p_win, ev, should_challenge, p_chal_expected,
                   challenged_here, won_here),
    OPPS_OUT
  )
  
  report_model(opps, params, grades)
  invisible(list(opps = opps, params = params, grades = grades))
}

report_model <- function(opps, params, grades) {
  cat("\n== Read-noise model ==\n")
  print(params |> mutate(across(where(is.double), \(x) round(x, 3))))
  cat("  sigma_in: typical error in a player's read of the pitch (inches)\n")
  cat("  leverage_coef < 0: they challenge more when the stakes are higher\n")
  
  cat("\n== League decisions ==\n")
  # Output names deliberately differ from input columns: inside summarise(),
  # a new column named `should_challenge` would shadow the logical column and
  # every later line would see the summed integer instead.
  opps |>
    group_by(role) |>
    summarise(
      n_should              = sum(should_challenge),
      pct_should_challenged = round(100 * mean(challenged_here[should_challenge]), 1),
      n_challenges          = sum(challenged_here),
      pct_challenges_pos_ev = round(100 * mean(should_challenge[challenged_here]), 1),
      runs_left_on_table    = round(sum(pmax(ev, 0)) - sum(ev[challenged_here]), 1),
      .groups = "drop"
    ) |>
    print(width = Inf)
  
  cat("\n== Do catchers challenge better when THEY bat? ==\n")
  grades |>
    filter(role == "batter", challenges > 0) |>
    group_by(batter_is_catcher = is_catcher) |>
    summarise(
      batters        = n(),
      challenges     = sum(challenges),
      overturn_rate  = round(sum(wins) / sum(challenges), 3),
      pct_pos_ev     = round(100 * (1 - sum(bad_challenges) / sum(challenges)), 1),
      regret_per_100 = round(100 * sum(regret_runs) / sum(opps), 3),
      .groups = "drop"
    ) |>
    print(width = Inf)
  
  cat("\n== Personas (graded players) ==\n")
  grades |> filter(graded) |> count(role, persona) |>
    pivot_wider(names_from = persona, values_from = n, values_fill = 0) |> print()
  
  show <- \(d) d |> select(name, team, is_catcher, challenges, overturn_rate,
                           aggression, selectivity, regret_per_100, persona) |>
    mutate(across(where(is.double), \(x) round(x, 2)))
  
  for (r in c("batter", "catcher")) {
    g <- grades |> filter(graded, role == r)
    cat("\n-- Best", r, "decision-makers (least regret per 100 opps) --\n")
    print(show(slice_max(g, regret_per_100, n = 5)), width = Inf)
    cat("-- Worst --\n")
    print(show(slice_min(g, regret_per_100, n = 5)), width = Inf)
  }
}