# R/run_expectancy.R
# Our own run-expectancy table (RE288: 12 counts x 8 base states x 3 outs),
# built from the 2026 season, and the run value of flipping a ball/strike call.
#
# Runs to the end of the half-inning, without a post-pitch score column:
# at the FIRST pitch of the next half-inning the teams have swapped, so that
# pitch's fld_score is the score the batting team finished its half-inning with.
#   runs_rest = (score at end of half-inning) - bat_score at this pitch
# Excluded, as in standard RE tables: bottom of the 9th, extra innings (ghost
# runner), and each game's final half-inning (no next half-inning to read).

library(dplyr)

BASE_STATES <- c("000", "100", "010", "001", "110", "101", "011", "111")

# Pseudo-pitches pulling a sparse cell toward a smooth additive model
# (base-out effect + count effect). A cell with n pitches keeps n/(n+K) of
# its own average. Only mid-count rare states are affected: 0-0 cells, which
# every walk/strikeout transition lands on, have thousands of pitches each.
RE_SHRINK_K <- 30

# ---- Walk: force runners along ---------------------------------------------
# "b1 b2 b3" -> new base string + runs forced in. A runner only moves if forced.
walk_advance <- function(bases) {
  b <- as.integer(strsplit(bases, "")[[1]])
  runs <- 0L
  if (b[1] == 0L) {
    b[1] <- 1L                 # batter to 1st, nobody forced
  } else if (b[2] == 0L) {
    b[2] <- 1L                 # 1st -> 2nd (3rd, if any, stays)
  } else if (b[3] == 0L) {
    b[3] <- 1L                 # 1st & 2nd -> bases loaded
  } else {
    runs <- 1L                 # bases loaded: run forced in, still loaded
  }
  list(bases = paste(b, collapse = ""), runs = runs)
}

# ---- Runs from each pitch to the end of its half-inning --------------------
runs_to_inning_end <- function(pitches) {
  p <- pitches |>
    mutate(half = as.integer(inning) * 2L + as.integer(inning_topbot == "Bot"))
  
  first_pitch <- p |>
    group_by(game_pk, half) |>
    slice_min(at_bat_number * 100 + pitch_number, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(game_pk, half, start_fld = fld_score)
  
  half_end <- first_pitch |>
    arrange(game_pk, half) |>
    group_by(game_pk) |>
    mutate(end_bat_score = lead(start_fld), next_half = lead(half)) |>
    ungroup() |>
    filter(next_half == half + 1L) |>          # only a directly following half
    select(game_pk, half, end_bat_score)
  
  p |>
    inner_join(half_end, by = c("game_pk", "half")) |>
    filter(inning <= 8 | (inning == 9 & inning_topbot == "Top")) |>
    mutate(runs_rest = end_bat_score - bat_score) |>
    filter(!is.na(runs_rest), runs_rest >= 0)
}

# ---- RE288 ------------------------------------------------------------------
compute_re288 <- function(re_data, k = RE_SHRINK_K) {
  d <- re_data |>
    filter(balls <= 3, strikes <= 2, bases %in% BASE_STATES) |>
    mutate(base_out = paste(bases, outs_when_up), count = paste(balls, strikes))
  
  smooth <- lm(runs_rest ~ base_out + count, data = d)
  
  grid <- expand.grid(balls = 0:3, strikes = 0:2, outs_when_up = 0:2,
                      bases = BASE_STATES, stringsAsFactors = FALSE) |>
    mutate(base_out = paste(bases, outs_when_up), count = paste(balls, strikes))
  grid$pred <- unname(predict(smooth, newdata = grid))
  
  cells <- d |>
    group_by(balls, strikes, outs_when_up, bases) |>
    summarise(n = n(), mean = mean(runs_rest), .groups = "drop")
  
  grid |>
    left_join(cells, by = c("balls", "strikes", "outs_when_up", "bases")) |>
    mutate(n  = coalesce(n, 0L),
           re = (n * coalesce(mean, 0) + k * pred) / (n + k)) |>
    select(balls, strikes, outs_when_up, bases, n, re)
}

# ---- Monotone count surface (isotonic regression) ---------------------------
# Baseball rule: a ball never hurts the batter, a strike never helps him. So
# within one base-out state, run expectancy must rise with balls and fall with
# strikes. Raw cells can break this through noise (a 3-0 cell with 10 pitches)
# or selection (with 1st base open, pitchers work around good hitters, so a
# 3-1 count partly measures WHO is batting). Isotonic regression finds the
# closest table, weighted by sample size, that obeys the rule: sparse cells
# give way, big cells barely move.

# Weighted pool-adjacent-violators: best non-decreasing fit to y.
pava <- function(y, w) {
  vals <- numeric(0); wts <- numeric(0); cnt <- integer(0)
  for (i in seq_along(y)) {
    vals <- c(vals, y[i]); wts <- c(wts, w[i]); cnt <- c(cnt, 1L)
    k <- length(vals)
    while (k > 1 && vals[k - 1] > vals[k]) {
      tw <- wts[k - 1] + wts[k]
      vals[k - 1] <- (vals[k - 1] * wts[k - 1] + vals[k] * wts[k]) / tw
      wts[k - 1]  <- tw
      cnt[k - 1]  <- cnt[k - 1] + cnt[k]
      vals <- vals[-k]; wts <- wts[-k]; cnt <- cnt[-k]
      k <- k - 1
    }
  }
  rep(vals, cnt)
}

# M: rows = strikes 0..3 (3 = strikeout), cols = balls 0..4 (4 = walk).
# Rows must be non-decreasing, columns non-increasing. Dykstra's alternating
# projections reach the weighted least-squares fit satisfying both.
monotone_count_surface <- function(M, W, iters = 500, tol = 1e-10) {
  proj_rows <- function(X) t(vapply(seq_len(nrow(X)),
                                    function(i) pava(X[i, ], W[i, ]), numeric(ncol(X))))
  proj_cols <- function(X) vapply(seq_len(ncol(X)),
                                  function(j) -pava(-X[, j], W[, j]), numeric(nrow(X)))
  x <- M; p <- 0 * M; q <- 0 * M
  for (it in seq_len(iters)) {
    y <- proj_rows(x + p); p <- x + p - y
    x_new <- proj_cols(y + q); q <- y + q - x_new
    done <- max(abs(x_new - x)) < tol
    x <- x_new
    if (done) break
  }
  x
}

# ---- Value of flipping a strike to a ball ----------------------------------
#   after a ball:   next count, or a WALK (forced runners, maybe a run in)
#   after a strike: next count, or a STRIKEOUT (one more out; 0 if it's the 3rd)
# A new plate appearance starts at 0-0, so walks/strikeouts land on 0-0 cells.
# With monotone = TRUE every value is >= 0 by construction.
FIXED_WEIGHT <- 1e6      

overturn_values_from_re <- function(re288, monotone = TRUE, k = RE_SHRINK_K) {
  key <- paste(re288$balls, re288$strikes, re288$outs_when_up, re288$bases)
  RE  <- function(b, s, o, bs) re288$re[match(paste(b, s, o, bs), key)]
  N   <- function(b, s, o, bs) re288$n[match(paste(b, s, o, bs), key)]
  
  rows <- list()
  for (o in 0:2) for (bs in BASE_STATES) {
    wa       <- walk_advance(bs)
    walk_val <- wa$runs + RE(0, 0, o, wa$bases)
    k_val    <- if (o == 2) 0 else RE(0, 0, o + 1, bs)
    
    M <- matrix(NA_real_, 4, 5); W <- matrix(NA_real_, 4, 5)
    for (b in 0:3) for (s in 0:2) {
      M[s + 1, b + 1] <- RE(b, s, o, bs)
      W[s + 1, b + 1] <- N(b, s, o, bs) + k
    }
    M[1:3, 5] <- walk_val; W[1:3, 5] <- FIXED_WEIGHT
    M[4, 1:4] <- k_val;    W[4, 1:4] <- FIXED_WEIGHT
    M[4, 5]   <- k_val;    W[4, 5]   <- 1e-6          # walk + strikeout: impossible, unused
    
    X <- if (monotone) monotone_count_surface(M, W) else M
    
    for (b in 0:3) for (s in 0:2) {
      rows[[length(rows) + 1]] <- data.frame(
        balls = b, strikes = s, outs_when_up = o, bases = bs,
        after_ball     = X[s + 1, b + 2],
        after_strike   = X[s + 2, b + 1],
        raw_value      = M[s + 1, b + 2] - M[s + 2, b + 1],
        stringsAsFactors = FALSE
      )
    }
  }
  bind_rows(rows) |>
    mutate(overturn_value = pmax(after_ball - after_strike, 0))   # clears ~1e-10 rounding
}