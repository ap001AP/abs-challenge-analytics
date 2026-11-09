# tests/testthat/test-run_expectancy.R

source(testthat::test_path("..", "..", "R", "run_expectancy.R"))

test_that("walks force runners only when forced", {
  cases <- list(
    "000" = list("100", 0L), "100" = list("110", 0L), "010" = list("110", 0L),
    "001" = list("101", 0L), "110" = list("111", 0L), "101" = list("111", 0L),
    "011" = list("111", 0L), "111" = list("111", 1L)
  )
  for (b in names(cases)) {
    got <- walk_advance(b)
    expect_equal(got$bases, cases[[b]][[1]], info = b)
    expect_equal(got$runs,  cases[[b]][[2]], info = b)
  }
})

# A toy, count-free RE table: every count in a base-out state has the same RE.
toy_re24 <- function() {
  re24 <- c("000" = 0.5, "100" = 0.9, "010" = 1.1, "001" = 1.4,
            "110" = 1.5, "101" = 1.8, "011" = 2.0, "111" = 2.3)
  outs_scale <- c(1, 0.6, 0.25)                 # fewer runs with more outs
  expand.grid(balls = 0:3, strikes = 0:2, outs_when_up = 0:2,
              bases = BASE_STATES, stringsAsFactors = FALSE) |>
    # unname(): a lookup by name keeps the names, and expect_equal compares them
    mutate(n = 100L, re = unname(re24[bases]) * outs_scale[outs_when_up + 1])
}

test_that("mid-count flips are worth 0 when the count doesn't matter", {
  ov <- overturn_values_from_re(toy_re24())
  v  <- ov$overturn_value[ov$balls == 1 & ov$strikes == 1]
  expect_true(all(abs(v) < 1e-12))
})

test_that("3-2 bases loaded, 0 out = forced run + loaded/0 out - loaded/1 out", {
  ov <- overturn_values_from_re(toy_re24())
  v  <- with(ov, overturn_value[balls == 3 & strikes == 2 & outs_when_up == 0 & bases == "111"])
  expect_equal(v, 1 + 2.3 - 2.3 * 0.6)
})

test_that("a two-out strikeout ends the inning (worth 0 afterwards)", {
  ov <- overturn_values_from_re(toy_re24())
  v  <- with(ov, overturn_value[balls == 3 & strikes == 2 & outs_when_up == 2 & bases == "000"])
  expect_equal(v, 0.9 * 0.25 - 0)               # walk -> runner on 1st, 2 out
})

test_that("a loaded full count is worth far more than an empty one", {
  ov <- overturn_values_from_re(toy_re24())
  at <- \(bs) with(ov, overturn_value[balls == 3 & strikes == 2 & outs_when_up == 0 & bases == bs])
  expect_gt(at("111"), 2 * at("000"))
})

test_that("shrinkage: an empty cell takes the smooth model's value", {
  grid <- expand.grid(balls = 0:3, strikes = 0:2, outs_when_up = 0:2,
                      bases = BASE_STATES, stringsAsFactors = FALSE)
  d <- grid[rep(seq_len(nrow(grid)), each = 3), ]
  d$runs_rest <- 1
  # remove every pitch from one cell: 3-2, bases loaded, 0 out
  d <- d[!(d$balls == 3 & d$strikes == 2 & d$outs_when_up == 0 & d$bases == "111"), ]
  
  re <- compute_re288(d)
  empty <- re[re$balls == 3 & re$strikes == 2 & re$outs_when_up == 0 & re$bases == "111", ]
  expect_equal(nrow(re), 288)
  expect_equal(empty$n, 0L)
  expect_equal(empty$re, 1, tolerance = 1e-8)   # all data say 1 run -> so does the model
})

test_that("pava returns the weighted non-decreasing fit", {
  expect_equal(pava(c(1, 2, 3), c(1, 1, 1)), c(1, 2, 3))          # already ordered
  expect_equal(pava(c(1, 3, 2), c(1, 1, 1)), c(1, 2.5, 2.5))      # pool the violators
  expect_equal(pava(c(1, 3, 2), c(1, 3, 1)), c(1, 2.75, 2.75))    # heavier cell wins
})

test_that("monotone surface fixes violations and keeps heavy cells in place", {
  M <- matrix(c(1.00, 1.05, 1.10, 1.15, 2.0,
                0.95, 0.70, 1.05, 1.10, 2.0,     # 0.70: a noisy, light cell
                0.90, 0.95, 1.00, 1.05, 2.0,
                0.50, 0.50, 0.50, 0.50, 0.5), nrow = 4, byrow = TRUE)
  W <- matrix(500, 4, 5); W[2, 2] <- 10
  W[1:3, 5] <- 1e6; W[4, ] <- 1e6
  X <- monotone_count_surface(M, W)
  expect_true(all(diff(t(X)) >= -1e-8))   # rows rise with balls
  expect_true(all(diff(X)    <=  1e-8))   # columns fall with strikes
  expect_equal(X[1:3, 5], rep(2.0, 3), tolerance = 1e-4)   # walk edge fixed
  expect_lt(abs(X[1, 1] - M[1, 1]), 0.01)                  # heavy cell barely moves
})

test_that("noisy tables never produce a negative overturn value", {
  set.seed(42)
  noisy <- toy_re24() |> mutate(n = sample(5:800, n(), replace = TRUE),
                                re = re + rnorm(n(), 0, 0.3) / sqrt(n) * 3)
  ov <- overturn_values_from_re(noisy)
  expect_true(all(ov$overturn_value >= 0))
  expect_true(any(ov$raw_value < 0))      # the raw table did have violations
})