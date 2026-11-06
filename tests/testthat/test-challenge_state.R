# tests/testthat/test-challenge_state.R

source(testthat::test_path("..", "..", "R", "challenge_state.R"))

HOME <- 1L; AWAY <- 2L

test_that("no challenges -> always 2", {
  expect_equal(remaining_before(c(1, 1, 2), c(NA, NA, NA), c(NA, NA, NA), HOME),
               c(2L, 2L, 2L))
})

test_that("failed challenge costs one, AFTER the pitch it was used on", {
  got <- remaining_before(c(1, 1, 1), c(HOME, NA, NA), c(FALSE, NA, NA), HOME)
  expect_equal(got, c(2L, 1L, 1L))
})

test_that("successful challenge is kept", {
  got <- remaining_before(c(1, 1), c(HOME, NA), c(TRUE, NA), HOME)
  expect_equal(got, c(2L, 2L))
})

test_that("the other team's challenges don't count against you", {
  got <- remaining_before(c(1, 1), c(AWAY, NA), c(FALSE, NA), HOME)
  expect_equal(got, c(2L, 2L))
})

test_that("two failures -> 0, and it never goes negative", {
  got <- remaining_before(c(1, 2, 3, 4), c(HOME, HOME, HOME, NA),
                          c(FALSE, FALSE, FALSE, NA), HOME)
  expect_equal(got, c(2L, 1L, 0L, 0L))
})

test_that("a team at 0 gets one back at the start of each extra inning", {
  got <- remaining_before(
    inning         = c(5, 6, 9, 10, 10, 11),
    challenge_team = c(HOME, HOME, NA, HOME, NA, NA),
    overturned     = c(FALSE, FALSE, NA, FALSE, NA, NA),
    team           = HOME
  )
  # 2 -> 1 -> 0 ... 10th: back to 1, fails -> 0 ... 11th: back to 1
  expect_equal(got, c(2L, 1L, 0L, 1L, 0L, 1L))
})

test_that("extra innings don't stack on top of remaining challenges", {
  got <- remaining_before(c(9, 10), c(NA, NA), c(NA, NA), HOME)
  expect_equal(got, c(2L, 2L))
})