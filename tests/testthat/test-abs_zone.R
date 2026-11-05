# tests/testthat/test-abs_zone.R
# Fixtures are the 6 real ABS challenges from game 824027 (2026-06-01, COL @ LAA).
# Statcast stores the FINAL call, so final_call is what ABS ruled.

source(testthat::test_path("..", "..", "R", "abs_zone.R"))

challenges <- data.frame(
  plate_x    = c(-0.397, 0.351, -0.751, 0.719, -0.138, -0.435),
  plate_z    = c( 1.47,  1.59,   1.66,  1.87,   1.59,   1.43),
  sz_top     = c( 3.20,  3.30,   3.20,  3.23,   3.34,   3.23),
  sz_bot     = c( 1.62,  1.67,   1.62,  1.63,   1.69,   1.63),
  final_call = c("B",   "S",    "S",   "S",    "S",    "B")
)

test_that("abs_call reproduces every real ABS ruling", {
  got <- with(challenges, abs_call(plate_x, plate_z, sz_top, sz_bot))
  expect_equal(got, challenges$final_call)
})

test_that("dead-centre pitch is deep inside the zone", {
  d <- edge_distance_in(0, 2.5, 3.5, 1.5)
  expect_lt(d, -6)
})

test_that("ball just grazing the side edge is a strike", {
  x <- PLATE_HALF_WIDTH_FT + BALL_RADIUS_FT - 0.001
  expect_equal(abs_call(x, 2.5, 3.5, 1.5), "S")
})

test_that("ball just missing the side edge is a ball", {
  x <- PLATE_HALF_WIDTH_FT + BALL_RADIUS_FT + 0.001
  expect_equal(abs_call(x, 2.5, 3.5, 1.5), "B")
})

test_that("corners use diagonal distance, not the larger gap", {
  # Centre sits 0.1 ft beyond BOTH the side and top edges.
  # Diagonal gap = sqrt(0.1^2 + 0.1^2) = 0.141 ft > ball radius -> ball,
  # even though each single gap (0.1 ft) is smaller than the radius.
  x <- PLATE_HALF_WIDTH_FT + 0.1
  z <- 3.5 + 0.1
  expect_equal(abs_call(x, z, 3.5, 1.5), "B")
})

test_that("missing location gives NA, not an error", {
  expect_true(is.na(abs_call(NA, 2.5, 3.5, 1.5)))
})