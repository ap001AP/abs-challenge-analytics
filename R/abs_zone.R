# R/abs_zone.R
# ABS strike-zone geometry.
#
# Conventions (Statcast units = feet):
#   plate_x : horizontal position of ball centre, 0 = middle of plate (catcher's view)
#   plate_z : vertical position of ball centre above ground
#   sz_top / sz_bot : batter's ABS zone top/bottom (fixed per batter in 2026)
#
# ABS rule: a pitch is a strike if ANY part of the ball touches the zone.
# So we measure the distance from the ball's CENTRE to the zone rectangle,
# and call it a strike when that distance <= the ball's radius.

PLATE_HALF_WIDTH_FT <- (17 / 2) / 12      # 17-inch plate -> 0.7083 ft
BALL_RADIUS_FT      <- (2.9 / 2) / 12     # ~2.9-inch diameter -> 0.1208 ft

# Signed distance (ft) from ball centre to the zone rectangle.
#   > 0 : centre is outside the zone, by that much
#   < 0 : centre is inside the zone, by that much (distance to nearest edge)
# Handles corners correctly: outside a corner, the distance is diagonal,
# not just the larger of the x and z gaps.
zone_signed_distance <- function(plate_x, plate_z, sz_top, sz_bot) {
  dx <- abs(plate_x) - PLATE_HALF_WIDTH_FT
  dz <- pmax(sz_bot - plate_z, plate_z - sz_top)
  
  outside <- sqrt(pmax(dx, 0)^2 + pmax(dz, 0)^2)
  inside  <- pmin(pmax(dx, dz), 0)
  outside + inside
}

# Distance from the ball's EDGE to the zone (inches).
#   <= 0 : ball touches the zone -> strike
#   >  0 : ball misses the zone by that many inches -> ball
# This is the "how close was it?" feature for the challenge model.
edge_distance_in <- function(plate_x, plate_z, sz_top, sz_bot) {
  (zone_signed_distance(plate_x, plate_z, sz_top, sz_bot) - BALL_RADIUS_FT) * 12
}

# What ABS would call: "S" (strike) or "B" (ball).
abs_call <- function(plate_x, plate_z, sz_top, sz_bot) {
  d <- edge_distance_in(plate_x, plate_z, sz_top, sz_bot)
  ifelse(is.na(d), NA_character_, ifelse(d <= 0, "S", "B"))
}

# A few batters show 2 distinct sz_top/sz_bot values in a season (likely
# re-measurement). Use each batter's most common value so the zone is stable.
stable_zone <- function(df) {
  mode_val <- function(v) {
    v <- v[!is.na(v)]
    if (length(v) == 0) return(NA_real_)
    as.numeric(names(which.max(table(v))))
  }
  df |>
    dplyr::group_by(batter) |>
    dplyr::mutate(
      sz_top_abs = mode_val(sz_top),
      sz_bot_abs = mode_val(sz_bot)
    ) |>
    dplyr::ungroup()
}