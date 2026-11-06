# R/challenge_state.R
# How many ABS challenges a team has BEFORE each pitch of a game.
#
# Rules (2026):
#   - Each team starts with 2.
#   - A failed (confirmed) challenge costs 1; a successful one is kept.
#   - Extra innings: a team with none left starts each extra inning with 1.
#     (Modelled as max(remaining, 1) at the start of innings 10+. This is
#     checked in build_dataset(): no team should ever challenge with 0 left.)
#
# Inputs are one game's pitches in order. `challenge_team` is NA on pitches
# that weren't challenged.

START_CHALLENGES  <- 2L
FIRST_EXTRA_INNING <- 10L

remaining_before <- function(inning, challenge_team, overturned, team) {
  n    <- length(inning)
  out  <- integer(n)
  rem  <- START_CHALLENGES
  last <- -1L
  
  for (i in seq_len(n)) {
    if (!is.na(inning[i]) && inning[i] != last) {
      if (inning[i] >= FIRST_EXTRA_INNING) rem <- max(rem, 1L)
      last <- inning[i]
    }
    out[i] <- rem
    if (!is.na(challenge_team[i]) && challenge_team[i] == team &&
        isFALSE(overturned[i])) {
      rem <- max(rem - 1L, 0L)
    }
  }
  out
}