# app/app.R
# Tap or Don't Tap: grading every MLB ABS challenge decision of 2026.
# Run from the project root with:  shiny::runApp("app")

library(shiny)
library(bslib)
library(dplyr)
library(plotly)

# ---- Data -------------------------------------------------------------------

grades    <- readRDS("app_data/grades.rds")
decisions <- readRDS("app_data/decisions.rds")
params    <- readRDS("app_data/params.rds")
sigma_groups <- readRDS("app_data/sigma_groups.rds")
read_curves  <- readRDS("app_data/read_curves.rds")
league       <- readRDS("app_data/league.rds")
overturn_values <- readRDS("app_data/overturn_values.rds")
robbed       <- readRDS("app_data/robbed.rds")
luck         <- readRDS("app_data/luck.rds")
park_bbe     <- readRDS("app_data/park_bbe.rds")
stadiums     <- readRDS("app_data/stadiums.rds")
# edge_distance_in(), abs_call() etc. come from app/R/abs_zone.R (auto-loaded)

REPO_URL <- "https://github.com/ap001AP/abs-challenge-analytics"
RUNS_PER_WIN <- 10      # standard sabermetric rule of thumb
N_TEAMS <- 30

# Headline numbers, all derived from the data
HL <- local({
  tot <- grades |>
    summarise(winnable = sum(winnable), wins = sum(wins),
              runs_left = sum(ev_available) - sum(ev_captured))
  by_role <- grades |>
    group_by(role) |>
    summarise(captured = sum(wins) / sum(winnable), .groups = "drop")
  list(
    captured   = tot$wins / tot$winnable,
    left       = tot$winnable - tot$wins,
    winnable   = tot$winnable,
    runs_left  = tot$runs_left,
    wins_left  = tot$runs_left / RUNS_PER_WIN,
    per_team   = tot$runs_left / RUNS_PER_WIN / N_TEAMS,
    cap_catch  = by_role$captured[by_role$role == "catcher"],
    cap_bat    = by_role$captured[by_role$role == "batter"]
  )
})

# ---- Design tokens: "neon scoreboard" dark theme ----------------------------
# Data colours (teal / pink / purple) were run through a colour-blind
# validator against the dark surface: all pairs pass, including
# deuteranopia (teal vs pink is the classic trap, so pink is kept darker than
# teal to separate them by lightness). Every series also has its own marker
# shape and a legend, so colour is never the only cue.
# Brighter "neon" tints are used only for decoration (title, glows), never data.

COL <- list(
  surface   = "#0d0b1e",   # page
  card      = "#15122b",   # cards / plot background
  border    = "#2a2550",
  text1     = "#ece9ff",
  text2     = "#a9a3c9",
  grid      = "#262145",
  others    = "#4a4566",   # non-highlighted players
  highlight = "#00a99d",   # selected team / occupied base (teal)
  won       = "#00a99d",   # teal
  lost      = "#c2378a",   # pink
  missed    = "#8a6cf2",   # purple
  neon_teal = "#2ee6d6",
  neon_pink = "#ff4fb4",
  neon_purp = "#b388ff"
)

NEON_CSS <- sprintf("
  .navbar { background: %1$s !important; border-bottom: 1px solid %3$s; }
  .navbar-brand {
    font-family: Rajdhani, sans-serif; font-weight: 700; letter-spacing: .08em;
    text-transform: uppercase;
    background: linear-gradient(90deg, %5$s, %7$s, %6$s);
    -webkit-background-clip: text; background-clip: text; color: transparent !important;
  }
  .navbar .nav-link { color: %4$s !important; letter-spacing: .04em; }
  .navbar .nav-link.active {
    color: %5$s !important; text-shadow: 0 0 8px rgba(46,230,214,.55);
    border-bottom: 2px solid %5$s !important;
  }
  .card {
    background: %2$s; border: 1px solid %3$s;
    box-shadow: 0 0 0 1px rgba(46,230,214,.06), 0 0 28px rgba(138,108,242,.14);
  }
  .card-header {
    background: transparent; border-bottom: 1px solid %3$s;
    font-family: Rajdhani, sans-serif; font-weight: 600;
    letter-spacing: .06em; text-transform: uppercase;
  }
  .bslib-sidebar-layout > .sidebar { background: %2$s; border-right: 1px solid %3$s; }
  .form-select, .form-control { background-color: %1$s; border-color: %3$s; color: inherit; }
  .stat-value { font-family: Rajdhani, sans-serif; font-size: 1.6rem; font-weight: 700; }
  hr { border-color: %3$s; opacity: 1; }
  .hero {
    font-family: Rajdhani, sans-serif; font-weight: 700; font-size: 3.2rem; line-height: 1.1;
    background: linear-gradient(90deg, %5$s, %7$s, %6$s);
    -webkit-background-clip: text; background-clip: text; color: transparent;
  }
  .eyebrow { letter-spacing: .14em; text-transform: uppercase; font-size: .8rem; }
  .btn-neon {
    color: %5$s; border: 1px solid %5$s; background: transparent;
    letter-spacing: .04em;
  }
  .btn-neon:hover, .btn-neon:focus {
    color: %1$s; background: %5$s; box-shadow: 0 0 16px rgba(46,230,214,.6);
  }
  .text-secondary { color: %4$s !important; }

", COL$surface, COL$card, COL$border, COL$text2,
                    COL$neon_teal, COL$neon_pink, COL$neon_purp)

# Shared plotly styling for the dark cards.
dark_layout <- function(p, ...) {
  layout(
    p, ...,
    plot_bgcolor = COL$card, paper_bgcolor = COL$card,
    font = list(color = COL$text2),
    hoverlabel = list(bgcolor = COL$surface, bordercolor = COL$neon_teal, align = "left",
                      font = list(color = COL$text1, size = 13))
  )
}

ZONE_HALF_FT <- (17 / 2) / 12
TEAMS <- sort(unique(grades$team))
AGG_TICKS  <- c(-2, -1, 0, 1, 2)                   # log2(aggression)
AGG_LABELS <- c("¼×", "½×", "1× league", "2×", "4×")
CALL_WORD  <- c(S = "STRIKE", B = "BALL")

# ---- Helpers ----------------------------------------------------------------

# "Vladimir Guerrero Jr." -> "Guerrero", not "Jr."
last_name <- function(x) {
  x <- sub("\\s+(Jr\\.?|Sr\\.?|II|III|IV)$", "", x)
  sub("^.* ", "", x)
}

# "010" (1st, 2nd, 3rd) -> "Runner on 2nd"
bases_text <- function(bases) {
  vapply(bases, \(b) {
    on <- c("1st", "2nd", "3rd")[strsplit(b, "")[[1]] == "1"]
    if (length(on) == 0) "Bases empty"
    else if (length(on) == 3) "Bases loaded"
    else if (length(on) == 1) paste("Runner on", on)
    else paste("Runners on", paste(on, collapse = " & "))
  }, character(1), USE.NAMES = FALSE)
}

verdict_text <- function(outcome, ev, ump_wrong = TRUE) {
  case_when(
    outcome == "Missed" & ump_wrong  ~ "should have challenged (would have won)",
    outcome == "Missed"              ~ "worth the gamble, even though it would have lost",
    outcome == "Won"  & ev > 0  ~ "good bet, paid off",
    outcome == "Won"            ~ "bad bet, got lucky",
    outcome == "Lost" & ev > 0  ~ "good bet, didn't pay off",
    TRUE                        ~ "bad bet"
  )
}

# Small infield diamond, occupied bases filled. Home plate at the bottom.
diamond_svg <- function(bases) {
  on <- strsplit(bases, "")[[1]] == "1"           # 1st, 2nd, 3rd
  base <- function(cx, cy, filled) sprintf(
    '<rect x="%g" y="%g" width="13" height="13" rx="1.5" transform="rotate(45 %g %g)" fill="%s" stroke="%s" stroke-width="2"/>',
    cx - 6.5, cy - 6.5, cx, cy,
    if (filled) COL$highlight else COL$card, COL$text2
  )
  HTML(sprintf(
    '<svg width="72" height="72" viewBox="0 0 72 72" role="img" aria-label="%s">
       <polygon points="36,60 60,36 36,12 12,36" fill="none" stroke="%s" stroke-width="2"/>
       <rect x="32" y="56" width="8" height="8" fill="%s"/>
       %s %s %s
     </svg>',
    bases_text(bases), COL$grid, COL$text2,
    base(60, 36, on[1]), base(36, 12, on[2]), base(12, 36, on[3])
  ))
}

outs_dots <- function(outs) {
  dots <- vapply(1:2, \(i) sprintf(
    '<circle cx="%d" cy="7" r="5.5" fill="%s" stroke="%s" stroke-width="2"/>',
    8 + (i - 1) * 16, if (i <= outs) COL$text1 else COL$card, COL$text2
  ), character(1))
  HTML(sprintf('<svg width="34" height="14" role="img" aria-label="%d out">%s</svg>',
               outs, paste(dots, collapse = "")))
}

call_badge <- function(label, call, emphasis = FALSE) {
  div(
    class = "text-center",
    div(class = "small text-secondary", label),
    span(
      class = "d-inline-block px-2 py-1 fw-semibold",
      style = sprintf("border:2px solid %s; border-radius:4px; min-width:80px;",
                      if (emphasis) COL$neon_teal else COL$border),
      CALL_WORD[[call]]
    )
  )
}

# ---- Who Sees the Zone helpers ------------------------------------------------
# Three groups, three validated slots (teal / purple / pink), each also with
# its own marker shape in the chart and its name on every card.
GROUP_STYLE <- list(
  "Catchers, catching" = list(col = COL$won,    symbol = "circle",  short = "Catchers behind the plate"),
  "Catchers, batting"  = list(col = COL$missed, symbol = "diamond", short = "The same catchers at the plate"),
  "Other batters"      = list(col = COL$lost,   symbol = "square",  short = "Everyone else at the plate")
)

sig <- function(g) sigma_groups$sigma_in[sigma_groups$group == g]

# A 17 x 24 in strike zone with a blurred band of +/- sigma around the edge:
# the region where this group can't tell ball from strike.
sigma_zone_svg <- function(sigma, col, id) {
  w <- 17; h <- 24; pad <- 8.5; r <- 1.45
  HTML(paste0(
    sprintf('<svg viewBox="%g %g %g %g" width="100%%" style="max-width:240px" role="img" aria-label="Strike zone with a fuzzy band of plus or minus %.1f inches">',
            -pad, -pad, w + 2 * pad, h + 2 * pad, sigma),
    sprintf('<defs><filter id="%s" x="-50%%" y="-50%%" width="200%%" height="200%%"><feGaussianBlur stdDeviation="%g"/></filter></defs>',
            id, sigma / 2),
    # outer glow, then punch out the confident middle
    sprintf('<rect x="%g" y="%g" width="%g" height="%g" fill="%s" opacity="0.55" filter="url(#%s)"/>',
            -sigma, -sigma, w + 2 * sigma, h + 2 * sigma, col, id),
    sprintf('<rect x="%g" y="%g" width="%g" height="%g" fill="%s" filter="url(#%s)"/>',
            sigma, sigma, max(w - 2 * sigma, 0.1), max(h - 2 * sigma, 0.1), COL$card, id),
    # crisp +/- sigma guides
    sprintf('<rect x="%g" y="%g" width="%g" height="%g" fill="none" stroke="%s" stroke-width="0.2" stroke-dasharray="0.8 0.8"/>',
            -sigma, -sigma, w + 2 * sigma, h + 2 * sigma, col),
    sprintf('<rect x="%g" y="%g" width="%g" height="%g" fill="none" stroke="%s" stroke-width="0.2" stroke-dasharray="0.8 0.8"/>',
            sigma, sigma, max(w - 2 * sigma, 0.1), max(h - 2 * sigma, 0.1), col),
    # the zone itself + a baseball to scale, touching the edge
    sprintf('<rect x="0" y="0" width="%g" height="%g" fill="none" stroke="%s" stroke-width="0.35"/>',
            w, h, COL$text1),
    sprintf('<circle cx="%g" cy="%g" r="%g" fill="none" stroke="%s" stroke-width="0.3"/>',
            w + r, h / 2, r, COL$text1),
    '</svg>'
  ))
}

sigma_card <- function(g) {
  row <- sigma_groups[sigma_groups$group == g, ]
  st  <- GROUP_STYLE[[g]]
  card(
    card_header(span(style = sprintf("color:%s;", st$col), "■ "), st$short),
    div(class = "text-center",
        sigma_zone_svg(row$sigma_in, st$col, paste0("blur", match(g, names(GROUP_STYLE)))),
        div(class = "stat-value mt-2", style = "font-size:2.2rem;",
            sprintf("±%.1f in", row$sigma_in)),
        div(class = "small text-secondary",
            sprintf("typical read error · 95%% CI %.1f–%.1f", row$sigma_lo, row$sigma_hi)),
        div(class = "small text-secondary",
            sprintf("%s players · %s challenges · %s opportunities",
                    row$n_players, format(row$n_chal, big.mark = ","),
                    format(row$n_opps, big.mark = ","))))
  )
}

# ---- Should He Have Tapped? helpers ----------------------------------------------
CHALLENGE_VALUE_RUNS <- 0.2
SIGMA_ROLE <- setNames(params$sigma_in, params$role)          # batter / catcher
STD_TOP <- 3.5; STD_BOT <- 1.5                                # standard zone (ft)
TAP_XS <- seq(-1.6, 1.6, by = 0.04)
TAP_ZS <- seq(0.8, 4.2, by = 0.04)

lookup_value <- function(b, s, o, bs) {
  r <- overturn_values[overturn_values$balls == b & overturn_values$strikes == s &
                         overturn_values$outs_when_up == o & overturn_values$bases == bs, ]
  if (nrow(r) > 0) return(r$overturn_value[1])
  r <- overturn_values[overturn_values$balls == b & overturn_values$strikes == s &
                         overturn_values$outs_when_up == o, ]
  mean(r$overturn_value)    # rare state: fall back to count + outs
}

# P(win) and EV of challenging a pitch at (x, z) in a given situation
tap_eval <- function(x, z, st) {
  d   <- edge_distance_in(x, z, STD_TOP, STD_BOT)  # > 0: ball misses the zone
  win <- if (st$call == "S") d else -d             # > 0: umpire got it wrong
  p   <- pnorm(win / st$sigma)
  list(edge = d, p = p, ev = p * st$v - (1 - p) * CHALLENGE_VALUE_RUNS)
}

# ---- Robbed helpers ---------------------------------------------------------------
ROB_TEAMS <- sort(unique(robbed$team))

# Statcast hit coordinates -> feet from home plate (home at 125.42, 198.27;
# ~2.5 ft per unit is the standard approximation).
spray_xy <- function(hc_x, hc_y) list(x = 2.5 * (hc_x - 125.42), y = 2.5 * (198.27 - hc_y))

# Field outlines use add_paths(), NOT add_lines(): add_lines() sorts points by x
# (fine for a time series), which scrambles a curved outline into zigzags.
# A field drawn as line traces: foul lines, outfield wall (330 ft lines, 400 ft
# centre), infield diamond, infield arc.
field_traces <- function(p) {
  th   <- seq(-pi / 4, pi / 4, length.out = 90)              # 0 = straight away centre
  wall <- 330 + 70 * cos(2 * th)
  arc  <- 95
  line <- list(color = COL$border, width = 2)
  p |>
    add_paths(x = c(-233, 0, 233), y = c(233, 0, 233), line = line,
              hoverinfo = "skip", showlegend = FALSE, inherit = FALSE) |>
    add_paths(x = wall * sin(th), y = wall * cos(th), line = line,
              hoverinfo = "skip", showlegend = FALSE, inherit = FALSE) |>
    add_paths(x = arc * sin(th), y = arc * cos(th), line = list(color = COL$border, width = 1),
              hoverinfo = "skip", showlegend = FALSE, inherit = FALSE) |>
    add_paths(x = c(0, 63.6, 0, -63.6, 0), y = c(0, 63.6, 127.3, 63.6, 0),
              line = list(color = COL$text2, width = 1.5),
              hoverinfo = "skip", showlegend = FALSE, inherit = FALSE)
}

PARKS <- sort(park_bbe$park)

# Parks whose outline may not match where the team played in 2026. Verify
# against the 2026 schedule and edit this list.
PARK_CAVEAT <- c(
  ATH = "The A's have been playing in a temporary home, so this outline may not match the 2026 park.",
  TB  = "The Rays were displaced from Tropicana Field after hurricane damage, so this outline may not match the 2026 park."
)
# (No Rangers caveat: the outline data uses the 2022 "guardians" name and was
#  rebuilt in 2023, after Texas moved to its new park in 2020.)

# A real park's walls, foul lines and infield, or the generic field if we
# have no outline for it.
park_traces <- function(p, park_code) {
  d <- stadiums[stadiums$park == park_code, ]
  if (nrow(d) == 0) return(field_traces(p))
  xy <- spray_xy(d$x, d$y); d$px <- xy$x; d$py <- xy$y
  for (sg in split(d, factor(d$segment, levels = unique(d$segment)))) {
    sg   <- sg[order(sg$order), ]
    wall <- sg$segment[1] == "outfield_outer"
    p <- p |> add_paths(
      x = sg$px, y = sg$py, hoverinfo = "skip", showlegend = FALSE, inherit = FALSE,
      line = list(color = if (wall) COL$text2 else COL$border, width = if (wall) 2.5 else 1.5)
    )
  }
  p
}

# ---- Headline helpers -----------------------------------------------------------
comma <- function(x) formatC(round(x), format = "d", big.mark = ",")

# 0.75 -> "3 of every 4"; falls back to a percentage if no simple fraction fits
as_fraction <- function(p) {
  fr <- list(c(1, 2), c(2, 3), c(3, 4), c(4, 5), c(1, 3), c(1, 4))
  for (f in fr) if (abs(p - f[1] / f[2]) < 0.03) return(sprintf("%d of every %d", f[1], f[2]))
  sprintf("%.0f%%", 100 * p)
}

stat_tile <- function(value, label, sub, col) {
  card(
    style = sprintf("border-top: 3px solid %s;", col),
    card_body(
      class = "text-center", fillable = FALSE,
      div(class = "stat-value mb-1",
          style = sprintf("font-size:2.6rem; color:%s; text-shadow:0 0 16px %s88;", COL$text1, col),
          value),
      div(class = "fw-semibold", label),
      div(class = "small text-secondary", sub)
    )
  )
}

neon_bar <- function(label, value, col) {
  div(class = "mb-3",
      div(class = "d-flex justify-content-between align-items-baseline mb-1",
          span(label),
          span(class = "stat-value", style = "font-size:1.2rem;", sprintf("%.0f%%", 100 * value))),
      div(style = sprintf("height:12px; border-radius:6px; background:%s; border:1px solid %s;",
                          COL$surface, COL$border),
          div(style = sprintf("width:%.1f%%; height:100%%; border-radius:6px; background:%s; box-shadow:0 0 12px %s;",
                              100 * value, col, col))))
}

explore_card <- function(title, text, button_id, button_label) {
  card(card_body(
    class = "d-flex flex-column",
    h5(class = "mb-2", title),
    p(class = "text-secondary", text),
    div(class = "mt-auto", actionButton(button_id, button_label, class = "btn-neon w-100"))
  ))
}

coming_soon <- function(title, text) {
  card(card_header(title), p(class = "text-secondary", text))
}

# ---- UI ---------------------------------------------------------------------

ui <- page_navbar(
  id = "nav",
  title = "Tap or Don't Tap: MLB ABS Challenges, 2026",
  fillable = FALSE,   # let the page scroll; never squash a card's plot
  theme = bs_theme(
    version = 5, bg = COL$surface, fg = COL$text1,
    primary = COL$highlight, secondary = COL$text2,
    base_font = font_google("Inter"),
    heading_font = font_google("Rajdhani")
  ) |> bs_add_rules(NEON_CSS),
  
  nav_panel(
    "Headline", value = "headline",
    div(class = "py-4",
        div(class = "eyebrow text-secondary mb-2",
            "2026 · Year one of MLB's ABS challenge system"),
        div(class = "hero mb-3",
            sprintf("MLB players let %s winnable challenges go.", as_fraction(1 - HL$captured))),
        p(class = "lead text-secondary mb-4",
          sprintf("We graded all %s ABS challenges of the season, and the %s missed calls nobody challenged.",
                  comma(league$n_challenges), comma(HL$left))),
        
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          stat_tile(comma(league$n_challenges), "challenges made",
                    sprintf("%.0f%% overturned", 100 * league$overturn_rate), COL$won),
          stat_tile(sprintf("%.1f%%", 100 * league$ump_accuracy), "of called pitches the ump got right",
                    sprintf("%s called pitches", comma(league$n_takes)), COL$missed),
          stat_tile(sprintf("~%.0f", HL$runs_left), "runs left on the table",
                    sprintf("≈ %.0f wins league-wide, ~%.0f per team", HL$wins_left, HL$per_team), COL$lost),
          stat_tile(sprintf("%.1f×", sig("Other batters") / sig("Catchers, catching")),
                    "sharper read from behind the plate",
                    sprintf("catchers ±%.1f in vs hitters ±%.1f in",
                            sig("Catchers, catching"), sig("Other batters")), COL$won)
        ),
        
        layout_columns(
          col_widths = c(5, 7),
          card(
            card_header("Winnable missed calls actually challenged and won"),
            card_body(
              neon_bar("Catchers (calls against their pitcher)", HL$cap_catch, COL$won),
              neon_bar("Batters (calls against themselves)",     HL$cap_bat,   COL$lost),
              neon_bar("League-wide (catchers + batters combined)", HL$captured, COL$missed),
              p(class = "small text-secondary mb-0",
                "A winnable call = the umpire missed it and the player's team still had a challenge.")
            )
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            explore_card("Who sees the zone?",
                         "Catchers read pitches far better than hitters, but only from behind the plate.",
                         "go_zone", "Explore →"),
            explore_card("Brave or Chicken?",
                         "Every catcher and batter graded on how often, and how wisely, they challenge.",
                         "go_quad", "Explore →"),
            explore_card("Should he have tapped?",
                         "Pick a count and a pitch location. See if a challenge is worth it.",
                         "go_tap", "Explore →")
          )
        ),
        
        card(
          card_header("How it works"),
          card_body(
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              div(strong("1 · Data"), p(class = "small text-secondary",
                                        sprintf("%s pitches from %s games (Statcast via baseballr) + every challenge from the MLB Stats API game feeds.",
                                                comma(league$n_pitches), comma(league$n_games)))),
              div(strong("2 · Validation"), p(class = "small text-secondary",
                                              "Every captured challenge reconciled against MLB's official per-team totals: 100% of team-games match.")),
              div(strong("3 · Model"), p(class = "small text-secondary",
                                         "A probit model of challenge behaviour estimates how precisely each group reads the zone, and so how confident a player should be.")),
              div(strong("4 · Grade"), p(class = "small text-secondary",
                                         "Each decision is valued in runs from the count, outs and bases. Good decisions are graded, not lucky outcomes."))
            ),
            div(class = "small text-secondary",
                "Built with R, Shiny, bslib and plotly · ",
                tags$a(href = REPO_URL, target = "_blank", "Source on GitHub"))
          )
        )
    )
  ),
  
  nav_panel(
    "Who Sees the Zone", value = "zone",
    div(class = "py-3",
        h3(class = "mb-1", "Who sees the zone best?"),
        p(class = "text-secondary mb-3",
          "How far off can a pitch be before a player can tell? Estimated from 345k",
          " challenge decisions: how sharply each group's willingness to challenge",
          " rises as a missed call gets more obvious. The glowing band is where they",
          " can't tell ball from strike. ○ = a baseball, to scale."),
        layout_columns(
          col_widths = c(4, 4, 4),
          sigma_card("Catchers, catching"),
          sigma_card("Catchers, batting"),
          sigma_card("Other batters")
        ),
        layout_columns(
          col_widths = c(7, 5),
          card(
            card_header("The evidence: how often each group challenges"),
            plotlyOutput("read_curve", height = "420px"),
            p(class = "small text-secondary mb-0",
              "Every called pitch that went against a player who still had a challenge.",
              " Left of the line the umpire was right; right of it he missed. A sharper",
              " jump at the line means a sharper read.")
          ),
          card(
            card_header("So is it the player, or the view?"),
            uiOutput("zone_story")
          )
        )
    )
  ),
  
  nav_panel(
    "Brave · Reckless · Chicken · Lost", value = "quad",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("role", "Decision-maker",
                     c("Catchers" = "catcher", "Batters" = "batter")),
        selectInput("team", "Highlight team", TEAMS,
                    selected = if ("TOR" %in% TEAMS) "TOR" else TEAMS[1]),
        hr(),
        p(class = "small text-secondary",
          strong("Aggression:"), " challenges made ÷ challenges a league-average",
          " player would make on the same pitches."),
        p(class = "small text-secondary",
          strong("Selectivity:"), " expected runs gained per challenge, minus what",
          " a league-average player would get from the same opportunities."),
        p(class = "small text-secondary",
          "Players with 15+ challenges and 100+ opportunities.",
          " Click a dot to see that player's decisions.")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          full_screen = TRUE,
          card_header("Who challenges too much, too little, and too badly"),
          plotlyOutput("quadrant", height = "540px")
        ),
        card(
          full_screen = TRUE,
          card_header(textOutput("player_title", inline = TRUE)),
          card_body(
            fillable = FALSE,
            uiOutput("player_stats"),
            plotlyOutput("zone", height = "400px"),
            p(class = "small text-secondary mb-2",
              "Catcher's view. Heights rescaled to one standard zone so pitches",
              " to different batters line up. Click a pitch for details."),
            uiOutput("pitch_card")
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Should He Have Tapped?", value = "tap",
    layout_sidebar(
      sidebar = sidebar(
        width = 290,
        radioButtons("tap_call", "The umpire called it a…",
                     c("Strike (batter decides)" = "S", "Ball (catcher decides)" = "B")),
        div(class = "d-flex gap-3",
            selectInput("tap_balls", "Balls", 0:3, selected = 1, width = "90px"),
            selectInput("tap_strikes", "Strikes", 0:2, selected = 2, width = "90px")),
        radioButtons("tap_outs", "Outs", 0:2, selected = 1, inline = TRUE),
        checkboxGroupInput("tap_runners", "Runners on",
                           c("1st", "2nd", "3rd"), inline = TRUE),
        selectInput("tap_eyes", "Whose eyes?",
                    c("Realistic (whoever decides)" = "real",
                      "Give him a catcher's eye" = "catcher",
                      "Give him a batter's eye" = "batter")),
        hr(),
        p(class = "small text-secondary",
          "Click anywhere on the map to place the pitch. The ○ is a baseball, to scale.")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Where is a challenge worth it?"),
          plotlyOutput("tap_map", height = "560px"),
          p(class = "small text-secondary mb-0",
            "Teal: tapping gains runs on average. Pink: it costs runs. The dotted line is",
            " the tap line, where the expected gain is exactly zero. Change the count and",
            " watch it move.")
        ),
        card(
          card_header("The decision"),
          card_body(fillable = FALSE, uiOutput("tap_result"))
        )
      )
    )
  ),
  
  nav_panel(
    "Robbed", value = "robbed",
    layout_sidebar(
      sidebar = sidebar(
        width = 290,
        selectInput("rob_team", "Highlight team", ROB_TEAMS,
                    selected = if ("TOR" %in% ROB_TEAMS) "TOR" else ROB_TEAMS[1]),
        selectInput("rob_park", "Ballpark", c("All parks (generic field)" = "ALL", PARKS)),
        sliderInput("rob_xba", "Robbed = an out with xBA of at least",
                    min = 0.5, max = 0.95, value = 0.7, step = 0.05),
        hr(),
        p(class = "small text-secondary",
          strong("xBA"), " is Statcast's expected batting average for a ball hit at",
          " that exit velocity and launch angle: how often it falls for a hit",
          " league-wide. An out at .700 xBA is a hit 7 times in 10.")
      ),
      uiOutput("rob_tiles"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Where the hits went to die"),
          plotlyOutput("rob_spray", height = "520px", fill = FALSE),
          uiOutput("rob_spray_note")
        ),
        card(
          card_header("Most robbed balls of 2026"),
          # The spray chart sets this row's height. The table fills it and
          # scrolls; the absolute-positioned inner div adds no height of its own,
          # so a long table can't stretch the row (and leave the chart with a gap).
          card_body(
            padding = 0,
            div(style = "position:relative; flex:1 1 auto; min-height:520px;",
                div(style = "position:absolute; inset:0; overflow:auto; padding:1rem 1.25rem; color-scheme:dark;",
                    uiOutput("rob_table")))
          )
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Unluckiest hitters: hits below expected"),
          plotlyOutput("rob_luck", height = "620px"),
          uiOutput("rob_luck_note")
        ),
        card(
          card_header("Where hits go to die: robbed outs per 1,000 balls in play"),
          plotlyOutput("rob_parks", height = "620px"),
          p(class = "small text-secondary mb-0",
            "A rate, so busy and quiet parks compare fairly. Click a bar to see that park.")
        )
      )
    )
  )
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  observeEvent(input$go_zone, nav_select("nav", "zone"))
  observeEvent(input$go_quad, nav_select("nav", "quad"))
  observeEvent(input$go_tap,  nav_select("nav", "tap"))
  
  graded <- reactive({
    grades |>
      filter(graded, role == input$role) |>
      mutate(hl = team == input$team, agg_log2 = log2(aggression))
  })
  
  # Clicked player; falls back to the highlighted team's busiest challenger.
  clicked <- reactiveVal(NULL)
  observeEvent(list(input$role, input$team), clicked(NULL))
  click_key <- function(src) {
    ev  <- suppressWarnings(event_data("plotly_click", source = src))
    key <- suppressWarnings(as.integer(ev$key))
    if (length(key) == 1 && !is.na(key)) key else NULL
  }
  observeEvent(suppressWarnings(event_data("plotly_click", source = "quad")), {
    k <- click_key("quad")
    if (!is.null(k)) clicked(k)
  })
  
  sel_id <- reactive({
    g  <- graded()
    id <- clicked()
    if (length(id) != 1 || is.na(id) || !(id %in% g$decider_id)) {
      pool <- if (any(g$hl)) filter(g, hl) else g
      id <- pool$decider_id[which.max(pool$challenges)]
    }
    id
  })
  
  sel_player    <- reactive(filter(graded(), decider_id == sel_id()))
  sel_decisions <- reactive(filter(decisions, role == input$role, decider_id == sel_id()))
  
  # Clicked pitch; cleared whenever the player changes.
  clicked_pitch <- reactiveVal(NULL)
  observeEvent(sel_id(), clicked_pitch(NULL))
  observeEvent(suppressWarnings(event_data("plotly_click", source = "zone")), {
    k <- click_key("zone")
    if (!is.null(k)) clicked_pitch(k)
  })
  
  # -- Quadrant ---------------------------------------------------------------
  output$quadrant <- renderPlotly({
    g <- graded() |>
      mutate(hover = sprintf(
        "<b>%s</b> (%s) · %s<br>%d challenges, %.0f%% won<br>Aggression: %.2f× league<br>Selectivity: %+.3f runs / challenge",
        name, team, persona, challenges, 100 * overturn_rate, aggression, selectivity
      ))
    others <- filter(g, !hl)
    team_g <- filter(g, hl)
    
    corner <- function(x, y, label, xanchor, yanchor) {
      list(x = x, y = y, xref = "paper", yref = "paper", text = paste0("<b>", label, "</b>"),
           showarrow = FALSE, xanchor = xanchor, yanchor = yanchor,
           font = list(color = COL$text2, size = 13))
    }
    
    p <- plot_ly(source = "quad") |>
      add_markers(
        data = others, x = ~agg_log2, y = ~selectivity, key = ~decider_id,
        text = ~hover, hoverinfo = "text", name = "Other teams",
        marker = list(size = 9, color = COL$others,
                      line = list(color = COL$card, width = 2))
      )
    
    if (nrow(team_g) > 0) {
      p <- p |>
        # Soft neon halo behind the highlighted team (decoration only)
        add_markers(
          data = team_g, x = ~agg_log2, y = ~selectivity, key = ~decider_id,
          hoverinfo = "none", showlegend = FALSE,
          marker = list(size = 26, color = COL$neon_teal, opacity = 0.18, line = list(width = 0))
        ) |>
        add_markers(
          data = team_g, x = ~agg_log2, y = ~selectivity, key = ~decider_id,
          text = ~hover, hoverinfo = "text", name = input$team,
          marker = list(size = 13, color = COL$highlight,
                        line = list(color = COL$card, width = 2))
        )
    }
    # Name labels as annotations so they can sit clear of the selection ring
    name_labels <- lapply(seq_len(nrow(team_g)), \(i) list(
      x = team_g$agg_log2[i], y = team_g$selectivity[i],
      text = last_name(team_g$name[i]), showarrow = FALSE, yshift = 18,
      font = list(color = COL$text1, size = 11)
    ))
    
    sel <- filter(g, decider_id == sel_id())
    if (nrow(sel) > 0) {
      p <- p |> add_markers(
        data = sel, x = ~agg_log2, y = ~selectivity, key = ~decider_id,
        hoverinfo = "none", name = "Selected",
        marker = list(size = 22, symbol = "circle-open", color = COL$text1,
                      line = list(width = 2, color = COL$text1))
      )
    }
    
    p |>
      dark_layout(
        xaxis = list(title = "Aggression", tickvals = AGG_TICKS, ticktext = AGG_LABELS,
                     zeroline = FALSE, gridcolor = COL$grid),
        yaxis = list(title = "Selectivity (runs per challenge vs league)",
                     zeroline = FALSE, gridcolor = COL$grid),
        shapes = list(
          list(type = "line", x0 = 0, x1 = 0, yref = "paper", y0 = 0, y1 = 1,
               line = list(color = COL$text2, width = 1, dash = "dot")),
          list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
               line = list(color = COL$text2, width = 1, dash = "dot"))
        ),
        annotations = c(list(
          corner(0.99, 0.99, "BRAVE",    "right", "top"),
          corner(0.99, 0.01, "RECKLESS", "right", "bottom"),
          corner(0.01, 0.99, "CHICKEN",  "left",  "top"),
          corner(0.01, 0.01, "LOST",     "left",  "bottom")
        ), name_labels),
        legend = list(orientation = "h", x = 0, y = -0.14),
        margin = list(t = 10)
      ) |>
      config(displayModeBar = FALSE) |>
      event_register("plotly_click")
  })
  
  # -- Player panel -----------------------------------------------------------
  output$player_title <- renderText({
    s <- sel_player()
    if (nrow(s) == 0) return("Pick a player")
    sprintf("%s (%s): %s", s$name, s$team, s$persona)
  })
  
  output$player_stats <- renderUI({
    s <- sel_player()
    if (nrow(s) == 0) return(NULL)
    stat <- function(label, value) {
      div(class = "me-4 text-center",
          div(class = "small text-secondary", label),
          div(class = "stat-value", value))
    }
    div(class = "d-flex flex-wrap mb-2",
        stat("Challenges", s$challenges),
        stat("Won", sprintf("%.0f%%", 100 * s$overturn_rate)),
        stat("Should've, didn't", s$missed_good),
        stat("Runs lost / 100 opps", sprintf("%.2f", -s$regret_per_100)))
  })
  
  output$zone <- renderPlotly({
    d <- sel_decisions() |>
      mutate(
        outcome = factor(outcome, levels = c("Won", "Lost", "Missed")),
        abs_part = case_when(
          outcome == "Won"    ~ paste("ABS:", CALL_WORD[true_call], "(overturned)"),
          outcome == "Lost"   ~ paste("ABS:", CALL_WORD[true_call], "(call stands)"),
          true_call != original_call ~ paste("ABS would say:", CALL_WORD[true_call]),
          TRUE                ~ paste("ABS would say:", CALL_WORD[true_call], "(ump was right)")
        ),
        hover = sprintf(
          "<b>%s</b> · %s · %d-%d, %d out<br>Umpire: %s → %s<br>%s<br>P(win) %.0f%% · EV %+.2f runs · %s",
          outcome, format(game_date, "%b %d"), balls, strikes, outs_when_up,
          CALL_WORD[original_call], abs_part, bases_text(bases),
          100 * p_win, ev, verdict_text(as.character(outcome), ev, true_call != original_call)
        )
      )
    
    style <- list(
      Won    = list(color = COL$won,    symbol = "circle",      name = "Challenged: won"),
      Lost   = list(color = COL$lost,   symbol = "x",           name = "Challenged: lost"),
      Missed = list(color = COL$missed, symbol = "circle-open", name = "Should've challenged")
    )
    
    p <- plot_ly(source = "zone")
    for (o in levels(d$outcome)) {
      dd <- filter(d, outcome == o)
      if (nrow(dd) == 0) next
      p <- p |> add_markers(
        data = dd, x = ~plate_x, y = ~z_std, key = ~pitch_id,
        text = ~hover, hoverinfo = "text", name = style[[o]]$name,
        # Tooltip border matches the outcome (teal / pink / purple)
        hoverlabel = list(bordercolor = style[[o]]$color),
        marker = list(size = 9, color = style[[o]]$color, symbol = style[[o]]$symbol,
                      line = list(width = 2, color = style[[o]]$color))
      )
    }
    
    picked <- filter(d, pitch_id %in% clicked_pitch())
    if (nrow(picked) > 0) {
      p <- p |> add_markers(
        data = picked, x = ~plate_x, y = ~z_std, key = ~pitch_id,
        hoverinfo = "none", showlegend = FALSE,
        marker = list(size = 22, symbol = "circle-open", color = COL$text1,
                      line = list(width = 2, color = COL$text1))
      )
    }
    
    p |>
      dark_layout(
        xaxis = list(range = c(-1.7, 1.7), title = "", zeroline = FALSE,
                     showgrid = FALSE, showticklabels = FALSE,
                     constrain = "domain", fixedrange = TRUE),
        yaxis = list(range = c(0.6, 4.4), title = "", zeroline = FALSE,
                     showgrid = FALSE, showticklabels = FALSE,
                     scaleanchor = "x", scaleratio = 1,
                     constrain = "domain", fixedrange = TRUE),
        shapes = list(
          list(type = "rect", x0 = -ZONE_HALF_FT, x1 = ZONE_HALF_FT, y0 = 1.5, y1 = 3.5,
               line = list(color = COL$neon_teal, width = 8), opacity = 0.15),
          list(type = "rect", x0 = -ZONE_HALF_FT, x1 = ZONE_HALF_FT, y0 = 1.5, y1 = 3.5,
               line = list(color = COL$text1, width = 2))
        ),
        legend = list(orientation = "h", x = 0, y = -0.02),
        margin = list(t = 5, b = 5, l = 5, r = 5)
      ) |>
      config(displayModeBar = FALSE) |>
      event_register("plotly_click")
  })
  
  # -- Should He Have Tapped? -------------------------------------------------
  tap_pt <- reactiveVal(list(x = 0.80, z = 2.50))     # just off the corner
  observeEvent(suppressWarnings(event_data("plotly_click", source = "tap")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "tap"))
    if (length(ev$x) == 1 && !is.na(ev$x)) tap_pt(list(x = ev$x, z = ev$y))
  })
  
  tap_state <- reactive({
    bs   <- paste0(as.integer(c("1st", "2nd", "3rd") %in% input$tap_runners), collapse = "")
    v    <- lookup_value(as.integer(input$tap_balls), as.integer(input$tap_strikes),
                         as.integer(input$tap_outs), bs)
    role <- if (input$tap_call == "S") "batter" else "catcher"
    sigma <- switch(input$tap_eyes, real = SIGMA_ROLE[[role]],
                    catcher = SIGMA_ROLE[["catcher"]], batter = SIGMA_ROLE[["batter"]])
    list(call = input$tap_call, role = role, sigma = sigma, v = v, bases = bs,
         balls = as.integer(input$tap_balls), strikes = as.integer(input$tap_strikes),
         outs = as.integer(input$tap_outs),
         breakeven = CHALLENGE_VALUE_RUNS / (CHALLENGE_VALUE_RUNS + v))
  })
  
  output$tap_map <- renderPlotly({
    st <- tap_state()
    pt <- tap_pt()
    grid <- expand.grid(x = TAP_XS, z = TAP_ZS)
    r    <- tap_eval(grid$x, grid$z, st)
    ev_m <- matrix(r$ev, nrow = length(TAP_ZS), byrow = TRUE)   # rows = z
    hov  <- matrix(sprintf("EV %+.2f runs<br>P(win) %.0f%%", r$ev, 100 * r$p),
                   nrow = length(TAP_ZS), byrow = TRUE)
    lim  <- max(CHALLENGE_VALUE_RUNS, st$v)
    br   <- (2.9 / 2) / 12                                       # ball radius (ft)
    
    # Tooltip border = the heatmap's colour at that cell. Same pink -> teal ramp,
    # but the midpoint is the light text colour instead of the card colour, so the
    # border stays visible on near-zero (toss-up) cells.
    ramp   <- colorRamp(c(COL$lost, COL$text2, COL$won))
    t01    <- (pmin(pmax(r$ev, -lim), lim) + lim) / (2 * lim)
    border <- matrix(rgb(ramp(t01), maxColorValue = 255),
                     nrow = length(TAP_ZS), byrow = TRUE)
    
    plot_ly(source = "tap") |>
      add_heatmap(
        x = TAP_XS, y = TAP_ZS, z = ev_m, text = hov, hoverinfo = "text",
        hoverlabel = list(bordercolor = border),   # 2-D, one colour per cell
        zmin = -lim, zmax = lim, zmid = 0,
        # Diverging: pink (costs runs) -> neutral -> teal (gains runs)
        colorscale = list(c(0, COL$lost), c(0.5, COL$card), c(1, COL$won)),
        colorbar = list(title = list(text = "EV (runs)", font = list(color = COL$text2)),
                        tickfont = list(color = COL$text2), outlinewidth = 0, len = 0.6)
      ) |>
      add_contour(
        x = TAP_XS, y = TAP_ZS, z = ev_m, showscale = FALSE, hoverinfo = "skip",
        contours = list(start = 0, end = 0, size = 1, coloring = "none"),
        line = list(color = COL$text1, width = 2, dash = "dot"), name = "Tap line"
      ) |>
      dark_layout(
        xaxis = list(range = range(TAP_XS), title = "", showgrid = FALSE, zeroline = FALSE,
                     showticklabels = FALSE, constrain = "domain", fixedrange = TRUE),
        yaxis = list(range = range(TAP_ZS), title = "", showgrid = FALSE, zeroline = FALSE,
                     showticklabels = FALSE, scaleanchor = "x", scaleratio = 1,
                     constrain = "domain", fixedrange = TRUE),
        shapes = list(
          list(type = "rect", x0 = -ZONE_HALF_FT, x1 = ZONE_HALF_FT, y0 = STD_BOT, y1 = STD_TOP,
               line = list(color = COL$text1, width = 2)),
          list(type = "circle", x0 = pt$x - br, x1 = pt$x + br, y0 = pt$z - br, y1 = pt$z + br,
               line = list(color = COL$text1, width = 2), fillcolor = "rgba(236,233,255,0.25)")
        ),
        showlegend = FALSE, margin = list(t = 5, b = 5, l = 5, r = 5)
      ) |>
      config(displayModeBar = FALSE) |>
      event_register("plotly_click")
  })
  
  output$tap_result <- renderUI({
    st <- tap_state(); pt <- tap_pt()
    r  <- tap_eval(pt$x, pt$z, st)
    tossup <- abs(r$ev) < 0.005                  # within half a hundredth of a run
    tap   <- r$ev > 0
    truth <- abs_call(pt$x, pt$z, STD_TOP, STD_BOT)
    wins  <- truth != st$call
    col   <- if (tossup) COL$text2 else if (tap) COL$won else COL$lost
    who   <- if (st$role == "batter") "The batter" else "The catcher"
    eyes  <- if (input$tap_eyes == "real") "" else
      sprintf(" (with a %s's eye)", if (input$tap_eyes == "catcher") "catcher" else "batter")
    
    edge_txt <- if (r$edge > 0) sprintf("The ball misses the zone by %.1f in.", r$edge)
    else sprintf("The ball clips the zone by %.1f in.", -r$edge)
    
    bar <- div(
      style = sprintf("position:relative; height:14px; border-radius:7px; background:%s; border:1px solid %s;",
                      COL$surface, COL$border),
      div(style = sprintf("width:%.1f%%; height:100%%; border-radius:7px; background:%s; box-shadow:0 0 12px %s;",
                          100 * r$p, col, col)),
      div(style = sprintf("position:absolute; left:%.1f%%; top:-6px; width:2px; height:24px; background:%s;",
                          100 * st$breakeven, COL$text1))
    )
    
    tagList(
      div(class = "stat-value",
          style = sprintf("font-size:3.4rem; color:%s; text-shadow:0 0 18px %s; line-height:1;", col, col),
          if (tossup) "TOSS-UP" else if (tap) "TAP IT" else "HOLD"),
      p(class = "mt-2",
        if (tossup) sprintf("Either choice is fine%s: the expected gain is essentially zero. %s", eyes, edge_txt)
        else sprintf("%s%s should %s. %s", who, eyes,
                     if (tap) "challenge" else "keep the challenge in his pocket", edge_txt)),
      
      div(class = "d-flex justify-content-between small mt-3 mb-1",
          span(sprintf("Chance of winning: %.0f%%", 100 * r$p)),
          span(class = "text-secondary", sprintf("needs %.0f%%", 100 * st$breakeven))),
      bar,
      p(class = "small text-secondary mt-1",
        "The white tick is the breakeven: the confidence needed for a challenge to pay off."),
      
      div(class = "d-flex align-items-center gap-4 flex-wrap my-3",
          div(class = "text-center", diamond_svg(st$bases),
              div(class = "small text-secondary", bases_text(st$bases))),
          div(div(class = "fs-4 fw-semibold", sprintf("%d-%d", st$balls, st$strikes)),
              div(class = "d-flex align-items-center gap-2",
                  outs_dots(st$outs),
                  span(class = "small text-secondary", sprintf("%d out", st$outs))))),
      
      tags$table(
        class = "table table-sm mb-3",
        style = sprintf("--bs-table-bg: transparent; color:%s;", COL$text1),
        tags$tbody(
          tags$tr(tags$td(class = "text-secondary", "Flipping this call is worth"),
                  tags$td(class = "text-end", sprintf("%.2f runs", st$v))),
          tags$tr(tags$td(class = "text-secondary", "A lost challenge costs"),
                  tags$td(class = "text-end", sprintf("%.2f runs", CHALLENGE_VALUE_RUNS))),
          tags$tr(tags$td(class = "text-secondary", sprintf("%s's read error", if (input$tap_eyes == "batter" || (input$tap_eyes == "real" && st$role == "batter")) "Batter" else "Catcher")),
                  tags$td(class = "text-end", sprintf("±%.1f in", st$sigma))),
          tags$tr(tags$td(strong("Expected value of tapping")),
                  tags$td(class = "text-end", strong(sprintf("%+.3f runs", r$ev))))
        )
      ),
      
      div(class = "rounded p-2 small",
          style = sprintf("border:1px dashed %s;", COL$border),
          span(class = "text-secondary", "What ABS would say (the player can't see this): "),
          strong(CALL_WORD[[truth]]), " · a challenge would ",
          strong(if (wins) "WIN" else "LOSE"),
          if (!tossup && tap != wins) span(class = "text-secondary",
                                           if (tap) " (still the right call: good decisions can lose)"
                                           else " (still right to hold: too risky to know)"))
    )
  })
  
  # -- Robbed -----------------------------------------------------------------
  rob <- reactive({
    r <- filter(robbed, xba >= input$rob_xba)
    if (input$rob_park != "ALL") r <- filter(r, park == input$rob_park)
    r
  })
  
  # Click a park's bar -> show that park
  observeEvent(suppressWarnings(event_data("plotly_click", source = "parks")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "parks"))
    k  <- as.character(ev$y)
    if (length(k) == 1 && k %in% PARKS) updateSelectInput(session, "rob_park", selected = k)
  })
  
  output$rob_tiles <- renderUI({
    r    <- rob()
    hard <- slice_max(r, ev, n = 1, with_ties = FALSE)
    worst <- slice_min(luck, luck, n = 1, with_ties = FALSE)
    team_n <- sum(r$team == input$rob_team)
    where  <- if (input$rob_park == "ALL") "" else sprintf(" at %s's park", input$rob_park)
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      stat_tile(comma(nrow(r)), paste0("robbed outs", where),
                sprintf("xBA %.2f or higher", input$rob_xba), COL$lost),
      stat_tile(comma(team_n), sprintf("%s hitters robbed%s", input$rob_team, where),
                if (input$rob_park == "ALL")
                  sprintf("%.1f%% of all robbed outs (avg team: %.1f%%)",
                          100 * team_n / max(nrow(r), 1), 100 / length(ROB_TEAMS))
                else
                  sprintf("%.1f%% of the robbed outs at this park", 100 * team_n / max(nrow(r), 1)),
                COL$won),
      if (nrow(hard) == 1)
        stat_tile(sprintf("%.1f", hard$ev), "mph: hardest-hit out",
                  sprintf("%s, %s", hard$name, format(hard$game_date, "%b %d")), COL$missed)
      else stat_tile("–", "mph: hardest-hit out", "none at this threshold", COL$missed),
      stat_tile(sprintf("%.1f", worst$luck), "hits below expected",
                sprintf("%s (%s): unluckiest hitter", worst$name, worst$team), COL$lost)
    )
  })
  
  output$rob_spray <- renderPlotly({
    r  <- rob() |>
      mutate(hl = team == input$rob_team,
             hover = sprintf("<b>%s</b> (%s) vs %s · %s<br>%.1f mph, %d°, %s ft<br>xBA %.3f → %s",
                             name, team, opp, format(game_date, "%b %d"),
                             ev, as.integer(round(la)),
                             ifelse(is.na(dist), "?", as.character(dist)), xba, result))
    xy <- spray_xy(r$hc_x, r$hc_y); r$x <- xy$x; r$y <- xy$y
    others <- filter(r, !hl); team_r <- filter(r, hl)
    
    # Real parks extend behind home plate (backstop, foul territory);
    # the generic field doesn't, so it gets a tighter view
    generic <- input$rob_park == "ALL"
    xr <- if (generic) c(-310, 310) else c(-330, 330)
    yr <- if (generic) c(-25, 450)  else c(-90, 470)
    
    plot_ly() |>
      park_traces(input$rob_park) |>
      add_markers(data = others, x = ~x, y = ~y, text = ~hover, hoverinfo = "text",
                  name = "Other teams",
                  marker = list(size = 6, color = COL$others, opacity = 0.7,
                                line = list(width = 0)),
                  hoverlabel = list(bordercolor = COL$text2)) |>
      add_markers(data = team_r, x = ~x, y = ~y, text = ~hover, hoverinfo = "text",
                  name = input$rob_team,
                  marker = list(size = 9, color = COL$highlight,
                                line = list(color = COL$card, width = 1.5)),
                  hoverlabel = list(bordercolor = COL$highlight)) |>
      dark_layout(
        xaxis = list(range = xr, showgrid = FALSE, zeroline = FALSE,
                     showticklabels = FALSE, title = "", fixedrange = TRUE,
                     constrain = "domain"),
        yaxis = list(range = yr, showgrid = FALSE, zeroline = FALSE,
                     showticklabels = FALSE, title = "", fixedrange = TRUE,
                     scaleanchor = "x", scaleratio = 1, constrain = "domain"),
        legend = list(orientation = "h", x = 0, y = -0.02),
        margin = list(t = 5, b = 5, l = 5, r = 5)
      ) |>
      config(displayModeBar = FALSE)
  })
  
  output$rob_spray_note <- renderUI({
    pk <- input$rob_park
    has_outline <- pk != "ALL" && pk %in% stadiums$park
    p(class = "small text-secondary mb-0",
      if (pk == "ALL") "Every robbed ball of the season on a generic field. Pick a ballpark to see its real walls."
      else if (has_outline) sprintf("Robbed balls hit at %s's park, on its real outline.", pk)
      else sprintf("No outline for %s's park, so it's shown on a generic field.", pk),
      if (pk %in% names(PARK_CAVEAT)) span(" ", PARK_CAVEAT[[pk]]),
      " Outlines: GeomMLBStadiums (Ben Dilday). Distances approximate.")
  })
  
  output$rob_parks <- renderPlotly({
    counts <- robbed |> filter(xba >= input$rob_xba) |> count(park, name = "robbed")
    d <- park_bbe |>
      left_join(counts, by = "park") |>
      mutate(robbed = coalesce(robbed, 0L), rate = 1000 * robbed / bbe,
             hl = park == input$rob_park,
             hover = sprintf("<b>%s</b> home park<br>%s robbed of %s balls in play<br>%.1f per 1,000",
                             park, comma(robbed), comma(bbe), rate)) |>
      arrange(rate)
    d$park <- factor(d$park, levels = d$park)
    
    bar_col <- ifelse(d$hl, COL$highlight, COL$others)
    plot_ly(d, x = ~rate, y = ~park, type = "bar", orientation = "h", source = "parks",
            text = ~hover, hoverinfo = "text", textposition = "none",
            marker = list(color = bar_col),
            hoverlabel = list(bordercolor = bar_col)) |>   # tooltip border = bar colour
      dark_layout(
        xaxis = list(title = "Robbed outs per 1,000 balls in play", gridcolor = COL$grid,
                     zeroline = FALSE),
        yaxis = list(title = "", tickfont = list(color = COL$text1, size = 11)),
        showlegend = FALSE, margin = list(t = 10, l = 10)
      ) |>
      config(displayModeBar = FALSE) |>
      event_register("plotly_click")
  })
  
  output$rob_table <- renderUI({
    top <- rob() |> arrange(desc(xba), desc(ev)) |> head(25)
    rows <- lapply(seq_len(nrow(top)), \(i) {
      t  <- top[i, ]
      hl <- t$team == input$rob_team
      tags$tr(
        style = if (hl) sprintf("box-shadow: inset 3px 0 0 %s;", COL$highlight) else NULL,
        tags$td(style = "white-space:nowrap;", format(t$game_date, "%b %d")),
        tags$td(strong(t$name), span(class = "text-secondary", sprintf(" %s", t$team))),
        tags$td(class = "text-end", sprintf("%.1f", t$ev)),
        tags$td(class = "text-end", sprintf("%d°", as.integer(round(t$la)))),
        tags$td(class = "text-end", ifelse(is.na(t$dist), "?", t$dist)),
        tags$td(class = "text-end", strong(sprintf(".%03d", round(1000 * t$xba)))),
        tags$td(class = "text-secondary", t$result)
      )
    })
    tags$table(
      class = "table table-sm mb-0 rob-table",
      style = sprintf("--bs-table-bg: transparent; color:%s; white-space:nowrap;", COL$text1),
      tags$thead(tags$tr(tags$th("Date"), tags$th("Batter"),
                         tags$th(class = "text-end", title = "Exit velocity (mph)", "EV"),
                         tags$th(class = "text-end", title = "Launch angle", "LA"),
                         tags$th(class = "text-end", title = "Distance (ft)", "Dist"),
                         tags$th(class = "text-end", "xBA"),
                         tags$th("Result"))),
      tags$tbody(rows)
    ) |> tagList(
      div(class = "text-secondary small mt-2",
          "EV = exit velocity (mph) · LA = launch angle · Dist in feet")
    )
  })
  
  output$rob_luck <- renderPlotly({
    worst <- luck |>
      slice_min(luck, n = 12, with_ties = FALSE) |>
      mutate(hl = team == input$rob_team,
             label = sprintf("%s (%s)", name, team),
             hover = sprintf("<b>%s</b> (%s)<br>%d batted balls<br>%d hits vs %.1f expected<br>Avg on contact .%03d vs xBA .%03d",
                             name, team, bbe, hits, x_hits,
                             round(1000 * bacon), round(1000 * xbacon)))
    worst$label <- factor(worst$label, levels = rev(worst$label))
    
    bar_col <- ifelse(worst$hl, COL$highlight, COL$lost)
    plot_ly(worst, y = ~label, x = ~luck, type = "bar", orientation = "h",
            text = ~hover, hoverinfo = "text", textposition = "none",
            marker = list(color = bar_col),
            hoverlabel = list(bordercolor = bar_col)) |>   # tooltip border = bar colour
      dark_layout(
        xaxis = list(title = "Hits minus expected hits (season)", zeroline = TRUE,
                     zerolinecolor = COL$text2, gridcolor = COL$grid),
        yaxis = list(title = "", tickfont = list(color = COL$text1)),
        showlegend = FALSE, margin = list(t = 10, l = 10)
      ) |>
      config(displayModeBar = FALSE)
  })
  
  output$rob_luck_note <- renderUI({
    t <- luck |> filter(team == input$rob_team) |> slice_min(luck, n = 1, with_ties = FALSE)
    p(class = "small text-secondary mb-0",
      "Hitters with 200+ batted balls. Expected hits = the sum of xBA over every ball in play.",
      if (nrow(t) == 1) sprintf(" Unluckiest %s hitter: %s, %.1f hits below expected.",
                                input$rob_team, t$name, t$luck))
  })
  
  # -- Who Sees the Zone -------------------------------------------------------
  output$read_curve <- renderPlotly({
    p <- plot_ly()
    for (g in names(GROUP_STYLE)) {
      st <- GROUP_STYLE[[g]]
      # Build hover text NOW: plotly evaluates ~formulas lazily, so a formula
      # referencing the loop variable `st` would see only its LAST value.
      d <- filter(read_curves, group == g) |>
        mutate(hover = sprintf(
          "<b>%s</b><br>%s<br>%.0f%% challenged (n = %s)",
          st$short,
          if_else(bin < 0, sprintf("Ump right by %.1f in", -bin),
                  if_else(bin > 0, sprintf("Ump wrong by %.1f in", bin),
                          "Right on the edge")),
          100 * challenge_rate, formatC(n, format = "d", big.mark = ",")  # no padding
        ))
      p <- p |> add_trace(
        data = d, x = ~bin, y = ~challenge_rate, type = "scatter", mode = "lines+markers",
        name = st$short,
        line = list(color = st$col, width = 2),
        marker = list(color = st$col, size = 8, symbol = st$symbol,
                      line = list(color = COL$card, width = 2)),
        text = ~hover,
        hoverinfo = "text", hoverlabel = list(bordercolor = st$col)
      )
    }
    p |>
      dark_layout(
        xaxis = list(title = "← umpire right      inches from the zone edge      umpire wrong →",
                     zeroline = FALSE, gridcolor = COL$grid, dtick = 1),
        yaxis = list(title = "Share of opportunities challenged", tickformat = ".0%",
                     zeroline = FALSE, gridcolor = COL$grid, rangemode = "tozero"),
        shapes = list(list(type = "line", x0 = 0, x1 = 0, yref = "paper", y0 = 0, y1 = 1,
                           line = list(color = COL$text2, width = 1, dash = "dot"))),
        legend = list(orientation = "h", x = 0, y = -0.22),
        margin = list(t = 10)
      ) |>
      config(displayModeBar = FALSE)
  })
  
  output$zone_story <- renderUI({
    s_cc <- sig("Catchers, catching")
    s_cb <- sig("Catchers, batting")
    s_ob <- sig("Other batters")
    gap_total   <- s_ob - s_cc
    kept_share  <- (s_ob - s_cb) / gap_total     # share of the catcher edge kept at bat
    
    verdict <- if (kept_share < 0.25) {
      "It's the view."
    } else if (kept_share > 0.75) {
      "It's the player."
    } else {
      "A bit of both."
    }
    # If the confidence intervals overlap, a point-estimate % would overclaim.
    cb_row <- sigma_groups[sigma_groups$group == "Catchers, batting", ]
    ob_row <- sigma_groups[sigma_groups$group == "Other batters", ]
    overlap <- cb_row$sigma_lo <= ob_row$sigma_hi && ob_row$sigma_lo <= cb_row$sigma_hi
    explain <- if (overlap) {
      "Statistically indistinguishable from other hitters: their edge doesn't come with them."
    } else {
      sprintf("Catchers keep about %.0f%% of their edge when they step into the box.",
              100 * max(min(kept_share, 1), 0))
    }
    
    cb <- grades |>
      filter(role == "batter", challenges > 0) |>
      group_by(is_catcher) |>
      summarise(won = sum(wins) / sum(challenges),
                pos_ev = 1 - sum(bad_challenges) / sum(challenges),
                .groups = "drop")
    pct <- \(flag, col) sprintf("%.0f%%", 100 * cb[[col]][cb$is_catcher == flag])
    
    row <- function(label, a, b) tags$tr(tags$td(class = "text-secondary", label),
                                         tags$td(class = "text-center", a),
                                         tags$td(class = "text-center", b))
    tagList(
      div(class = "stat-value", style = sprintf("font-size:2rem; color:%s;", COL$text1), verdict),
      p(sprintf("From behind the plate, catchers read the zone to ±%.1f in, about %.1f× sharper than hitters (±%.1f in).",
                s_cc, s_ob / s_cc, s_ob)),
      p(sprintf("The same catchers at the plate: ±%.1f in. %s", s_cb, explain)),
      p(class = "text-secondary",
        "Behind the plate a catcher sees the pitch arrive head-on, all the way into his glove.",
        " A hitter sees it from the side, while deciding whether to swing."),
      tags$table(
        class = "table table-sm mt-2 mb-0", style = sprintf("--bs-table-bg: transparent; color:%s;", COL$text1),
        tags$thead(tags$tr(tags$th(""), tags$th(class = "text-center", "Catchers at bat"),
                           tags$th(class = "text-center", "Other batters"))),
        tags$tbody(
          row("Challenges won", pct(TRUE, "won"), pct(FALSE, "won")),
          row("Challenges that were good bets", pct(TRUE, "pos_ev"), pct(FALSE, "pos_ev"))
        )
      )
    )
  })
  
  # -- Pitch card (click a pitch) ---------------------------------------------
  output$pitch_card <- renderUI({
    id <- clicked_pitch()
    if (is.null(id)) return(NULL)
    pt <- filter(sel_decisions(), pitch_id == id)
    if (nrow(pt) == 0) return(NULL)
    
    result <- switch(pt$outcome,
                     Won    = "✓ Overturned",
                     Lost   = "✗ Call stands",
                     Missed = if (pt$true_call != pt$original_call) "Not challenged: would have won"
                     else "Not challenged: ump was right")
    
    look <- switch(pt$outcome,
                   Won    = list(col = COL$won,    icon = "●", label = "Challenged: won"),
                   Lost   = list(col = COL$lost,   icon = "✖", label = "Challenged: lost"),
                   Missed = list(col = COL$missed, icon = "○", label = "Should've challenged"))
    
    div(
      class = "rounded p-3",
      style = sprintf("border:2px solid %s; box-shadow: 0 0 18px %s40;", look$col, look$col),
      div(class = "mb-2",
          span(class = "px-2 py-1 rounded fw-semibold small",
               style = sprintf("color:%s; border:1px solid %s; letter-spacing:.06em; text-transform:uppercase;",
                               look$col, look$col),
               paste(look$icon, look$label))),
      div(class = "d-flex align-items-center gap-4 flex-wrap",
          div(class = "text-center",
              diamond_svg(pt$bases),
              div(class = "small text-secondary", bases_text(pt$bases))),
          div(
            div(class = "fs-4 fw-semibold", sprintf("%d-%d", pt$balls, pt$strikes)),
            div(class = "d-flex align-items-center gap-2",
                outs_dots(pt$outs_when_up),
                span(class = "small text-secondary", sprintf("%d out", pt$outs_when_up))),
            div(class = "small text-secondary mt-1", format(pt$game_date, "%b %d, %Y"))
          ),
          div(class = "d-flex align-items-center gap-2",
              call_badge("Umpire", pt$original_call),
              span(class = "fs-4 text-secondary",
                   if (pt$true_call != pt$original_call) "→" else "="),
              call_badge("ABS", pt$true_call,
                         emphasis = pt$true_call != pt$original_call))
      ),
      div(class = "mt-2",
          strong(result), " · ",
          sprintf("P(win) %.0f%%, worth %.2f runs, EV %+.2f runs: %s",
                  100 * pt$p_win, pt$overturn_value, pt$ev,
                  verdict_text(pt$outcome, pt$ev, pt$true_call != pt$original_call)))
    )
  })
}

shinyApp(ui, server)