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

coming_soon <- function(title, text) {
  card(card_header(title), p(class = "text-secondary", text))
}

# ---- UI ---------------------------------------------------------------------

ui <- page_navbar(
  title = "Tap or Don't Tap: MLB ABS Challenges, 2026",
  fillable = FALSE,   # let the page scroll; never squash a card's plot
  theme = bs_theme(
    version = 5, bg = COL$surface, fg = COL$text1,
    primary = COL$highlight, secondary = COL$text2,
    base_font = font_google("Inter"),
    heading_font = font_google("Rajdhani")
  ) |> bs_add_rules(NEON_CSS),
  
  nav_panel("Headline",
            coming_soon("Coming next", "Players let 3 of 4 winnable challenges go.")),
  
  nav_panel("Who Sees the Zone",
            coming_soon("Coming next", "Catchers read the zone to ~2 inches; batters ~3.5.")),
  
  nav_panel(
    "Brave · Reckless · Chicken · Lost",
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
  
  nav_panel("Should He Have Tapped?",
            coming_soon("Coming next", "Drag a pitch around the zone: P(win), breakeven and EV live.")),
  
  nav_panel("Robbed",
            coming_soon("Coming next", "The unluckiest hard-hit balls of 2026."))
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {
  
  graded <- reactive({
    grades |>
      filter(graded, role == input$role) |>
      mutate(hl = team == input$team, agg_log2 = log2(aggression))
  })
  
  # Clicked player; falls back to the highlighted team's busiest challenger.
  clicked <- reactiveVal(NULL)
  observeEvent(list(input$role, input$team), clicked(NULL))
  observeEvent(event_data("plotly_click", source = "quad"), {
    clicked(as.integer(event_data("plotly_click", source = "quad")$key))
  })
  
  sel_id <- reactive({
    g  <- graded()
    id <- clicked()
    if (is.null(id) || !(id %in% g$decider_id)) {
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
  observeEvent(event_data("plotly_click", source = "zone"), {
    clicked_pitch(as.integer(event_data("plotly_click", source = "zone")$key))
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
          data = team_g, x = ~agg_log2, y = ~selectivity, hoverinfo = "none",
          showlegend = FALSE,
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
        data = sel, x = ~agg_log2, y = ~selectivity, hoverinfo = "none",
        name = "Selected",
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
      config(displayModeBar = FALSE)
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
        data = picked, x = ~plate_x, y = ~z_std, hoverinfo = "none",
        showlegend = FALSE,
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
      config(displayModeBar = FALSE)
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