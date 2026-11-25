library(shiny)
library(tidyverse)
library(nflreadr)

SEASON    <- 2026
POSITIONS <- c("QB", "RB", "WR", "TE", "K", "P")

# Every stat used in the scoring formula. Any column missing from the
# nflreadr data for a given season gets created as 0 so the formula never errors.
STAT_COLS <- c(
  "attempts", "completions", "passing_yards", "passing_tds",
  "passing_2pt_conversions", "passing_interceptions", "sacks_suffered",
  "carries", "rushing_yards", "rushing_tds", "rushing_2pt_conversions",
  "targets", "receptions", "receiving_yards", "receiving_tds",
  "receiving_2pt_conversions", "fumbles_total", "fumbles_lost_total",
  "fg_missed", "fg_missed_0_19", "fg_missed_20_29", "fg_missed_30_39",
  "fg_missed_40_49", "fg_missed_50_59", "fg_missed_60_", "pat_missed",
  "pt_return_tds", "pt_blocked"
)

# ---- Data ------------------------------------------------------------------

stats <- load_player_stats(seasons = SEASON)

# fumbles_total may not exist; build it from components if needed
if (!"fumbles_total" %in% names(stats)) {
  fum_cols <- intersect(
    c("sack_fumbles", "rushing_fumbles", "receiving_fumbles"), names(stats)
  )
  stats$fumbles_total <- if (length(fum_cols)) rowSums(stats[fum_cols], na.rm = TRUE) else 0
}
if (!"fumbles_lost_total" %in% names(stats)) {
  lost_cols <- intersect(
    c("sack_fumbles_lost", "rushing_fumbles_lost", "receiving_fumbles_lost"),
    names(stats)
  )
  stats$fumbles_lost_total <- if (length(lost_cols)) rowSums(stats[lost_cols], na.rm = TRUE) else 0
}

for (col in STAT_COLS) {
  if (!col %in% names(stats)) stats[[col]] <- 0
}

games <- load_schedules(seasons = SEASON) %>%
  select(game_id, home_team, result) %>%
  filter(!is.na(result))

scored <- stats %>%
  inner_join(games, by = "game_id") %>%
  mutate(
    across(all_of(STAT_COLS), ~ replace_na(.x, 0)),
    loss = as.integer((team == home_team & result < 0) |
                        (team != home_team & result > 0)),
    evil_pts =
      (attempts - completions) - 0.04 * passing_yards - 4 * passing_tds -
      passing_2pt_conversions + 10 * passing_interceptions + 2 * sacks_suffered +
      carries - 0.25 * rushing_yards - 2 * rushing_tds - rushing_2pt_conversions +
      targets - receptions - 0.05 * receiving_yards - 2 * receiving_tds -
      receiving_2pt_conversions + 5 * fumbles_total + 10 * fumbles_lost_total +
      fg_missed + 10 * fg_missed_0_19 + 10 * fg_missed_20_29 +
      10 * fg_missed_30_39 + 10 * fg_missed_40_49 + 5 * fg_missed_50_59 +
      3 * fg_missed_60_ + 5 * pat_missed - 6 * pt_return_tds + loss +
      25 * pt_blocked
  ) %>%
  filter(position %in% POSITIONS)

latest_week <- max(scored$week, na.rm = TRUE)

# ---- UI --------------------------------------------------------------------

position_tab <- function(pos) {
  tabPanel(
    pos,
    br(),
    fluidRow(
      column(6, h4("Top 5 Single Games"), tableOutput(paste0("single_", pos))),
      column(6, h4("Top 5 Season Totals"), tableOutput(paste0("total_", pos)))
    )
  )
}

ui <- navbarPage(
  title = paste0("Evil Fantasy Football ", SEASON),
  id = "page",
  
  # Controls shown above every page
  header = div(
    style = "padding: 0 20px 10px 20px;",
    checkboxInput("include_post", "Include playoff games", value = FALSE),
    helpText(paste0("Data through week ", latest_week, ". Higher score = more evil."))
  ),
  
  # Page 1: everyone, all positions combined
  tabPanel(
    "Award Race",
    fluidRow(
      column(6, h4("Rex Grossman Performance of the Year (POY)"), tableOutput("single_all")),
      column(6, h4("Jon Kitna Most Villianous Player (MVP)"), tableOutput("total_all"))
    )
  ),
  
  # Page 2: broken out by position
  tabPanel(
    "Position Leaders",
    do.call(tabsetPanel, c(list(id = "pos_tabs"), lapply(POSITIONS, position_tab)))
  )
)

# ---- Server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  filtered <- reactive({
    if (isTRUE(input$include_post)) scored else filter(scored, season_type == "REG")
  })
  
  # ---- Overall (all positions) ----
  output$single_all <- renderTable({
    filtered() %>%
      arrange(desc(evil_pts)) %>%
      slice_head(n = 5) %>%
      transmute(
        Rank   = row_number(),
        Player = player_display_name,
        Pos    = position,
        Team   = team,
        Week   = week,
        `Points` = round(evil_pts, 1)
      )
  }, digits = 1)
  
  output$total_all <- renderTable({
    filtered() %>%
    group_by(player_id, player_display_name) %>%
      summarise(
        Team  = last(team),
        Games = n(),
        Total = sum(evil_pts),
        .groups = "drop"
      ) %>%
      arrange(desc(Total)) %>%
      slice_head(n = 5) %>%
      transmute(
        Rank   = row_number(),
        Player = player_display_name,
        Team,
        Games,
        `Points` = round(Total, 1)
      )
  }, digits = 1)

  # ---- By position ----
  for (pos in POSITIONS) {
    local({
      p <- pos
      
      output[[paste0("single_", p)]] <- renderTable({
        filtered() %>%
          filter(position == p) %>%
          arrange(desc(evil_pts)) %>%
          slice_head(n = 5) %>%
          transmute(
            Rank   = row_number(),
            Player = player_display_name,
            Team   = team,
            Week   = week,
            `Points` = round(evil_pts, 1)
          )
      }, digits = 1)
      
      output[[paste0("total_", p)]] <- renderTable({
        filtered() %>%
          filter(position == p) %>%
          group_by(player_id, player_display_name) %>%
          summarise(
            Team  = last(team),
            Games = n(),
            Total = sum(evil_pts),
            .groups = "drop"
          ) %>%
          arrange(desc(Total)) %>%
          slice_head(n = 5) %>%
          transmute(
            Rank   = row_number(),
            Player = player_display_name,
            Team,
            Games,
            `Points` = round(Total, 1)
          )
      }, digits = 1)
    })
  }
}

shinyApp(ui, server)
