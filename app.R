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

# ---- Play-by-play scoring (for Play of the Year / Quarter of the Year) ------
#
# Weekly box score stats can't be split by play or quarter, so these two awards
# are built from play-by-play. The weights below mirror the formula above, one
# event at a time. Edit them here if your scoring changes.

PLAY_W <- c(
  inc = 1,  py = -0.04, ptd = -4, p2pc = -1, int = 10, sk = 2,       # passing
  ra = 1,   ry = -0.25, rtd = -2, p2pr = -1,                         # rushing
  ret = 3,  rec = -3,   rey = -0.05, retd = -2, p2pre = -1,          # receiving
  fum = 5,  fuml = 10,                                               # fumbles
  fgm = 1,  fgm0 = 10,  fgm40 = 10, fgm50 = 5, fgm60 = 3, patm = 5,  # kicking
  ptb = 25,                                                          # blocked punt (punter)
  prtd = -6,                                                         # punt return TD
  krtd = 0, ftd = 0, inttd = 0, frtd = 0, blkkrtd = 0                # other return TDs: not in the weekly formula
)

PBP_COLS <- c(
  "game_id", "play_id", "qtr", "week", "season_type", "play_type", "desc", "posteam",
  "two_point_attempt", "two_point_conv_result", "sack", "incomplete_pass", "complete_pass",
  "pass_touchdown", "rush_touchdown", "interception", "return_touchdown",
  "passer_player_id", "passer_player_name", "passing_yards",
  "rusher_player_id", "rusher_player_name", "rushing_yards",
  "receiver_player_id", "receiver_player_name", "receiving_yards",
  "field_goal_attempt", "field_goal_result", "kick_distance", "yardline_100",
  "kicker_player_id", "kicker_player_name", "extra_point_attempt", "extra_point_result",
  "punt_blocked", "punter_player_id", "punter_player_name", "punt_attempt", "kickoff_attempt",
  "fumble", "fumble_lost", "fumbled_1_team", "fumbled_1_player_id", "fumbled_1_player_name",
  "fumbled_2_team", "fumbled_2_player_id", "fumbled_2_player_name",
  "fumble_recovery_1_team", "fumble_recovery_2_team",
  "td_team", "td_player_id", "td_player_name"
)

# One row per player per play per stat, with points attached.
play_events <- function(pbp) {
  valid <- pbp %>% filter(!is.na(play_type), play_type != "no_play")
  reg   <- valid %>% filter(is.na(two_point_attempt) | two_point_attempt == 0)
  twopt <- valid %>% filter(two_point_attempt == 1)
  
  ev <- function(df, id, name, team, stat, value = 1) {
    df %>%
      transmute(game_id, play_id, qtr, week, season_type,
                team = {{ team }}, player_id = {{ id }}, stat = stat, value = {{ value }}) %>%
      filter(!is.na(player_id), !is.na(value))
  }
  
  pass_plays <- reg %>% filter(play_type %in% c("pass", "qb_spike"))
  # an interception also counts as an incompletion (attempts - completions)
  inc  <- pass_plays %>% filter(incomplete_pass == 1 | interception == 1) %>% ev(passer_player_id, passer_player_name, posteam, "inc")
  py   <- pass_plays %>% filter(sack == 0 | is.na(sack)) %>% ev(passer_player_id, passer_player_name, posteam, "py", passing_yards)
  ptd  <- pass_plays %>% filter(pass_touchdown == 1) %>% ev(passer_player_id, passer_player_name, posteam, "ptd")
  int  <- pass_plays %>% filter(interception == 1)   %>% ev(passer_player_id, passer_player_name, posteam, "int")
  sk   <- pass_plays %>% filter(sack == 1)           %>% ev(passer_player_id, passer_player_name, posteam, "sk")
  p2pc <- twopt %>% filter(two_point_conv_result == "success", play_type == "pass") %>% ev(passer_player_id, passer_player_name, posteam, "p2pc")
  
  rush_plays <- reg %>% filter(play_type %in% c("run", "qb_kneel")) %>%
    mutate(rid = coalesce(rusher_player_id, passer_player_id), rname = coalesce(rusher_player_name, passer_player_name))
  ra   <- rush_plays %>% ev(rid, rname, posteam, "ra")
  ry   <- rush_plays %>% ev(rid, rname, posteam, "ry", rushing_yards)
  rtd  <- rush_plays %>% filter(rush_touchdown == 1) %>% ev(rid, rname, posteam, "rtd")
  p2pr <- twopt %>% filter(two_point_conv_result == "success", play_type == "run") %>% ev(rusher_player_id, rusher_player_name, posteam, "p2pr")
  
  tgt   <- pass_plays %>% filter(!is.na(receiver_player_id), sack == 0 | is.na(sack))
  ret   <- tgt %>% ev(receiver_player_id, receiver_player_name, posteam, "ret")
  rec   <- tgt %>% filter(complete_pass == 1)  %>% ev(receiver_player_id, receiver_player_name, posteam, "rec")
  rey   <- tgt %>% filter(complete_pass == 1)  %>% ev(receiver_player_id, receiver_player_name, posteam, "rey", receiving_yards)
  retd  <- tgt %>% filter(pass_touchdown == 1) %>% ev(receiver_player_id, receiver_player_name, posteam, "retd")
  p2pre <- twopt %>% filter(two_point_conv_result == "success", play_type == "pass") %>% ev(receiver_player_id, receiver_player_name, posteam, "p2pre")
  
  # missed FGs (blocked FGs count as misses); distance buckets stack on the flat miss
  fgm <- valid %>% filter(field_goal_attempt == 1, field_goal_result %in% c("missed", "blocked")) %>%
    mutate(dist = coalesce(kick_distance, yardline_100 + 17),
           bucket = case_when(dist < 40 ~ "fgm0", dist < 50 ~ "fgm40", dist < 60 ~ "fgm50", TRUE ~ "fgm60"))
  fgm_flat <- fgm %>% ev(kicker_player_id, kicker_player_name, posteam, "fgm")
  fgm_bkt  <- fgm %>% transmute(game_id, play_id, qtr, week, season_type, team = posteam,
                                player_id = kicker_player_id, stat = bucket, value = 1) %>%
    filter(!is.na(player_id))
  patm <- valid %>% filter(extra_point_attempt == 1, extra_point_result %in% c("failed", "blocked")) %>%
    ev(kicker_player_id, kicker_player_name, posteam, "patm")
  ptb  <- valid %>% filter(punt_blocked == 1) %>% ev(punter_player_id, punter_player_name, posteam, "ptb")
  
  # fumbles (up to two fumblers per play) and fumbles lost
  fum_long <- bind_rows(
    valid %>% filter(!is.na(fumbled_1_player_id)) %>%
      transmute(game_id, play_id, qtr, week, season_type, team = fumbled_1_team, player_id = fumbled_1_player_id,
                lost = (!is.na(fumble_recovery_1_team) & fumble_recovery_1_team != fumbled_1_team) |
                  (fumble_lost == 1 & is.na(fumbled_2_player_id))),
    valid %>% filter(!is.na(fumbled_2_player_id)) %>%
      transmute(game_id, play_id, qtr, week, season_type, team = fumbled_2_team, player_id = fumbled_2_player_id,
                lost = !is.na(fumble_recovery_2_team) & fumble_recovery_2_team != fumbled_2_team))
  fum  <- fum_long %>% mutate(stat = "fum", value = 1) %>% select(-lost)
  fuml <- fum_long %>% filter(lost) %>% mutate(stat = "fuml", value = 1) %>% select(-lost)
  
  # return TDs
  ret_td <- valid %>% filter(return_touchdown == 1) %>%
    mutate(stat = case_when(
      kickoff_attempt == 1 ~ "krtd",
      punt_attempt == 1 & punt_blocked == 1 ~ "blkkrtd",
      punt_attempt == 1 ~ "prtd",
      field_goal_attempt == 1 | extra_point_attempt == 1 ~ "blkkrtd",
      interception == 1 ~ "inttd",
      fumble == 1 & td_team == posteam ~ "ftd",
      fumble == 1 ~ "frtd",
      TRUE ~ NA_character_)) %>%
    filter(!is.na(stat)) %>%
    transmute(game_id, play_id, qtr, week, season_type, team = td_team,
              player_id = td_player_id, stat, value = 1) %>%
    filter(!is.na(player_id))
  
  bind_rows(inc, py, ptd, int, sk, p2pc, ra, ry, rtd, p2pr, ret, rec, rey, retd, p2pre,
            fgm_flat, fgm_bkt, patm, ptb, fum, fuml, ret_td) %>%
    mutate(points = value * unname(PLAY_W[stat]))
}

pbp <- readr::read_csv(
  sprintf("https://github.com/nflverse/nflverse-data/releases/download/pbp/play_by_play_%d.csv.gz", SEASON),
  col_select = any_of(PBP_COLS), guess_max = 100000, show_col_types = FALSE
)

# Names and positions come from the weekly stats so play-level rows use the
# same display names and the same position filter as the rest of the app.
player_lookup <- stats %>%
  filter(position %in% POSITIONS) %>%
  distinct(player_id, .keep_all = TRUE) %>%
  select(player_id, player_display_name, position)

events <- play_events(pbp) %>%
  inner_join(player_lookup, by = "player_id")
stopifnot(!anyNA(events$points))

# Blocked punts are a flat 25 and swamp everything else, so plays containing one
# are left out of Play of the Year. They still count in Quarter of the Year.
blocked_punt_plays <- pbp %>%
  filter(punt_blocked == 1) %>%
  distinct(game_id, play_id)

play_scores <- events %>%
  group_by(game_id, play_id, player_id) %>%
  summarise(
    player_display_name = first(player_display_name), position = first(position),
    team = first(team), week = first(week), qtr = first(qtr),
    season_type = first(season_type), evil_pts = sum(points), .groups = "drop"
  ) %>%
  anti_join(blocked_punt_plays, by = c("game_id", "play_id")) %>%
  left_join(pbp %>% select(game_id, play_id, desc), by = c("game_id", "play_id"))

quarter_scores <- events %>%
  group_by(game_id, qtr, player_id) %>%
  summarise(
    player_display_name = first(player_display_name), position = first(position),
    team = first(team), week = first(week),
    season_type = first(season_type), evil_pts = sum(points), .groups = "drop"
  )

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
      column(6, h4("Jon Kitna Most Villanous Player (MVP)"), tableOutput("total_all"))
    ),
    fluidRow(
      column(7, h4("Jalen Hurts Play of the Year"), tableOutput("play_all")),
      column(5, h4("Carson Palmer Quarter of the Year"), tableOutput("quarter_all"))
    ),
    helpText("Play of the Year excludes plays with a blocked punt.")
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
  
  filtered_plays <- reactive({
    if (isTRUE(input$include_post)) play_scores else filter(play_scores, season_type == "REG")
  })
  
  filtered_quarters <- reactive({
    if (isTRUE(input$include_post)) quarter_scores else filter(quarter_scores, season_type == "REG")
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
        `Evil Pts` = round(evil_pts, 1)
      )
  }, digits = 1)
  
  output$total_all <- renderTable({
    filtered() %>%
      group_by(player_id, player_display_name, position) %>%
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
        Pos    = position,
        Team,
        Games,
        `Evil Pts` = round(Total, 1)
      )
  }, digits = 1)
  
  # ---- Play of the Year (blocked-punt plays excluded) ----
  output$play_all <- renderTable({
    filtered_plays() %>%
      arrange(desc(evil_pts)) %>%
      slice_head(n = 5) %>%
      transmute(
        Rank   = row_number(),
        Player = player_display_name,
        Pos    = position,
        Team   = team,
        Week   = as.integer(week),
        Qtr    = if_else(qtr >= 5, "OT", paste0("Q", qtr)),
        `Evil Pts` = round(evil_pts, 1)
      )
  }, digits = 1)
  
  # ---- Quarter of the Year ----
  output$quarter_all <- renderTable({
    filtered_quarters() %>%
      arrange(desc(evil_pts)) %>%
      slice_head(n = 5) %>%
      transmute(
        Rank   = row_number(),
        Player = player_display_name,
        Pos    = position,
        Team   = team,
        Week   = as.integer(week),
        Qtr    = if_else(qtr >= 5, "OT", paste0("Q", qtr)),
        `Evil Pts` = round(evil_pts, 1)
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
            `Evil Pts` = round(evil_pts, 1)
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
            `Evil Pts` = round(Total, 1)
          )
      }, digits = 1)
    })
  }
}

shinyApp(ui, server)
