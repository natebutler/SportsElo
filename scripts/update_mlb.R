# scripts/update_mlb.R
# Fetches latest MLB game data, calculates Elo ratings, and exports JSON + plot
# Supports fast incremental updates resuming from the last recorded date in data/mlb_elo.rds

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(tidyverse)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(purrr)
  library(mlbplotR)
  library(ggplot2)
})

# 1. Game parser for a specific date from MLB Stats API
ParserGameByDate <- function(game_date = Sys.Date()) {
  formatted_date <- format(as.Date(game_date), "%Y-%m-%d")
  url <- paste0(
    "http://statsapi.mlb.com/api/v1/schedule/games?sportId=1&startDate=", 
    formatted_date, "&endDate=", formatted_date
  )
  
  res <- tryCatch({
    GET(url, timeout(15))
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(res) || status_code(res) != 200) return(NULL)
  
  raw_text <- rawToChar(res$content)
  if (nchar(raw_text) == 0) return(NULL)
  
  data <- tryCatch({ fromJSON(raw_text, flatten = TRUE) }, error = function(e) NULL)
  if (is.null(data) || is.null(data$totalItems) || data$totalItems == 0) return(NULL)
  
  dataRaw <- enframe(unlist(data))
  dataRaw$name <- str_replace_all(dataRaw$name, "\\.", "_")
  
  totalItems <- dataRaw %>%
    filter(name == "totalItems") %>%
    pull(value) %>%
    as.integer()
  
  if (is.na(totalItems) || totalItems == 0) return(NULL)
  
  cleanedNames <- str_replace_all(dataRaw$name, "[:digit:]", "")
  uniqueColumns <- unique(cleanedNames)
  todaysGamesColumns <- uniqueColumns
  numGames <- dataRaw[dataRaw$name == "totalItems", ]
  parsedGames <- data.frame(matrix(nrow = as.integer(numGames$value), 
                                   ncol = length(todaysGamesColumns)))
  colnames(parsedGames) <- todaysGamesColumns
  
  for (i in seq_along(todaysGamesColumns)) {
    values <- dataRaw %>%
      filter(str_detect(name, fixed(todaysGamesColumns[i])))
    val_vec <- values$value
    n <- as.integer(numGames$value)
    if (length(val_vec) < n) {
      val_vec <- c(val_vec, rep(NA, n - length(val_vec)))
    }
    if (length(val_vec) > n) {
      val_vec <- val_vec[1:n]
    }
    parsedGames[, i] <- val_vec
  }
  
  return(parsedGames)
}

# 2. Elo update function
update_elo <- function(team_elo, opponent_elo, outcome, k = 7) {
  expected_score <- 1 / (1 + 10 ^ ((opponent_elo - team_elo) / 400))
  new_elo <- team_elo + k * (outcome - expected_score)
  return(new_elo)
}

# 3. Initialize baseline ratings with regression toward 1500
initialize_elo <- function(regress_to_mean = 0.75) {
  last_year_teams <- c(
    "Los Angeles Dodgers", "Toronto Blue Jays", "Milwaukee Brewers", "Philadelphia Phillies",
    "New York Yankees", "Chicago Cubs", "Boston Red Sox", "San Diego Padres",
    "Seattle Mariners", "Cleveland Guardians", "Texas Rangers", "Kansas City Royals",
    "New York Mets", "Houston Astros", "Arizona Diamondbacks", "Cincinnati Reds",
    "Atlanta Braves", "Detroit Tigers", "San Francisco Giants", "Tampa Bay Rays",
    "Athletics", "Baltimore Orioles", "Pittsburgh Pirates", "Miami Marlins",
    "St. Louis Cardinals", "Minnesota Twins", "Chicago White Sox", "Los Angeles Angels",
    "Washington Nationals", "Colorado Rockies"
  )
  
  last_year_elo_ratings <- c(
    1575, 1558, 1555, 1554, 1548, 1542, 1539, 1537, 1534, 1518,
    1517, 1515, 1513, 1511, 1510, 1509, 1506, 1506, 1505, 1499,
    1492, 1488, 1487, 1486, 1479, 1460, 1449, 1442, 1437, 1377
  )
  
  elo_df <- data.frame(team = last_year_teams, elo_old = last_year_elo_ratings)
  elo_df$elo <- regress_to_mean * elo_df$elo_old + (1 - regress_to_mean) * 1500
  new_year_elo <- elo_df %>% arrange(desc(elo)) %>% select(team, elo)
  return(new_year_elo)
}

# 4. Incremental / Full Elo Runner
update_all_mlb <- function(rds_path = "data/mlb_elo.rds", k = 7, regress = 0.14) {
  existing_data <- NULL
  if (file.exists(rds_path)) {
    tryCatch({
      existing_data <- readRDS(rds_path)
      message("Found existing data through: ", as.character(max(as.Date(existing_data$Date))))
    }, error = function(e) {
      existing_data <<- NULL
    })
  }
  
  today <- Sys.Date()
  
  if (!is.null(existing_data) && nrow(existing_data) > 0) {
    last_date <- max(as.Date(existing_data$Date))
    
    if (last_date >= today) {
      message("MLB data is already up to date through today (", as.character(today), ").")
      return(existing_data)
    }
    
    start_date <- last_date + 1
    message("Fetching incremental games from ", as.character(start_date), " to ", as.character(today), "...")
    
    # Extract last snapshot for ratings and records
    last_snapshot <- existing_data %>%
      filter(as.Date(Date) == last_date) %>%
      distinct(Team, .keep_all = TRUE)
    
    elo_ratings <- tibble(team = last_snapshot$Team, elo = as.numeric(last_snapshot$`Elo Rating`))
    team_records <- tibble(team = last_snapshot$Team, wins = as.integer(last_snapshot$Wins), losses = as.integer(last_snapshot$Losses))
    previous_elo <- elo_ratings
    elo_history <- list()
  } else {
    start_date <- as.Date("2026-03-24")
    message("Running full MLB calculation from ", as.character(start_date), " to ", as.character(today), "...")
    elo_ratings <- tibble(initialize_elo(regress))
    elo_history <- list()
    previous_elo <- elo_ratings
    team_records <- tibble(team = elo_ratings$team, wins = 0, losses = 0)
    
    initial_elo <- elo_ratings %>%
      mutate(
        `Daily Elo Change` = 0,
        Wins = 0,
        Losses = 0,
        Date = as.Date(start_date)
      ) %>%
      rename(`Team` = team, `Elo Rating` = elo) %>%
      select(`Team`, `Elo Rating`, `Daily Elo Change`, Wins, Losses, Date)
    
    elo_history[[as.character(start_date)]] <- initial_elo
    start_date <- start_date + 1
  }
  
  date_seq <- seq.Date(start_date, today, by = "day")
  total_days <- length(date_seq)
  day_count <- 0
  
  for (current_date in date_seq) {
    day_count <- day_count + 1
    current_date_str <- format(as.Date(current_date), "%Y-%m-%d")
    
    if (day_count %% 15 == 0 || day_count == total_days) {
      message("Processing date: ", current_date_str, " (", day_count, "/", total_days, ")")
    }
    
    games <- ParserGameByDate(current_date_str)
    
    if (is.null(games)) {
      # No games on this date; record carryover
      daily_elo <- elo_ratings %>%
        mutate(
          date = as.Date(current_date),
          delta_elo = 0
        ) %>%
        left_join(team_records, by = "team") %>%
        arrange(desc(elo)) %>%
        select("Team" = team, "Elo Rating" = elo, "Daily Elo Change" = delta_elo,
               "Wins" = wins, "Losses" = losses, "Date" = date)
      elo_history[[as.character(current_date)]] <- daily_elo
      next
    }
    
    valid_names <- names(games)
    valid_names <- valid_names[!is.na(valid_names) & valid_names != ""]
    games <- games %>% select(all_of(valid_names))
    
    if (!("dates_games_seriesDescription" %in% names(games))) next
    if (!("dates_games_status_abstractGameState" %in% names(games))) next
    
    games <- games %>%
      filter(dates_games_seriesDescription == "Regular Season") %>%
      filter(dates_games_status_abstractGameState == "Final")
    
    if (nrow(games) == 0) {
      next
    }
    
    games <- games %>%
      filter(!is.na(dates_games_teams_home_score), !is.na(dates_games_teams_away_score)) %>%
      mutate(
        home_team = dates_games_teams_home_team_name,
        away_team = dates_games_teams_away_team_name,
        home_score = as.numeric(dates_games_teams_home_score),
        away_score = as.numeric(dates_games_teams_away_score)
      )
    
    if (nrow(games) == 0) next
    
    for (i in seq_len(nrow(games))) {
      game <- games[i, ]
      home <- game$home_team
      away <- game$away_team
      home_win <- as.numeric(game$dates_games_teams_home_isWinner == "TRUE")
      away_win <- as.numeric(game$dates_games_teams_away_isWinner == "TRUE")
      
      # Handle Athletics alias
      if (home == "Oakland Athletics") home <- "Athletics"
      if (away == "Oakland Athletics") away <- "Athletics"
      
      if ((home %in% elo_ratings$team) && (away %in% elo_ratings$team)) {
        home_elo <- elo_ratings$elo[elo_ratings$team == home]
        away_elo <- elo_ratings$elo[elo_ratings$team == away]
        
        new_home_elo <- update_elo(home_elo, away_elo, home_win, k)
        new_away_elo <- update_elo(away_elo, home_elo, away_win, k)
        
        elo_ratings$elo[elo_ratings$team == home] <- new_home_elo
        elo_ratings$elo[elo_ratings$team == away] <- new_away_elo
      }
    }
    
    elo_ratings$elo <- round(elo_ratings$elo, 1)
    
    # Update team records
    record_df <- tibble(
      team = c(games$home_team, games$away_team),
      wins = c(as.integer(games$dates_games_teams_home_leagueRecord_wins),
               as.integer(games$dates_games_teams_away_leagueRecord_wins)),
      losses = c(as.integer(games$dates_games_teams_home_leagueRecord_losses),
                 as.integer(games$dates_games_teams_away_leagueRecord_losses))
    ) %>%
      mutate(team = ifelse(team == "Oakland Athletics", "Athletics", team)) %>%
      group_by(team) %>%
      summarise(
        wins = max(wins, na.rm = TRUE),
        losses = max(losses, na.rm = TRUE),
        .groups = "drop"
      )
    
    team_records <- team_records %>%
      rows_update(record_df, by = "team")
    
    daily_elo <- elo_ratings %>%
      left_join(previous_elo, by = "team", suffix = c("", "_prev")) %>%
      mutate(
        date = as.Date(current_date),
        delta_elo = round(elo - elo_prev, 1)
      ) %>%
      left_join(team_records, by = "team") %>%
      arrange(desc(elo)) %>%
      select("Team" = team, "Elo Rating" = elo, "Daily Elo Change" = delta_elo,
             "Wins" = wins, "Losses" = losses, "Date" = date)
    
    elo_history[[as.character(current_date)]] <- daily_elo
    previous_elo <- elo_ratings
  }
  
  if (length(elo_history) == 0) {
    message("No completed games found in the incremental window.")
    return(existing_data)
  }
  
  new_days_df <- bind_rows(elo_history)
  if (nrow(new_days_df) == 0) {
    message("No completed games found in the incremental window.")
    return(existing_data)
  }
  
  # Join logos for the new days
  team_logos <- load_mlb_teams() %>% 
    select(team_name, team_abbr, team_color, team_logo_espn)
  
  new_days_with_logos <- new_days_df %>%
    mutate(Team = ifelse(Team == "Athletics", "Oakland Athletics", Team)) %>%
    left_join(team_logos, by = c("Team" = "team_name")) %>%
    mutate(Team = ifelse(Team == "Oakland Athletics", "Athletics", Team)) %>%
    group_by(Date) %>%
    mutate(Rank = min_rank(desc(`Elo Rating`))) %>%
    ungroup()
  
  if (!is.null(existing_data) && nrow(existing_data) > 0) {
    combined_df <- bind_rows(existing_data, new_days_with_logos) %>%
      distinct(Date, Team, .keep_all = TRUE)
  } else {
    combined_df <- new_days_with_logos
  }
  
  return(combined_df)
}

# --- Execution ---
message("Updating MLB Elo Ratings...")
elo_with_logos <- update_all_mlb(rds_path = "data/mlb_elo.rds", k = 7, regress = 0.14)

# Save updated RDS
saveRDS(elo_with_logos, file = "data/mlb_elo.rds")
message("Saved full history through ", as.character(max(as.Date(elo_with_logos$Date))), " to data/mlb_elo.rds")

# Latest snapshot for website
latest_date <- max(as.Date(elo_with_logos$Date))
latest_elo_df <- elo_with_logos %>%
  filter(as.Date(Date) == latest_date) %>%
  group_by(Team) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(Rank = min_rank(desc(`Elo Rating`))) %>%
  arrange(Rank)

# In case a team had an off-day on latest_date, look up their most recent non-zero delta
teams_with_changes <- elo_with_logos %>%
  filter(`Daily Elo Change` != 0) %>%
  group_by(Team) %>%
  filter(Date == max(Date)) %>%
  select(Team, last_active_change = `Daily Elo Change`)

latest_elo_df <- latest_elo_df %>%
  left_join(teams_with_changes, by = "Team") %>%
  mutate(
    final_daily_change = ifelse(`Daily Elo Change` != 0, `Daily Elo Change`, coalesce(last_active_change, 0))
  )

Sys.setenv(TZ = "America/Chicago")
updated_timestamp <- format(Sys.time(), "%b %d, %Y %I:%M %p %Z")

# Export to JSON
mlb_export <- list(
  sport = "MLB",
  season = 2026,
  updated_at = updated_timestamp,
  total_teams = nrow(latest_elo_df),
  teams = latest_elo_df %>%
    transmute(
      rank = as.integer(Rank),
      team_name = Team,
      team_abbr = team_abbr,
      team_logo_espn = team_logo_espn,
      team_color = ifelse(is.na(team_color), "#002D62", team_color),
      rating = as.numeric(round(`Elo Rating`, 1)),
      daily_change = as.numeric(round(final_daily_change, 1)),
      wins = as.integer(Wins),
      losses = as.integer(Losses),
      record = paste0(Wins, "-", Losses)
    )
)

write_json(mlb_export, "data/mlb.json", pretty = TRUE, auto_unbox = TRUE)
message("Wrote data/mlb.json successfully (", nrow(latest_elo_df), " teams).")

# Export full time-series history for interactive Chart.js
message("Exporting data/mlb_history.json for interactive visualization...")
all_dates <- sort(unique(as.Date(elo_with_logos$Date)))
formatted_dates <- format(all_dates, "%Y-%m-%d")
display_dates <- format(all_dates, "%b %d")

divisions <- list(
  "BAL" = "AL East", "BOS" = "AL East", "NYY" = "AL East", "TB" = "AL East", "TOR" = "AL East",
  "CWS" = "AL Central", "CLE" = "AL Central", "DET" = "AL Central", "KC" = "AL Central", "MIN" = "AL Central",
  "HOU" = "AL West", "LAA" = "AL West", "ATH" = "AL West", "OAK" = "AL West", "SEA" = "AL West", "TEX" = "AL West",
  "ATL" = "NL East", "MIA" = "NL East", "NYM" = "NL East", "PHI" = "NL East", "WSH" = "NL East",
  "CHC" = "NL Central", "CIN" = "NL Central", "MIL" = "NL Central", "PIT" = "NL Central", "STL" = "NL Central",
  "ARI" = "NL West", "COL" = "NL West", "LAD" = "NL West", "SD" = "NL West", "SF" = "NL West"
)

teams_list <- list()
for (i in seq_len(nrow(latest_elo_df))) {
  tm <- latest_elo_df$Team[i]
  abbr <- latest_elo_df$team_abbr[i]
  
  tm_history <- elo_with_logos %>%
    filter(Team == tm) %>%
    select(Date, rating = `Elo Rating`) %>%
    arrange(as.Date(Date))
  
  ratings_vec <- rep(NA_real_, length(all_dates))
  match_idx <- match(as.Date(tm_history$Date), all_dates)
  ratings_vec[match_idx] <- round(as.numeric(tm_history$rating), 1)
  
  for (k in seq_along(ratings_vec)) {
    if (is.na(ratings_vec[k]) && k > 1) {
      ratings_vec[k] <- ratings_vec[k - 1]
    }
  }
  
  div_val <- divisions[[abbr]]
  if (is.null(div_val)) div_val <- "MLB"
  
  teams_list[[abbr]] <- list(
    name = tm,
    abbr = abbr,
    color = ifelse(is.na(latest_elo_df$team_color[i]), "#002D62", latest_elo_df$team_color[i]),
    logo = latest_elo_df$team_logo_espn[i],
    division = div_val,
    current_rank = as.integer(latest_elo_df$Rank[i]),
    current_rating = round(as.numeric(latest_elo_df$`Elo Rating`[i]), 1),
    record = paste0(latest_elo_df$Wins[i], "-", latest_elo_df$Losses[i]),
    ratings = ratings_vec
  )
}

history_export <- list(
  sport = "MLB",
  season = 2026,
  dates = formatted_dates,
  display_dates = display_dates,
  teams = teams_list
)
write_json(history_export, "data/mlb_history.json", pretty = FALSE, auto_unbox = TRUE)
message("Wrote data/mlb_history.json successfully.")

# Generate Trend Plot with Scoreboard Logos (preserved as fallback/reference)
message("Rendering data/mlb_trend.png...")
min_elo <- 1400
max_elo <- 1590
custom_breaks <- c(1400, 1450, 1500, 1550, 1590)

p <- ggplot(elo_with_logos, aes(x = as.Date(Date), y = `Elo Rating`, group = team_abbr, color = team_abbr)) +
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_mlb_scoreboard_logos(
    data = latest_elo_df,
    mapping = aes(x = as.Date(Date), y = `Elo Rating`, team_abbr = team_abbr),
    inherit.aes = FALSE,
    width = 0.045
  ) +
  scale_color_mlb(type = "primary") +
  scale_y_continuous(limits = c(1400, 1590), breaks = custom_breaks) +
  scale_x_date(
    limits = c(min(as.Date(elo_with_logos$Date)), max(as.Date(elo_with_logos$Date)) + 2),
    breaks = seq(min(as.Date(elo_with_logos$Date)), max(as.Date(elo_with_logos$Date)), length.out = 6),
    date_labels = "%b %d"
  ) +
  labs(
    title = "MLB Elo Ratings Over Time",
    subtitle = paste("Historical Elo Progression • Updated", updated_timestamp),
    x = "Date",
    y = "Elo Rating"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 18, color = "#1a1a1a"),
    plot.subtitle = element_text(color = "#555555", size = 12, margin = margin(b = 15)),
    axis.text = element_text(size = 11, color = "#444444"),
    axis.title = element_text(face = "bold", size = 12, color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#e5e7eb")
  )

ggsave("data/mlb_trend.png", plot = p, width = 12, height = 7.5, dpi = 150, bg = "white")
message("Saved data/mlb_trend.png successfully.")
