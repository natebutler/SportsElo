# scripts/update_nfl.R
# Computes NFL Elo ratings using nflfastR and exports structured JSON + RDS

suppressPackageStartupMessages({
  library(nflfastR)
  library(nflreadr)
  library(dplyr)
  library(jsonlite)
  library(tibble)
})

run_nfl_elo <- function(seasons = c(2025, 2026),
                        k = 25,
                        home_field = 40,
                        regress = 1/3,
                        mean_elo = 1500) {
  
  message("Loading NFL schedules for seasons: ", paste(seasons, collapse = ", "))
  games <- load_schedules(seasons) %>%
    filter(game_type == "REG", !is.na(home_score)) %>%
    select(season, week, home_team, away_team, home_score, away_score) %>%
    arrange(season, week)
  
  teams <- unique(c(games$home_team, games$away_team))
  elo <- data.frame(
    team = teams, 
    rating = mean_elo,
    wins = 0, 
    losses = 0, 
    ties = 0,
    stringsAsFactors = FALSE
  )
  
  for (s in sort(unique(games$season))) {
    # Offseason regression toward mean for multi-year tracking
    if (s != min(games$season)) {
      elo$rating <- elo$rating + regress * (mean_elo - elo$rating)
    }
    
    # Reset season records
    elo$wins <- 0
    elo$losses <- 0
    elo$ties <- 0
    
    season_games <- games %>% filter(season == s)
    
    for (w in sort(unique(season_games$week))) {
      week_games <- season_games %>% filter(week == w)
      
      for (i in seq_len(nrow(week_games))) {
        h <- which(elo$team == week_games$home_team[i])
        a <- which(elo$team == week_games$away_team[i])
        
        home_pts <- week_games$home_score[i]
        away_pts <- week_games$away_score[i]
        
        home_result <- if (home_pts > away_pts) 1 else if (home_pts < away_pts) 0 else 0.5
        
        expected_home <- 1 / (1 + 10^((elo$rating[a] - (elo$rating[h] + home_field)) / 400))
        
        elo$rating[h] <- elo$rating[h] + k * (home_result - expected_home)
        elo$rating[a] <- elo$rating[a] + k * ((1 - home_result) - (1 - expected_home))
        
        # Update records
        if (home_result == 1) {
          elo$wins[h]   <- elo$wins[h] + 1
          elo$losses[a] <- elo$losses[a] + 1
        } else if (home_result == 0) {
          elo$losses[h] <- elo$losses[h] + 1
          elo$wins[a]   <- elo$wins[a] + 1
        } else {
          elo$ties[h] <- elo$ties[h] + 1
          elo$ties[a] <- elo$ties[a] + 1
        }
      }
    }
  }
  
  elo_res <- elo %>%
    mutate(record = ifelse(ties > 0,
                           paste0(wins, "-", losses, "-", ties),
                           paste0(wins, "-", losses))) %>%
    select(team, rating, record, wins, losses, ties) %>%
    arrange(desc(rating))
  
  return(elo_res)
}

# --- Execution ---
message("Updating NFL Elo Ratings...")
final_elo <- run_nfl_elo(seasons = c(2025, 2026), k = 25, home_field = 40)

# Save RDS backup
saveRDS(final_elo, "data/nfl_elo.rds")

# Join with official NFL team branding & logos
message("Fetching NFL logos & team information...")
team_logos <- load_teams() %>%
  select(team_abbr, team_name, team_logo_espn, team_color)

elo_table <- final_elo %>%
  mutate(rank = row_number()) %>%
  left_join(team_logos, by = c("team" = "team_abbr"))

Sys.setenv(TZ = "America/Chicago")
updated_timestamp <- format(Sys.time(), "%b %d, %Y %I:%M %p %Z")

nfl_export <- list(
  sport = "NFL",
  seasons = c(2025, 2026),
  updated_at = updated_timestamp,
  total_teams = nrow(elo_table),
  teams = elo_table %>%
    transmute(
      rank = as.integer(rank),
      team_name = coalesce(team_name, team),
      team_abbr = team,
      team_logo_espn = team_logo_espn,
      team_color = coalesce(team_color, "#013369"),
      rating = round(rating, 1),
      record = record
    )
)

write_json(nfl_export, "data/nfl.json", pretty = TRUE, auto_unbox = TRUE)
message("Wrote data/nfl.json successfully.")
