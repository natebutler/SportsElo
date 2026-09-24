# scripts/export_from_rds.R
# Populates data/mlb.json, data/nfl.json, and data/mlb_trend.png from existing RDS data

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(nflreadr)
  library(mlbplotR)
  library(ggplot2)
})

# 1. MLB Export
message("Exporting MLB from data/mlb_elo.rds...")
mlb_data <- readRDS("data/mlb_elo.rds")
latest_mlb <- mlb_data %>%
  group_by(Team) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  arrange(Rank)

Sys.setenv(TZ = "America/Chicago")
up_time <- format(Sys.time(), "%b %d, %Y %I:%M %p %Z")

mlb_json <- list(
  sport = "MLB",
  season = 2026,
  updated_at = up_time,
  total_teams = nrow(latest_mlb),
  teams = latest_mlb %>%
    transmute(
      rank = as.integer(Rank),
      team_name = Team,
      team_abbr = team_abbr,
      team_logo_espn = team_logo_espn,
      team_color = ifelse(is.na(team_color), "#002D62", team_color),
      rating = as.numeric(round(`Elo Rating`, 1)),
      daily_change = as.numeric(round(`Daily Elo Change`, 1)),
      wins = as.integer(Wins),
      losses = as.integer(Losses),
      record = paste0(Wins, "-", Losses)
    )
)
write_json(mlb_json, "data/mlb.json", pretty = TRUE, auto_unbox = TRUE)
message("Wrote data/mlb.json")

# MLB Plot
message("Generating data/mlb_trend.png...")
min_elo <- floor(min(mlb_data[["Elo Rating"]], na.rm = TRUE) / 5) * 5
true_max_elo <- max(mlb_data[["Elo Rating"]], na.rm = TRUE)
max_elo <- ceiling(true_max_elo / 5) * 5
max_break <- max_elo + ifelse(max_elo == true_max_elo, 5, 0)
base_breaks <- seq(min_elo, max_break, by = 20)
custom_breaks <- sort(unique(c(base_breaks, 1500, min_elo, max_break)))

p <- ggplot(mlb_data, aes(x = as.Date(Date), y = `Elo Rating`, group = team_abbr, color = team_abbr)) +
  geom_line(linewidth = 1, alpha = 0.85) +
  geom_mlb_scoreboard_logos(
    data = latest_mlb,
    mapping = aes(x = as.Date(Date), y = `Elo Rating`, team_abbr = team_abbr),
    inherit.aes = FALSE,
    width = 0.045
  ) +
  scale_color_mlb(type = "primary") +
  scale_y_continuous(limits = c(min_elo, max_break), breaks = custom_breaks) +
  scale_x_date(
    limits = c(min(as.Date(mlb_data$Date)), max(as.Date(mlb_data$Date)) + 1),
    breaks = seq(min(as.Date(mlb_data$Date)), max(as.Date(mlb_data$Date)), length.out = 6),
    date_labels = "%b %d"
  ) +
  labs(
    title = "MLB Elo Ratings Over Time",
    subtitle = paste("Historical Elo Progression • Updated", up_time),
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
message("Saved data/mlb_trend.png")

# 2. NFL Export
message("Exporting NFL from data/nfl_elo.rds...")
nfl_data <- readRDS("data/nfl_elo.rds")
nfl_logos <- load_teams() %>% select(team_abbr, team_name, team_logo_espn, team_color)
nfl_table <- nfl_data %>%
  mutate(rank = row_number()) %>%
  left_join(nfl_logos, by = c("team" = "team_abbr"))

nfl_json <- list(
  sport = "NFL",
  seasons = c(2025, 2026),
  updated_at = up_time,
  total_teams = nrow(nfl_table),
  teams = nfl_table %>%
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
write_json(nfl_json, "data/nfl.json", pretty = TRUE, auto_unbox = TRUE)
message("Wrote data/nfl.json")
message("Initial export complete!")
