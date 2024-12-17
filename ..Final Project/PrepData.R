library(tidyverse)
library(lubridate)
library(hoopR)
library(skellam)
library(scales)

# Team names by conference

east_teams <- c("76ers", "Bucks", "Celtics", "Hawks", "Hornets", "Heat", "Knicks", 
                "Magic", "Nets", "Pacers", "Pistons", "Raptors", "Wizards", 
                "Cavaliers", "Bulls")

west_teams <- c("Mavericks", "Nuggets", "Warriors", "Clippers", "Lakers", 
                "Grizzlies", "Timberwolves", "Pelicans", "Thunder", "Suns", 
                "Trail Blazers", "Kings", "Spurs", "Jazz", "Rockets")

# This loads in current season data, player stats, and schedule.
# This runs when you boot up the website, so changes teams and running
# simulations is a bit faster.

current_season <- most_recent_nba_season()
box_scores <- load_nba_player_box(seasons = current_season)
league_schedule <- load_nba_schedule()

# Filter to current season
league_schedule <- league_schedule %>%
  filter(season == current_season)

# Clean box score data, changing bpm format to numeric, replacing NA's w 0
box_scores <- box_scores %>%
  mutate(plus_minus = as.numeric(plus_minus),
    plus_minus = replace_na(plus_minus, 0),
    points = replace_na(points, 0),
    assists = replace_na(assists, 0),
    rebounds = replace_na(rebounds, 0))

# Calculate average player stats
league_player_data <- box_scores %>%
  group_by(athlete_id, athlete_display_name, team_name, team_abbreviation) %>%
  summarise(ppg = mean(points, na.rm = TRUE),
    apg = mean(assists, na.rm = TRUE),
    rpg = mean(rebounds, na.rm = TRUE),
    avg_plus_minus = mean(plus_minus, na.rm = TRUE),
    .groups = "drop") %>%
  # Get rid of those without a team names or blank team names (gets rid of
  # any free agents that might be in the data)
  filter(!is.na(team_name) & team_name != "")

# Calculate team-level stats (points, games played, avg bpm)
league_player_stats <- box_scores %>%
  group_by(team_name) %>%
  summarise(avg_bpm = mean(plus_minus, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    games_played = n_distinct(game_id),
    .groups = "drop") %>%
  distinct(team_name, .keep_all = TRUE)

# Calculate league average points per game (team-level)
# Making this static, since league avg ppg is something I do not want recalc'd
# when players are hurt
league_avg_points_static <- league_player_stats %>%
  summarise(league_avg_points = sum(total_points, na.rm = TRUE) 
            / sum(games_played, na.rm = TRUE)) %>%
  pull(league_avg_points)

# Pull and clean future games from data.
league_future_games <- league_schedule %>%
  mutate(
    game_date_time = as.POSIXct(game_date_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    game_date_time_ET = with_tz(game_date_time, tzone = "America/New_York"),
    game_date = as.Date(game_date_time_ET),
    game_time = format(game_date_time_ET, "%I:%M %p")
  ) %>%
  filter(game_date_time > Sys.time())

# There is no need to save the data as a file, we will just pass it into the Shiny app.