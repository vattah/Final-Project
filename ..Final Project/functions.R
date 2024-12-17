library(tidyverse)
library(lubridate)
library(hoopR)
library(skellam)
library(scales)

####
# TEAM & LEAGUE STANDINGS FUNCTIONS

# Function to calculate seed for both conferences
sort_standings <- function(standings) {
  east <- standings %>%
    filter(team %in% east_teams) %>%
    arrange(desc(final.wins), final.losses) %>%
    mutate(seed = row_number())
  
  west <- standings %>%
    filter(team %in% west_teams) %>%
    arrange(desc(final.wins), final.losses) %>%
    mutate(seed = row_number())
  
  list(east = east, west = west)
}

# Function to get what the default standings tab should be. If selected team is 
# in the East, default should be East. Vice versa for the West.
default_tab <- function(selected_team) {
  if (selected_team %in% east_teams) return("East")
  if (selected_team %in% west_teams) return("West")
  return("East")
}

# calc current standings
calculate_current_standings <- function(team_filter = NULL) {
  # filter by completed games, make sure they have scores
  completed_games <- league_schedule %>%
    filter(status_type_name == "STATUS_FINAL", !is.na(home_score), 
           !is.na(away_score)) %>%
    # assign wins based on score
    mutate(home_win = ifelse(home_score > away_score, 1, 0),
           away_win = ifelse(away_score > home_score, 1, 0))
  
  standings <- completed_games %>%
    pivot_longer(
      cols = c(home_name, away_name),
      names_to = "location",
      values_to = "team_name"
    ) %>%
    mutate(win = case_when(
      # assign wins
      location == "home_name" & home_win == 1 ~ 1,
      location == "away_name" & away_win == 1 ~ 1,
      TRUE ~ 0
    ),
    loss = 1 - win
    ) %>%
    # Summing
    group_by(team_name) %>%
    summarise(wins = sum(win, na.rm = TRUE),
              losses = sum(loss, na.rm = TRUE),
              games_played = n(),
              .groups = "drop"
    ) %>%
    arrange(desc(wins), losses)
  
  # Filter for team selected (if there is one)
  if (!is.null(team_filter)) {
    standings <- standings %>% filter(team_name == team_filter)
  }
  
  standings
}

# Create new standings (after trade or player injury)
create_new_standings <- function(old_standings, new_standings) {
  # Again, sort old and new standings into East and West
  old_split <- sort_standings(old_standings)
  new_split <- sort_standings(new_standings)
  
  new_east <- old_split$east %>%
    select(team, old_final_wins = final.wins, old_final_losses = final.losses, 
           old_seed = seed) %>%
    inner_join(new_split$east %>% 
                 select(team, new_final_wins = final.wins, new_final_losses = final.losses,
                        new_seed = seed), by = "team") %>%
    mutate(
      # Seed change (b4 and after roster changes)
      Seed_Change = old_seed - new_seed,
      # Calculate the change in wins. Adding "+" if positive.
      `Change In Wins` = new_final_wins - old_final_wins,
      `Change In Wins` = ifelse(`Change In Wins` > 0,
                                paste0("+", `Change In Wins`),
                                as.character(`Change In Wins`)),
      Seed = new_seed) %>%
    arrange(Seed) %>%
    select(Seed, Team = team, `Final Wins` = new_final_wins, 
           `Final Losses` = new_final_losses, `Change In Wins`, Seed_Change)
  
  new_west <- old_split$west %>%
    select(team, old_final_wins = final.wins, old_final_losses = final.losses,
           old_seed = seed) %>%
    inner_join(new_split$west %>%
                 select(team, new_final_wins = final.wins, 
                        new_final_losses = final.losses, new_seed = seed), 
               by = "team") %>%
    mutate(Seed_Change = old_seed - new_seed,
           `Change In Wins` = new_final_wins - old_final_wins,
           `Change In Wins` = ifelse(`Change In Wins` > 0,
                                     paste0("+", `Change In Wins`),
                                     as.character(`Change In Wins`)),
           Seed = new_seed
    ) %>%
    arrange(Seed) %>%
    select(Seed, Team = team, `Final Wins` = new_final_wins, 
           `Final Losses` = new_final_losses, `Change In Wins`, Seed_Change)
  
  list(east = new_east, west = new_west)
}

# calc team stats following the trade
recalc_team_stats_after_trade <- function(team_players) {
  if (is.null(team_players) || nrow(team_players) == 0) {
    return(tibble(team_name = "Unknown", avg_bpm = 0, total_points = 0, games_played = 1))
  }
  new_team_stats <- team_players %>%
    summarise(team_name = first(team_name),
              avg_bpm = mean(avg_plus_minus, na.rm = TRUE),
              total_points = 0,
              games_played = 1
    )
  
  if (nrow(new_team_stats) == 0 || is.na(new_team_stats$team_name)) {
    team_name_val <- if (!is.null(team_players$team_name)) first(team_players$team_name) else "Unknown"
    new_team_stats <- tibble(team_name = team_name_val, avg_bpm = 0, total_points = 0, games_played = 1)
  }
  
  new_team_stats
}

# merge
merge_updated_stats <- function(league_stats, updated_team_stats) {
  league_stats <- league_stats %>% distinct(team_name, .keep_all = TRUE)
  team_name_val <- updated_team_stats$team_name[1]
  if (team_name_val %in% league_stats$team_name) {
    league_stats <- league_stats %>%
      mutate(avg_bpm = ifelse(team_name == team_name_val,
                              updated_team_stats$avg_bpm, avg_bpm))
  } else {
    league_stats <- bind_rows(league_stats, updated_team_stats)
  }
  
  league_stats %>% distinct(team_name, .keep_all = TRUE)
}

# calc league wide predictions. Runs win probs again for all teams,
# Even those not involved in the trade or injury.
calculate_league_predictions <- function(updated_stats_list, current_rosters, 
                                         injured_players, league_avg_points_static) {
  league_stats <- recalc_league_stats(current_rosters, injured_players)
  
  for (ustat in updated_stats_list) {
    league_stats <- merge_updated_stats(league_stats, ustat)
  }
  
  teams <- unique(league_stats$team_name)
  purrr::map_dfr(teams, ~ calculate_predicted_record_new(.x, league_stats, 
                                                         league_avg_points_static))
}


####
# PREDICTION & RECORD CALCULATION FUNCTIONS

calculate_predicted_record <- function(team_win_probs, selected_team, current_standings) {
  current_wins <- current_standings$wins
  current_losses <- current_standings$losses
  # get win probs.
  team_win_probs <- team_win_probs %>%
    mutate(team_win_probability = ifelse(home_name == selected_team,
                                         win_probability_home, 1 - win_probability_home))
  # sum up win probs for the games that are left
  total_remaining <- nrow(team_win_probs)
  # calculate expected wins and losses.
  expected_wins <- round(sum(team_win_probs$team_win_probability))
  expected_losses <- total_remaining - expected_wins
  
  final_wins <- current_wins + expected_wins
  final_losses <- current_losses + expected_losses
  
  tibble(
    team = selected_team,
    current.wins = current_wins,
    current.losses = current_losses,
    predicted.wins = expected_wins,
    predicted.losses = expected_losses,
    final.wins = final_wins,
    final.losses = final_losses
  )
}

calculate_predicted_record_new <- function(team_name, updated_player_stats, league_avg_points_static) {
  updated_player_stats <- updated_player_stats %>% distinct(team_name, .keep_all = TRUE)
  cs <- calculate_current_standings(team_filter = team_name)
  
  current_wins <- cs$wins
  current_losses <- cs$losses
  # sim future games and get win probs
  fg <- league_future_games
  wp <- simulate_win_probabilities(updated_player_stats, fg, league_avg_points_static)
  
  team_fgs <- wp %>%
    filter(home_name == team_name | away_name == team_name) %>%
    mutate(team_win_probability = ifelse(home_name == team_name,
                                         win_probability_home, 
                                         1 - win_probability_home))
  total_remaining <- nrow(team_fgs)
  
  # if statement for if we are at the end of the szn with 0 games left.
  if (total_remaining == 0) {
    predicted_wins <- 0
    predicted_losses <- 0
  } else {
    expected_wins <- sum(team_fgs$team_win_probability)
    predicted_wins <- round(expected_wins)
    predicted_losses <- total_remaining - predicted_wins
  }
  
  
  final_wins <- current_wins + predicted_wins
  final_losses <- current_losses + predicted_losses
  
  tibble(
    team = team_name,
    current.wins = current_wins,
    current.losses = current_losses,
    predicted.wins = predicted_wins,
    predicted.losses = predicted_losses,
    final.wins = final_wins,
    final.losses = final_losses
  )
}


####
# SIMULATION & GAME FUNCTIONS

simulate_win_probabilities <- function(player_stats, future_games, league_avg_points_static) {
  team_strength <- player_stats %>%
    distinct(team_name, .keep_all = TRUE) %>%
    select(team_name, avg_bpm)
  
  fg <- future_games %>%
    left_join(team_strength, by = c("home_name" = "team_name")) %>%
    rename(home_avg_bpm = avg_bpm) %>%
    left_join(team_strength, by = c("away_name" = "team_name")) %>%
    rename(away_avg_bpm = avg_bpm) %>%
    mutate(
      # lambda for skellam is league avg pts + avg bpm (has if in case stuff is NA)
      lambda_home = league_avg_points_static + ifelse(is.na(home_avg_bpm), 0, home_avg_bpm),
      lambda_away = league_avg_points_static + ifelse(is.na(away_avg_bpm), 0, away_avg_bpm),
      win_probability_home = pskellam(q = 0, lambda1 = lambda_home, 
                                      lambda2 = lambda_away, lower.tail = FALSE) +
        dskellam(0, lambda_home, lambda_away)*0.5
    )
  
  fg
}

get_upcoming_games <- function(future_games, team_name, wp) {
  upcoming_games <- future_games %>%
    filter(home_name == team_name | away_name == team_name) %>%
    mutate(
      game_date_time_ET = with_tz(game_date_time, tzone = "America/New_York"),
      game_date = as.Date(game_date_time_ET),
      opponent = ifelse(home_name == team_name, paste("vs.", away_name), paste("@", home_name))
    ) %>%
    left_join(wp %>% select(game_id, win_probability_home), by = "game_id") %>%
    mutate(
      win_probability = ifelse(home_name == team_name, win_probability_home, 1 - win_probability_home)
    ) %>%
    arrange(game_date_time_ET) %>%
    distinct(game_date, opponent, .keep_all = TRUE)
  
  upcoming_games
}


####
# TEAM, PLAYER, AND ROSTERFUNCTIONS

filter_current_team <- function(rosters, injured, team_name) {
  rosters %>%
    filter(team_name == !!team_name, # Filter for rows where team_name matches the 
           #inputted team_name
           # Exclude hurt players
           !athlete_display_name %in% injured$athlete_display_name)
}

# updating dropdowns for player selection
update_team_dropdown <- function(team_df, input_id, session) {
  if (!is.null(team_df) && nrow(team_df) > 0) {
    player_choices <- team_df$athlete_display_name
    player_labels <- paste0(
      # Putting stats along with labels for players
      team_df$athlete_display_name, " (",
      round(team_df$ppg,1), " PPG, ",
      round(team_df$apg,1), " AST, ",
      round(team_df$rpg,1), " REB, ",
      round(team_df$avg_plus_minus,1), " +/-)"
    )
    names(player_choices) <- player_labels
    updateSelectInput(session, input_id, choices = player_choices, selected = NULL)
  } else {
    updateSelectInput(session, input_id, choices = character(0))
  }
}

# Function to change rosters to update for trades
execute_trade_rosters <- function(rosters, teamA, teamB, playersA, playersB) {
  # filter out traded players
  keep <- !(rosters$athlete_display_name %in% playersA & rosters$team_name == teamA)
  keep <- keep & !(rosters$athlete_display_name %in% playersB & rosters$team_name == teamB)
  new_rosters <- rosters[keep, ]
  
  # Filter for traded players
  traded_from_A <- rosters %>% filter(athlete_display_name %in% playersA & team_name == teamA)
  traded_from_B <- rosters %>% filter(athlete_display_name %in% playersB & team_name == teamB)
  
  # Swap team names for the traded players
  traded_from_A$team_name <- teamB
  traded_from_B$team_name <- teamA
  
  #Combine
  bind_rows(new_rosters, traded_from_A, traded_from_B)
}

# recalc stats for non-hurt players
recalc_league_stats <- function(current_rosters, injured_players) {
  active_rosters <- current_rosters %>%
    filter(!athlete_display_name %in% injured_players$athlete_display_name)
  
  team_stats <- active_rosters %>%
    group_by(team_name) %>%
    summarise(
      avg_bpm = mean(avg_plus_minus, na.rm = TRUE),
      games_played = 1,
      .groups = "drop"
    ) %>%
    distinct(team_name, .keep_all = TRUE)
  
  team_stats
}
