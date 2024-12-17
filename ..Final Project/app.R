library(shiny)
library(tidyverse)
library(lubridate)
library(hoopR)
library(skellam)
library(scales)
library(DT)
source("PrepData.R")
source("functions.R")

############ UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .main-section { margin-top: 20px; }
      .trade-section { margin-top: 20px; }
      .btn { margin-top: 5px; margin-bottom: 5px; }
      .green-text { color: green; }
      .default-text { color: black; }
    "))
  ),
  
  titlePanel("NBA Win Probability Simulator"),
  
  tabsetPanel(
    #### SIMULATION TAB
    tabPanel("Simulation",
             sidebarLayout(
               sidebarPanel(
                 selectInput("team", "Select a Team:", 
                             choices = sort(unique(league_player_data$team_name))),
                 actionButton("simulate", "Simulate Win Probabilities", class = "btn"),
                 helpText("Select a team and run simulations. View 
                          standings and upcoming games below. See Trading and 
                          Injuries tabs to simulate the impact of trades and injuries."),
                 actionButton("view_standings_sim", "View NBA Standings", class = "btn")
               ),
               mainPanel(
                 h3("Predicted Team Record"),
                 DTOutput("predicted_record_table"),
                 h3("Upcoming Games"),
                 actionButton("see_more_games", "See More Games", class = "btn"),
                 DTOutput("upcoming_games_table")
               )
             )
    ),
    
    #### TRADING TAB
    tabPanel("Trading",
             fluidPage(
               helpText("Select up to three players to trade between the team 
               selected on the Simulation tab and other teams. Execute Trade to
               and view impact on standings. Press reset rosters to return to 
               original rosters."),
               
               selectInput("other_team", "Select Team to Trade With:", choices = NULL),
               
               selectInput("players_to_trade", 
                           "Select up to Three Players (From Your Selected Team):", 
                           choices = NULL, multiple = TRUE),
               
               selectInput("other_players_to_acquire", 
                           "Select up to Three Players (From Opponent Team):",
                           choices = NULL, multiple = TRUE),
               
               actionButton("execute_trade", "Execute Trade", class = "btn"),
               actionButton("reset_rosters", "Reset Rosters", class = "btn"),
               
               actionButton("view_original_standings_trade", 
                            "View Original Standings Projections", class = "btn"),
               actionButton("view_new_standings_trade", 
                            "View New Standings Projections", class = "btn"),
               
               h3("Trade Impact"),
               verbatimTextOutput("trade_impact"),
               
               h4("Pre-Trade Records (Selected Teams)"),
               DTOutput("pre_trade_records"),
               
               h4("Post-Trade Records (Selected Teams)"),
               DTOutput("post_trade_records")
             )
    ),
    
    #### INJURIES TAB
    tabPanel("Injuries",
             sidebarLayout(
               sidebarPanel(
                 helpText("Select a team and players to injure. 
                           Select injury duration. Press reset to clear injuries"),
                 selectInput("injury_team", "Select a Team:", 
                             choices = sort(unique(league_player_data$team_name))),
                 uiOutput("injury_players_ui"),
                 actionButton("apply_injuries", "Apply Injuries", class = "btn"),
                 actionButton("reset_injuries", "Reset Injuries", class = "btn"),
                 
                 actionButton("view_original_standings_inj", 
                              "View Original Standings Projections", class = "btn"),
                 actionButton("view_new_standings_inj", 
                              "View New Standings Projections", class = "btn")
               ),
               mainPanel(
                 h3("Injured Players"),
                 DTOutput("injured_players_table")
               )
             )
    )
  )
)
############ SERVER

server <- function(input, output, session) {
  
  ####
  # INITIALIZATION, REACTIVE VALS
  
  # Current rosters
  current_rosters <- reactiveVal(league_player_data)
  
  # Hurt players + games hurt
  injured_players <- reactiveVal(data.frame(
    athlete_display_name = character(),
    team_name = character(),
    games_injured = integer(),
    stringsAsFactors = FALSE
  ))
  
  # Upcoming games
  upcoming_games_full <- reactiveVal(NULL)
  
  # Changes to True if user presses see all games
  show_all_games <- reactiveVal(FALSE)
  
  # New and old standings
  previous_standings <- reactiveVal(NULL)
  updated_standings <- reactiveVal(NULL)
  
  # For trades - selected team and other team players
  current_team_players <- reactiveVal(NULL)
  other_team_players <- reactiveVal(NULL)
  
  #Records b4 and after trades
  pre_trade_records <- reactiveVal(NULL)
  post_trade_records <- reactiveVal(NULL)
  
  # When app runs, start calculating standings
  observeEvent(TRUE, {
    isolate({
      ps <- calculate_league_predictions(list(), current_rosters(), 
                                         injured_players(), league_avg_points_static)
      previous_standings(ps)
      updated_standings(ps)
    })
  }, once = TRUE)
  
  # Check if standings have changed
  changes_occurred <- reactive({
    old_stand <- previous_standings()
    new_stand <- updated_standings()
    identical(arrange(old_stand, team), arrange(new_stand, team))
  })
  
  
  ####
  # STANDINGS FUNCTIONS
  
  # Shows original projected standings
  view_standings_original <- function(selected_team) {
    req(previous_standings())
    orig_split <- prepare_standings(previous_standings())
    showModal(modalDialog(
      title = "Original NBA Standings (Projected)",
      tabsetPanel(
        id = "standings_tabs_orig",
        tabPanel("East", DTOutput("east_standings_orig")),
        tabPanel("West", DTOutput("west_standings_orig"))
      ),
      easyClose = TRUE, footer = NULL
    ))
    
    output$east_standings_orig <- renderDT({
      # Select relavant variables for display & change names
      df <- orig_split$east %>% 
        select(seed, team, final.wins, final.losses) %>%
        rename(Seed = seed, Team = team, 
               `Final Wins` = final.wins, `Final Losses` = final.losses)
      datatable(df, rownames = FALSE, options = list(dom = 't', pageLength = 30))
    })
    
    output$west_standings_orig <- renderDT({
      df <- orig_split$west %>%
        select(seed, team, final.wins, final.losses) %>%
        rename(Seed = seed, Team = team, 
               `Final Wins` = final.wins, `Final Losses` = final.losses)
      datatable(df, rownames = FALSE, options = list(dom = 't', pageLength = 30))
    })
    
    # Default tab selection for standings. If team is in west then it should
    # default to west standings.
    updateTabsetPanel(session, "standings_tabs_orig", selected = default_tab(selected_team))
  }
  
  # View new standings
  view_standings_new <- function(selected_team) {
    req(updated_standings(), previous_standings())
    old_stand <- previous_standings()
    new_stand <- updated_standings()
    # for comparison
    new_comp <- create_new_standings(old_stand, new_stand)
    # for updated standings
    showModal(modalDialog(
      title = "NBA Standings (Updated)",
      tabsetPanel(
        id = "standings_tabs_compare",
        tabPanel("East", DTOutput("east_standings_update")),
        tabPanel("West", DTOutput("west_standings_update"))
      ),
      easyClose = TRUE, footer = NULL
    ))
    
    # If the seed change (old - new) is greater than 0, the team has gotten better
    # So will color it green. If no change it's 0 (white). If it's worse, color it red
    
    output$east_standings_update <- renderDT({
      df <- new_comp$east %>%
        select(Seed, Team, `Final Wins`, `Final Losses`, `Change In Wins`, Seed_Change)
      
      datatable(df, rownames = FALSE,
                #Hides seed_change from display
                options = list(dom='t', pageLength=30,
                               columnDefs = list(list(visible=FALSE, targets=5)))) %>%
        formatStyle(
          'Seed',
          valueColumns = 'Seed_Change',
          # so if negative - red, if 0 white, if positive green
          backgroundColor = styleInterval(
            c(-0.0001,0.0001),
            c('tomato','white','lightgreen')
          )
        )
    })
    
    output$west_standings_update <- renderDT({
      df <- new_comp$west %>% 
        select(Seed, Team, `Final Wins`, `Final Losses`, `Change In Wins`, Seed_Change)
      
      datatable(df, rownames = FALSE,
                options = list(dom='t', pageLength=30,
                               columnDefs = list(list(visible=FALSE, targets=5)))) %>%
        formatStyle(
          'Seed',
          valueColumns = 'Seed_Change',
          backgroundColor = styleInterval(
            c(-0.0001,0.0001),
            c('tomato','white','lightgreen')
          )
        )
    })
    
    updateTabsetPanel(session, "standings_tabs_compare", selected = default_tab(selected_team))
  }
  
  
  ####
  # SIMULATION CODE
  
  #  standings from Simulation tab
  observeEvent(input$view_standings_sim, {
    # If no changes the sim tab shows og standings. If changes it shows new.
    if (changes_occurred()) {
      view_standings_original(input$team)
    } else {
      view_standings_new(input$team)
    }
  })
  
  # Update dropdown when team changes
  observeEvent(input$team, {
    req(input$team)
    ctp <- filter_current_team(current_rosters(), injured_players(), input$team)
    current_team_players(ctp)
    opp_teams <- setdiff(sort(unique(current_rosters()$team_name)), input$team)
    updateSelectInput(session, "other_team", choices = opp_teams,
                      selected = if (length(opp_teams) > 0) opp_teams[1] else NULL)
  })
  
  observeEvent(input$other_team, {
    req(input$other_team)
    otp <- filter_current_team(current_rosters(), injured_players(), input$other_team)
    other_team_players(otp)
  })
  
  observeEvent(current_team_players(), {
    ctp <- current_team_players()
    update_team_dropdown(ctp, "players_to_trade", session)
  })
  
  observeEvent(other_team_players(), {
    opp <- other_team_players()
    update_team_dropdown(opp, "other_players_to_acquire", session)
  })
  
  # Simulate the season and show projected record and upcoming games
  sznsim <- function(selected_team) {
    print("Simulating season...")
    
    updated_player_stats <- recalc_league_stats(current_rosters(), injured_players())
    fg <- league_future_games
    wp <- simulate_win_probabilities(updated_player_stats, fg, league_avg_points_static)
    records <- calculate_predicted_record_new(selected_team, updated_player_stats, 
                                              league_avg_points_static)
    
    all_games <- get_upcoming_games(fg, selected_team, wp) %>%
      select(game_date, opponent, win_probability) %>%
      mutate(win_probability = percent(win_probability, accuracy = 0.1))
    
    upcoming_games_full(all_games)
    # defaults to false 
    show_all_games(FALSE)
    
    # Predicted record table
    output$predicted_record_table <- renderDT({
      df <- records %>%
        rename(
          `Current Wins` = current.wins,
          `Current Losses` = current.losses,
          `Predicted Wins` = predicted.wins,
          `Predicted Losses` = predicted.losses,
          `Final Wins` = final.wins,
          `Final Losses` = final.losses,
          Team = team
        )
      datatable(df, rownames = FALSE, options = list(dom = 't', pageLength = 30))
    })
    
    # Upcoming games table
    output$upcoming_games_table <- renderDT({
      games_to_show <- upcoming_games_full()
      if (!show_all_games() && nrow(games_to_show) > 10) {
        games_to_show <- games_to_show[1:10, ]
      }
      games_to_show <- games_to_show %>%
        rename(`Game Date`=game_date, Opponent=opponent, `Win Probability`=win_probability)
      
      datatable(games_to_show, rownames = FALSE, options = list(dom='t', pageLength=30))
    })
    
    # Print team's avg_bpm after simulation to console
    team_bpm <- updated_player_stats %>%
      filter(team_name == selected_team) %>%
      pull(avg_bpm)
    cat("Team's avg_bpm after simulation:", round(team_bpm, 2), "\n")
    
    records
  }
  
  # Simulate when button is pressed
  observeEvent(input$simulate, {
    req(input$team)
    sznsim(input$team)
  })
  
  # Expand when user presses see more games
  observeEvent(input$see_more_games, {
    show_all_games(TRUE)
    output$upcoming_games_table <- renderDT({
      games_to_show <- upcoming_games_full()
      games_to_show <- games_to_show %>%
        rename(`Game Date`=game_date, Opponent=opponent, `Win Probability`=win_probability)
      datatable(games_to_show, rownames=FALSE, options=list(dom='t', pageLength=30))
    })
  })
  
  
  ####
  # TRADING CODE
  
  # Trade execution
  observeEvent(input$execute_trade, {
    print("Simulating trade...")
    
    req(input$players_to_trade, input$other_players_to_acquire, input$team, input$other_team)
    # current team players
    ctp <- filter_current_team(current_rosters(), injured_players(), input$team)
    # other team players
    otp <- filter_current_team(current_rosters(), injured_players(), input$other_team)
    # Put a limit of trading three players at a team. If users tries to do this they are 
    # prompted.
    if (length(input$players_to_trade) > 3 || length(input$other_players_to_acquire) > 3) {
      output$trade_impact <- renderText("You cannot trade more than three players from either team.")
      return()
    }
    
    # Calculate pre-trade predictions
    pre_ctp_stats <- recalc_team_stats_after_trade(ctp)
    pre_otp_stats <- recalc_team_stats_after_trade(otp)
    pre_all_records <- calculate_league_predictions(
      updated_stats_list = list(pre_ctp_stats, pre_otp_stats),
      current_rosters = current_rosters(),
      injured_players = injured_players(),
      league_avg_points_static = league_avg_points_static
    )
    pre_trade_records(pre_all_records)
    
    pre_team <- pre_all_records %>% filter(team == input$team)
    pre_oppteam <- pre_all_records %>% filter(team == input$other_team)
    
    # Execute trade
    new_rosters <- execute_trade_rosters(
      rosters = current_rosters(),
      teamA = input$team,
      teamB = input$other_team,
      playersA = input$players_to_trade,
      playersB = input$other_players_to_acquire
    )
    current_rosters(new_rosters)
    
    # Post-trade predictions
    post_ctp <- filter_current_team(current_rosters(), injured_players(), input$team)
    post_otp <- filter_current_team(current_rosters(), injured_players(), input$other_team)
    post_ctp_stats <- recalc_team_stats_after_trade(post_ctp)
    post_otp_stats <- recalc_team_stats_after_trade(post_otp)
    
    post_all_records <- calculate_league_predictions(
      updated_stats_list = list(post_ctp_stats, post_otp_stats),
      current_rosters = current_rosters(),
      injured_players = injured_players(),
      league_avg_points_static = league_avg_points_static
    )
    post_trade_records(post_all_records)
    
    post_team <- post_all_records %>% filter(team == input$team)
    post_oppteam <- post_all_records %>% filter(team == input$other_team)
    
    diff_team_wins <- (post_team$final.wins - pre_team$final.wins)
    diff_opp_wins <- (post_oppteam$final.wins - pre_oppteam$final.wins)
    
    # First combine players being traded into X,Y,Z format
    team_traded <- paste(input$players_to_trade, collapse = ", ")
    opp_traded <- paste(input$other_players_to_acquire, collapse = ", ")
    # This will let the user know the impact of the trade on wins for both teams
    # + is there are more wins, - if there are less wins.
    impact_message <- paste0(
      "Trading ", team_traded, " for ", opp_traded, " leads to:\n",
      input$team, ": ", ifelse(diff_team_wins >= 0, paste0("+", round(diff_team_wins,1)),
                               round(diff_team_wins,1)), " change in wins\n",
      input$other_team, ": ", 
      ifelse(diff_opp_wins >= 0,
             paste0("+", round(diff_opp_wins,1)), round(diff_opp_wins,1)), 
      " change in wins")
    output$trade_impact <- renderText(impact_message)
    
    # Pre-trade and post-trade records
    output$pre_trade_records <- renderDT({
      df <- pre_trade_records() %>% filter(team %in% c(input$team, input$other_team))
      df <- df %>%
        rename(Team = team, `Final Wins`=final.wins, `Final Losses`=final.losses)
      datatable(df, rownames=FALSE, options=list(dom='t', pageLength=30))
    })
    output$post_trade_records <- renderDT({
      df <- post_trade_records() %>% filter(team %in% c(input$team, input$other_team))
      df <- df %>%
        rename(Team=team, `Final Wins`=final.wins, `Final Losses`=final.losses)
      datatable(df, rownames=FALSE, options=list(dom='t', pageLength=30))
    })
    
    updated_standings(post_all_records)
    
    # Print team's avg_bpm before and after trade to console
    pre_team_bpm <- pre_ctp_stats$avg_bpm[1]
    post_team_bpm <- post_ctp_stats$avg_bpm[1]
    pre_opp_bpm <- pre_otp_stats$avg_bpm[1]
    post_opp_bpm <- post_otp_stats$avg_bpm[1]
    
    cat("Before Trade:", input$team, "avg_bpm =", round(pre_team_bpm,2), 
        "|", input$other_team, "avg_bpm =", round(pre_opp_bpm,2), "\n",
        "After Trade:", input$team, "avg_bpm =", round(post_team_bpm,2), 
        "|", input$other_team, "avg_bpm =", round(post_opp_bpm,2), "\n")
  })
  
  # Reset rosters 
  observeEvent(input$reset_rosters, {
    print("Resetting rosters...")
    current_rosters(league_player_data)
    updated_standings(calculate_league_predictions(list(), current_rosters(), 
                                                   injured_players(), league_avg_points_static))
    
    updateSelectInput(session, "team", choices = sort(unique(league_player_data$team_name)))
    updateSelectInput(session, "other_team", choices = NULL)
    updateSelectInput(session, "players_to_trade", choices = NULL)
    updateSelectInput(session, "other_players_to_acquire", choices = NULL)
    output$trade_impact <- renderText("Rosters reset to original.")
  })
  
  # default
  output$trade_impact <- renderText("No simulations run yet.")
  
  
  #####
  # INJURY CODE 
  
  
  # Injuries
  observeEvent(input$injury_team, {
    req(input$injury_team)
    injury_options  <- filter_current_team(current_rosters(), injured_players(), input$injury_team)
    injury_player_labels  <- setNames(injury_options$athlete_display_name,
                                      paste0(
                                        # display stats too
                                        injury_options$athlete_display_name, " (",
                                        round(injury_options$ppg,1), " PPG, ",
                                        round(injury_options$apg,1), " AST, ",
                                        round(injury_options$rpg,1), " REB, ",
                                        round(injury_options$avg_plus_minus,1), " +/-)"
                                      ))
    output$injury_players_ui <- renderUI({
      tagList(
        selectInput("injury_players", "Select Players to Injure:", 
                    choices = injury_player_labels , multiple = TRUE),
        numericInput("injury_games", "Number of Games Injured:", min = 1, value = 1)
      )
    })
  })
  
  observeEvent(input$apply_injuries, {
    print("Simulating injuries...")
    
    req(input$injury_team, input$injury_players, input$injury_games)
    future_team_games <- league_future_games %>%
      filter(home_name == input$injury_team | away_name == input$injury_team)
    total_remaining <- nrow(future_team_games)
    # if use tries to injury for too many games return error message
    if (input$injury_games > total_remaining) {
      showNotification("Injury duration cannot exceed the team's remaining games.", type = "error")
      return()
    }
    
    # df for new injuries
    new_injuries <- data.frame(
      athlete_display_name = input$injury_players,
      team_name = input$injury_team,
      games_injured = input$injury_games,
      stringsAsFactors=FALSE
    )
    
    # Combine current injuries with existing injuries
    current_inj <- injured_players()
    all_injuries <- unique(rbind(current_inj, new_injuries))
    injured_players(all_injuries)
    
    # display list of hurt players so user can keep track
    output$injured_players_table <- renderDT({
      df <- all_injuries %>%
        rename(`Player Name`=athlete_display_name, 
               `Team Name`=team_name, 
               `Games Injured`=games_injured)
      datatable(df, rownames=FALSE, options=list(dom='t', pageLength=30))
    })
    
    # update standings
    updated_standings(calculate_league_predictions(list(), current_rosters(), 
                                                   injured_players(), 
                                                   league_avg_points_static))
    
    # Print team's avg_bpm after injuries to console
    updated_player_stats <- recalc_league_stats(current_rosters(), injured_players())
    inj_bpm <- updated_player_stats %>%
      filter(team_name == input$injury_team) %>%
      pull(avg_bpm)
    cat("Team's avg_bpm after injuries:", round(inj_bpm, 2), "\n")
  })
  
  #Reset
  observeEvent(input$reset_injuries, {
    print("Resetting injuries...")
    # clear dataframe
    injured_players(data.frame(
      athlete_display_name=character(), 
      team_name=character(), 
      games_injured=integer(), 
      stringsAsFactors=FALSE
    ))
    output$injured_players_table <- renderDT({
      datatable(injured_players(), rownames=FALSE, options=list(dom='t', pageLength=30))
    })
    # calculate standings with newly reset/empty injury rosters
    updated_standings(calculate_league_predictions(list(), current_rosters(), 
                                                   injured_players(), league_avg_points_static))
  })
  
  output$injured_players_table <- renderDT({
    ip <- injured_players()
    if (nrow(ip)>0) {
      ip <- ip %>%
        rename(`Player Name`=athlete_display_name, 
               `Team Name`=team_name, 
               `Games Injured`=games_injured)
    }
    datatable(ip, rownames=FALSE, options=list(dom='t', pageLength=30))
  })
  
  
  ####
  # VIEWING STANDINGS CODE
  
  observeEvent(input$view_original_standings_trade, {
    view_standings_original(input$team)
  })
  
  observeEvent(input$view_new_standings_trade, {
    view_standings_new(input$team)
  })
  
  observeEvent(input$view_original_standings_inj, {
    view_standings_original(input$team)
  })
  
  observeEvent(input$view_new_standings_inj, {
    view_standings_new(input$team)
  })
}

shinyApp(ui = ui, server = server)
