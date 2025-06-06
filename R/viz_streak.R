



viz_streak <- function(team, year) {

  ### Check if arguments are valid ----
  valid_teams <- c("AL East", "AL Central", "AL West", "AL Overall",
                   "NL East", "NL Central", "NL West", "NL Overall",
                   "MLB")
  if (!(is.character(team) && ((nchar(team) == 3) | (team %in% valid_teams)))) {
    stop("The 'team' must be the Baseball Reference team abbreviation or any of these ones:
         AL East, AL Central, AL West, AL Overall, NL East, NL Central, NL West, NL Overall or MLB")
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (!(is.numeric(year) && year >= 1876 && year <= current_year)) {
    stop(paste0("The 'year' must be a numeric value between 1876 and the last/current MLB season"))
  }

  # If both arguments are valid, continue with the function

  ### Identify 'team' input type and get names of teams to visualize ----

  if (intersect(grepl("AL|NL", team),
                grepl("East|Central|West|Overall", team))) {
    # Division or leagues in that year
    message(paste0("Retreiving teams that played in ", team, " in ", year, "..."))
    teams <- baseballr::bref_standings_on_date(paste0(year,"-04-30"), team) |>
      as.data.frame() |>
      select(1) |>
      unlist()

  } else if (team == "MLB") {

    # All MLB teams in year
    mlb <- c("AL Overall", "NL Overall")

    message(paste0("Retreiving teams that played in ", team, " in ", year, "..."))
    teams <- pbapply::pbsapply(mlb, baseballr::bref_standings_on_date, date = paste0(year,"-04-30"))
    teams <- c(teams[[1,1]], teams[[1,2]])

  } else {
    # Only one team
    teams <- team
  }

  ### Get the game's results of each team to be visualized ----

  message("Getting games' data ...")

  # Gets the team's results tables for each team
  streak <- pbapply::pblapply(teams, baseballr::bref_team_results, year)
  # Binds tables for all teams into one data frame
  streak <- do.call("rbind", streak)

  # change Game variable to numeric
  streak$Gm <- as.numeric(streak$Gm)

  ### Tidying the `rd` data frame ----

  streak <- streak |>
    tidyr::separate(Date, c("wd", "Date"), sep = ", ") |>     # wd = weekday
    tidyr::unite(Date, c("Date", "Year"), sep = ", ") |>
    dplyr::select(Gm, Date, Tm, H_A, Opp, Record, Rank, Streak)

  names(streak)[c(1,3)] <- c("Game", "Team")

  # Step 1: Determine the best and worst streak (by absolute value) for each team
  team_streak_summary <- streak |>
    dplyr::group_by(Team) |>
    dplyr::summarise(
      MaxStreak = max(Streak),
      MinStreak = min(Streak),
      Game_MaxStreak = max(Game[Streak == MaxStreak], na.rm = TRUE),
      Game_MinStreak = max(Game[Streak == MinStreak], na.rm = TRUE)
    )

  # Set the "Best Streak" and "Worst Streak" values for each team in a new column
  streak_with_labels <- streak |>
    dplyr::left_join(team_streak_summary, by = "Team") |>
    dplyr::group_by(Team) |>
    dplyr::mutate(Label = ifelse(Game <= Game_MaxStreak &
                                   Game > Game_MaxStreak - MaxStreak, "Best Streak",
                                 ifelse(Game <= Game_MinStreak &
                                          Game > Game_MinStreak + MinStreak, "Worst Streak",NA))) |>
    dplyr::filter(!is.na(Label)) |>
    ungroup()

  ### Determine the columns for the grid chart, based on number of teams ----

  if (length(teams) <= 5) {
    # Either only one team or one Division
    viz_col <- 1
  } else if (length(teams) == 15) {
    # One League
    viz_col <- 3
  } else if (length(teams) == 30) {
    # All MLB
    viz_col <- 5
  }

  ### Defining min & max for yAxis to be the same for all charts ----
  min_streak <- floor(min(streak_with_labels$Streak)/5)*5
  max_streak <- ceiling(max(streak_with_labels$Streak)/5)*5

  # interval in y axis to allow a better adjustment of y min and max
  # if (max_RD <= 120) {
  #   y_interval <- 25
  # } else if (max_RD <= 250) {
  #   y_interval <- 50
  # } else if (max_RD <= 350) {
  #   y_interval <- 100
  # } else {
  #   y_interval <- 200
  # }

  ### Creating an ordered vector (not factor) of teams based on maximum streak. ----
  ### NOTE: This is because 'highcharter' hc_grid function plots charts in the order they are created and not based on factors (as ggplot)

  teams_factor <- streak_with_labels  |>
    dplyr::group_by(Team)  |>
    dplyr::summarise(max_streak = max(Streak)) |>
    dplyr::arrange(dplyr::desc(max_streak)) |>
    dplyr::mutate(Rank = dplyr::min_rank(dplyr::desc(max_streak)),
                  Rank_Streak = dplyr::case_when(
                    Rank == 1 ~ "1st",
                    Rank == 2 ~ "2nd",
                    Rank == 3 ~ "3rd",
                    TRUE ~ paste0(Rank, "th")
                  )
    )

  ## Joining the teams_factor ranking to the streak dataframe
  streak_with_labels <- streak_with_labels |>
    dplyr::inner_join(teams_factor |>
                        dplyr::select(Team, Rank_Streak),
                      by = "Team")

  ## Getting the vector of ordered teams
  teams_factor <- teams_factor  |>
    dplyr::select(1)  |>
    unlist()

  ### Calculating how many games each team has played (maximum number of games per team) ----
  max_games <- streak  |>
    dplyr::group_by(Team)  |>
    dplyr::summarise(max_games = max(Game))

  ###Creating charts for each team ----

  purrr::map(teams_factor, function(x) {

    team_data <- streak_with_labels[streak_with_labels$Team == x,]        # store team data in a new variable
    max_strk_team <- max(team_data$Streak)                                # calculate max streak for the team
    min_strk_team <- min(team_data$Streak)                                # calculate min streak for the team

    team_data |>
      highcharter::hchart(showInLegend = FALSE,
                          type = "scatter",
                          highcharter::hcaes(x = Game,
                                             y = Streak),
                          marker = list(enabled = FALSE),
                          color = "white",
                          fillOpacity = 0.4,
                          name = "Streak") |>
      highcharter::hc_add_series(team_data[team_data$Label == "Best Streak", ],
                                 type = "areaspline",
                                 highcharter::hcaes(x = Game,
                                                    y = Streak),
                                 color = "darkblue",
                                 fillColor = "#1B70F7",
                                 marker = list(symbol = "triangle",
                                               radius = 4,
                                               lineWidth = 1),
                                 showInLegend = TRUE,
                                 Opacity = 0.4,
                                 zIndex = 3,
                                 name = "Last best streak") |>
      highcharter::hc_add_series(team_data[team_data$Label == "Worst Streak", ],
                                 type = "areaspline",
                                 highcharter::hcaes(x = Game,
                                                    y = Streak),
                                 color = "darkred",
                                 negativeFillColor = "#D96704",
                                 marker = list(symbol = "triangle-down",
                                               radius = 4,
                                               lineWidth = 1),
                                 showInLegend = TRUE,
                                 Opacity = 1,
                                 zIndex = 3,
                                 name = "Last worst streak") |>
      highcharter::hc_tooltip(useHTML = TRUE,
                              headerFormat = "",
                              pointFormat = "<b>Team:</b> {point.Team} <br>
                                            <b>Date:</b> {point.Date} <br>
                                            <b>Game:</b> {point.Game} <br>
                                            <b>W-L:</b> {point.Record} <br>
                                            <b>Streak:</b> {point.Streak} <br>",
                              borderWidth = 1,
                              borderColor = "#000000")  |>
      highcharter::hc_add_theme(highcharter::hc_theme_smpl())  |>
      # X axis definition
      highcharter::hc_xAxis(title = list(text = "Games"),
                            tickInterval = "1",
                            min = 1,
                            max = max(max_games$max_games))  |>
      # Y axis definition
      highcharter::hc_yAxis(title = list(text = "Games streak"),
                            #tickInterval = y_interval,
                            min = min_streak,
                            max = max_streak)  |>
      highcharter::hc_title(text = paste0(x, "<span style=\"background-color:#002d73\"> - Games Streak </span>"))  |>
      highcharter::hc_subtitle(text =
                                 if (length(teams_factor) > 1) {
                                   paste0(year, " Season. After ", max_games[max_games$Team == x, 2], " games played.", "<br>",
                                          "Ranked as ", unique(team_data$Rank_Streak[team_data$Team == x]), " with a best streak")
                                 } else {
                                   paste0(year, " Season. After ", max_games[max_games$Team == x, 2], " games played")
                                 })  |>
      # adding credits and date when the chart was build
      highcharter::hc_credits(enabled = TRUE,
                              text = paste0("Source: Baseball Reference. Using 'bbgraphsR' R package. Retrieved on: ",
                                            lubridate::with_tz(Sys.time(), "US/Eastern")  |>
                                              format("%Y-%m-%d %H:%M %Z")))  |>
      # enable exporting option
      highcharter::hc_exporting(enabled = TRUE)
  }
  )  |>

    # faceting all charts
    highcharter::hw_grid(rowheight = 400,
                         ncol = viz_col) |>
    htmltools::browsable()

}
