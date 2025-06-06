#' @title Graph of Accumulated Runs Differential for teams
#' @description
#' Function to plot the accumulated runs differential for an MLB team (or a group of them) on a specific MLB season
#'
#' @param team a string input, Baseball Reference Team abbreviation, a division or the whole League
#' @param year a numeric value, MLB season to be analyzed
#'
#' @export
#'
#' @importFrom baseballr bref_standings_on_date
#' @importFrom pbapply pbsapply pblapply
#' @importFrom dplyr select group_by mutate ungroup summarise arrange case_when min_rank row_number inner_join
#' @importFrom tidyr separate unite
#' @importFrom purrr map
#' @importFrom highcharter hchart hcaes hc_tooltip hc_add_theme hc_theme_smpl hc_xAxis hc_yAxis hc_title hc_subtitle hc_credits hc_exporting hw_grid hc_add_series
#' @importFrom htmltools browsable
#' @importFrom lubridate with_tz
#'
#' @return A areaspline-type chart with the accumulated run differential for the Team(s) along the season analyzed
#'
#' @examples
#' viz_rd("AL West", 2021)
#' ## returns an RD chart for all the AL West Teams in 2021, in descending order
#' \dontrun{
#' viz_rd_wpct("BOS", 2023)
#' ## returns an RD chart for Boston Red Sox in the 2023 Season
#' viz_rd_wpct("NL Central", 2008)
#' ## returns an RD chart for all the NL Central Teams in 2008, in descending order
#'  }

viz_rd_wpct <- function(team, year) {

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
    message(paste0("Retrieving teams that played in ", team, " in ", year, "..."))
    teams <- baseballr::bref_standings_on_date(paste0(year,"-04-30"), team) |>
      as.data.frame() |>
      select(1) |>
      unlist()

  } else if (team == "MLB") {

    # All MLB teams in year
    mlb <- c("AL Overall", "NL Overall")

    message(paste0("Retrieving teams that played in ", team, " in ", year, "..."))
    teams <- pbapply::pbsapply(mlb, baseballr::bref_standings_on_date, date = paste0(year,"-04-30"))
    teams <- c(teams[[1,1]], teams[[1,2]])

  } else {
    # Only one team
    teams <- team
  }

  ### Get the game's results of each team to be visualized ----

  message("Getting games' data ...")

  # Gets the team's results tables for each team
  rd <- pblapply(teams, baseballr::bref_team_results, year)
  # Binds tables for all teams into one data frame
  rd <- do.call("rbind", rd)

  # change Game variable to numeric
  rd$Gm <- as.numeric(rd$Gm)
  # change Runs Allowed variable to numeric
  rd$RA <- as.numeric(rd$RA)

  ### Tidying the `rd` data frame ----

  rd <- rd |>
    tidyr::separate(Date, c("wd", "Date"), sep = ", ") |>     # wd = weekday
    tidyr::unite(Date, c("Date", "Year"), sep = ", ") |>
    tidyr::separate(Record, c("W", "L"), sep = "-", remove = FALSE) |>
    dplyr::select(Gm, Date, Tm, R, RA, Record, Rank, W, L)  |>
    dplyr::group_by(Tm) |>
    dplyr::mutate(RD = R - RA,
                  cum_RD = cumsum(RD),
                  Wpct = as.numeric(W)/(as.numeric(W)+as.numeric(L))) |>
    dplyr::arrange(Gm) |>
    dplyr::ungroup()

  names(rd)[c(1,3)] <- c("Game", "Team")
  rd$W <- as.numeric(rd$W)
  rd$L <- as.numeric(rd$L)

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
  min_RD <- floor(min(rd$cum_RD)/10)*10
  max_RD <- ceiling(max(rd$cum_RD)/10)*10

  # interval in x axis to allow a better adjustment of x min and max
  if (max_RD <= 120) {
    x_interval <- 25
  } else if (max_RD <= 250) {
    x_interval <- 50
  } else if (max_RD <= 350) {
    x_interval <- 100
  } else {
    x_interval <- 200
  }

  ### Creating an ordered vector (not factor) of teams based on accumulated Runs Differential. ----
  ### NOTE: This is because 'highcharter' hc_grid function plots charts in the order they are created and not based on factors (as ggplot)

  teams_factor <- rd  |>
    dplyr::group_by(Team)  |>
    dplyr::filter(Game == max(Game)) |>
    ungroup() |>
    dplyr::arrange(dplyr::desc(Wpct), dplyr::desc(cum_RD))  |>
    dplyr::mutate(
      Rank = dplyr::min_rank(dplyr::desc(Wpct)),
      Rank_Wpct = dplyr::case_when(
        Rank == 1 ~ "1st",
        Rank == 2 ~ "2nd",
        Rank == 3 ~ "3rd",
        TRUE ~ paste0(Rank, "th")
        )
      )


  ## Printing the teams factor in the console, with the ranking, Wpct and cumRD at their last game
  if (nrow(teams_factor) > 1) {
    # If there is more than one team being analyzed, show a message to indicate that the chart will be ordered by descending order of RD
    message(paste0("By ", as.character(max(as.Date(rd$Date, format = "%b %d, %Y"))), ", the team(s) are ranked based on their Winning percentage, as shown here below (defining their order in the chart)"))
    teams_factor |>
      dplyr::select(Rank_Wpct, Team, W, L, Wpct, cum_RD) |>
      print(n = nrow(teams_factor))
  } else {
    # If there is only one team, show a message to indicate their results
    message(paste0("By ", as.character(max(as.Date(rd$Date, format = "%b %d, %Y"))), ", ", team, " had scored and allowed runs as shown here below"))
    teams_factor |>
      dplyr::select(Team, W, L, Wpct, cum_RD) |>
      print()
  }

  ## Joining the RD's ranking to the rd dataframe
  rd <- rd  |>
    dplyr::inner_join(teams_factor  |>
                        dplyr::select(Team, Rank_Wpct),
                      by = "Team")

  ### Calculating how many games each team has played (maximum number of games per team) ----
  max_games <- rd  |>
    group_by(Team)  |>
    summarise(max_games = max(Game))

  ## Getting the vector of ordered teams
  teams_factor <- teams_factor  |>
    dplyr::select(3)  |>
    unlist()

  ###Creating charts for each team ----

  map(teams_factor, function(x) {

    team_data <- rd[rd$Team == x,]        # store team data in a new variable
    max_diff <- max(team_data$cum_RD)     # calculate max cum_RD for the team
    min_diff <- min(team_data$cum_RD)     # calculate min cum_RD for the team

    team_data  |>
      # adding the area chart for the accumulated run differential
      highcharter::hchart(showInLegend = FALSE,
                          type = "scatter",
                          highcharter::hcaes(x = cum_RD,
                                             y = Wpct,
                                             color = Game),
                          zIndex = 5,
                          Opacity = 0.5,
                          marker = list(symbol = "circle",
                                        radius = 3,
                                        lineWidth = .5),
                          name = "By game") |>
      # adding points on the maximum of run differentials
      highcharter::hc_add_series(team_data,
                                 type = "line",
                                 highcharter::hcaes(x = cum_RD,
                                                    y = Wpct),
                                 color = "darkgray",
                                 witdh = 0.5,
                                 # marker = list(symbol = "triangle",
                                 #               radius = 4,
                                 #               lineWidth = 1),
                                 showInLegend = TRUE,
                                 Opacity = 0.5,
                                 zIndex = 3,
                                 name = "Game sequence") |>
      highcharter::hc_tooltip(useHTML = TRUE,
                              headerFormat = "",
                              pointFormat = "<b>Team:</b> {point.Team} <br>
                                            <b>Date:</b> {point.Date} <br>
                                            <b>Game:</b> {point.Game} <br>
                                            <b>RD:</b> {point.cum_RD} <br>
                                            <b>W%</b>: {point.Wpct} <br>
                                            <b>W-L:</b> {point.Record}",
                              borderWidth = 0.5,
                              borderColor = "#000000")  |>
      highcharter::hc_add_theme(hc_theme_smpl())  |>
      # X axis definition
      highcharter::hc_xAxis(title = list(text = "Run Differential"),
                            tickInterval = "1",
                            min = min_RD,
                            max = max_RD,
                            tickInterval = x_interval,
                            plotLines = list(
                              list(
                                value = 0,
                                color = "gray",
                                width = 0.5,
                                zIndex = 2
                              )
                            )
                            )  |>
      # Y axis definition
      highcharter::hc_yAxis(title = list(text = "Winning %"),
                            min = 0,
                            max = 1,
                            tickInterval = 0.1,
                            plotLines = list(
                              list(
                                value = 0.5,
                                color = "gray",
                                width = 0.5,
                                zIndex = 2
                              )
                            )
                            )  |>
      highcharter::hc_title(text = paste0(x, "<span style=\"background-color:#002d73\"> - RD vs W% </span>"))  |>
      highcharter::hc_subtitle(text =
                                 if (length(teams_factor) > 1) {
                                   paste0(year, " Season. After ", max_games$max_games[max_games$Team == x], " games played.", "<br>",
                                          "Ranked as ", unique(rd$Rank_Wpct[rd$Team == x]), " in Wpct in ", team)
                                 } else {
                                   paste0(year, " Season. After ", max_games$max_games[max_games$Team == x], " games played")
                                 })  |>
      # adding credits and date when the chart was build
      highcharter::hc_credits(enabled = TRUE,
                              text = paste0("Source: Baseball Reference. Using 'baseballr' R package. Retrieved on: ",
                                            with_tz(Sys.time(), "US/Eastern")  |>
                                              format("%Y-%m-%d %H:%M %Z")))  |>
      # enable exporting option
      highcharter::hc_exporting(enabled = TRUE)
  }
  )  |>

    # faceting all charts
    highcharter::hw_grid(rowheight = 400,
                         ncol = viz_col) |>
    browsable()
}
