





viz_wlp <- function(from_season, until_season = NULL, league = "all", fran_tm = "team") {

  if (is.null(until_season)) {

    until_season <- from_season # meaning just 1 season will be retrieved

  } else {
    seasons <- seq(from_season, until_season, by = 1)
  }

  wl <- baseballr::fg_team_pitcher(x = from_season, y = until_season, league = league, ind = 1) |>
    dplyr::select(Season, Team, W, L) |>
    dplyr::mutate(WLpct = round(W / (W+L), 3)) |>
    dplyr::group_by(Team) |>
    dplyr::arrange(Season) |>
    dplyr::ungroup()

  wl$Season <- as.numeric(wl$Season)

  # Create a table of colors by team and also if they clinched the playoffs and won the World Series

  Teams_Lahman <- read.csv("data/Teams.csv")[ , -1]
  Teams_Franchises <- read.csv("data/TeamsFranchises.csv")[ , -1]

  teams_meta <- Teams_Lahman  |>
    dplyr::left_join(Teams_Franchises, by = "franchID") |>
    dplyr::select(yearID, teamIDBR, name, franchID, franchName, active, lgID, divID, DivWin, WCWin, LgWin, WSWin) |>
    dplyr::filter(active == "Y",
                  lgID == "AL" | lgID == "NL")


  teams_meta$name[teams_meta$teamIDBR == "LAA"] <- "Los Angeles Angels"

  teamcolors <- read.csv("data/teamcolors.csv")[ , -1] |>
    dplyr::filter(league == "mlb")

  # Replace "primary" color of some teams by its "secondary" color
  teamcolors[c(1, 2, 5, 11, 16, 17, 21, 25, 27, 28), 3] <- teamcolors[c(1, 2, 5, 11, 16, 17, 21, 25, 27, 28), 4]
  # 1 ARI, 2 ATL, 5 CHC, 11 HOU, 16 MIL, 17 MIN, 21 PHI, 25 SEA, 27 TBR, 28 TEX

  teams_meta <- teams_meta |>
    dplyr::left_join(teamcolors, by = "name") |>
    dplyr::rename(Season = yearID,
                  Team = teamIDBR,
                  TeamName = name,
                  Franchise = franchName) |>
    dplyr::select(Season, Team, TeamName, franchID, Franchise, active, lgID, divID, DivWin, WCWin, LgWin, WSWin, primary)

  teams_meta$primary[teams_meta$TeamName == "Brooklyn Dodgers"] <- "#082984"
  teams_meta$primary[teams_meta$TeamName == "Boston Braves"] <- "#C8102E"
  teams_meta$primary[teams_meta$TeamName == "New York Giants"] <- "#FF3E00"
  teams_meta$primary[teams_meta$TeamName == "Philadelphia Athletics"] <- "#150360"
  teams_meta$primary[teams_meta$TeamName == "Kansas City Athletics"] <- "#C43922"
  teams_meta$primary[teams_meta$TeamName == "California Angels"] <- "#001E40"
  teams_meta$primary[teams_meta$TeamName == "Tampa Bay Devil Rays"] <- "#02B189"
  teams_meta$primary[teams_meta$TeamName == "Los Angeles Angels of Anaheim"] <- "#BA0021"
  teams_meta$primary[teams_meta$TeamName == "Anaheim Angels"] <- "#BA0021"
  teams_meta$primary[teams_meta$TeamName == "Cleveland Guardians"] <- "#00385D"
  teams_meta$primary[teams_meta$TeamName == "Florida Marlins"] <- "#00A3B3"
  teams_meta$primary[teams_meta$TeamName == "Montreal Expos"] <-"#67ABE5"
  teams_meta$primary[is.na(teams_meta$primary)] <- "#000000"


  # Join "primary" color column to wl data.frame
  wl <- wl |>
    dplyr::left_join(teams_meta, by = c("Team", "Season"))

  # If the Lahman package doesn't have the data for the current season, then the data for the teams in the current Season needs to be filled in

  if (is.na(wl[max(wl$Season), 6])) {

    # Create a temp data frame with the second last Season data
    temp_df <- wl %>%
      dplyr::filter(Season == max(wl$Season) - 1) %>%
      dplyr::mutate(Season = max(wl$Season))  # Change the Season to the last Season

    # Join this data frame to the original data frame to fill the missing data of the last Season
    wl <- wl %>%
      dplyr::left_join(temp_df, by = c("Team", "Season"), suffix = c("", ".y")) %>%
      dplyr::mutate(
        TeamName = ifelse(is.na(TeamName), TeamName.y, TeamName),
        franchID = ifelse(is.na(franchID), franchID.y, franchID),
        Franchise = ifelse(is.na(Franchise), Franchise.y, Franchise),
        active = ifelse(is.na(active), active.y, active),
        lgID = ifelse(is.na(lgID), lgID.y, lgID),
        divID = ifelse(is.na(divID), divID.y, divID),
        primary = ifelse(is.na(primary), primary.y, primary),
      ) %>%
      dplyr::select(-ends_with(".y"))  # Remove the extra columns

  }

  ### Determine the columns for the grid chart, based on number of teams ----

  if (length(seasons) == 1 | length(seasons) == 3 | length(seasons) == 5) {
    viz_col <- 1
  } else if (length(seasons) == 2 | length(seasons) == 4 | length(seasons) == 6) {
    viz_col <- 2
  } else if (length(seasons) > 6 & length(seasons) < 10) {
    viz_col <- 3
  } else if (length(seasons) >= 10) {
    viz_col <- 5
  }

  ### Creating an ordered vector (not factor) of teams based on accumulated Runs Differential. ----
  ### NOTE: This is because 'highcharter' hc_grid function plots charts in the order they are created and not based on factors (as ggplot)

  if (fran_tm == "team") {

    teams_factor <- wl  |>
      dplyr::group_by(Team)  |>
      dplyr::summarise(W = sum(W), L = sum(L))  |>
      dplyr::mutate(WLpct = W/(W+L))  |>
      dplyr::arrange(dplyr::desc(WLpct))

  } else if (fran_tm == "franchise") {

    teams_factor <- wl  |>
      dplyr::group_by(franchID)  |>
      dplyr::summarise(W = sum(W), L = sum(L))  |>
      dplyr::mutate(WLpct = W/(W+L))  |>
      dplyr::arrange(dplyr::desc(WLpct))

  }

  message("The team(s) are sorted in descending order by their W-L% in the whole period, as shown here below (defining their order in the chart)")
  teams_factor |>
    print(n = nrow(teams_factor))

  teams_factor <- teams_factor |>
    dplyr::select(1)  |>
    unlist()

  ###Creating charts for each team ----

  purrr::map(teams_factor, function(x) {

    if (fran_tm == "team") {

      teams_data <- wl[wl$Team == x, ]      # store team data in a new variable

    } else if (fran_tm == "franchise") {

      teams_data <- wl[wl$franchID == x, ]      # store team data in a new variable

    }

    max_wlpct <- max(teams_data$WLpct)    # calculate max WL% in a season for the team
    min_wlpct <- min(teams_data$WLpct)    # calculate min WL% in a season for the team

    teams_data  |>
      # Scatter plot for the two metrics: x axis are batting and y axis pitching
      highcharter::hchart(showInLegend = FALSE,
                          type = "spline",
                          highcharter::hcaes(x = Season,
                                             y = WLpct),
                          Opacity = 0.9,
                          name = "W-L%",
                          color = teams_data$primary[1],
                          zIndex = 10) |>
      highcharter::hc_plotOptions(spline = list(lineWidth = 4)) |>  # adjust line width here
      # adding markers if the team clinched the playoffs
      highcharter::hc_add_series(teams_data[teams_data$DivWin == "Y" |
                                              teams_data$WCWin == "Y" |
                                              teams_data$LgWin == "Y", ],
                                 type = "scatter",
                                 highcharter::hcaes(x = Season,
                                                    y = WLpct),
                                 marker = list(symbol = "circle",
                                               lineColor = "black",
                                               fillColor = "white",
                                               radius = 2,
                                               lineWidth = 1),
                                 showInLegend = TRUE,
                                 Opacity = 1,
                                 zIndex = 15,
                                 name = "Clinched playoffs")  |>
      # # adding stars if the team won the World Series
      highcharter::hc_add_series(teams_data[teams_data$WSWin == "Y" , ],
                                 type = "scatter",
                                 highcharter::hcaes(x = Season,
                                                    y = WLpct),
                                 color = "blue",
                                 marker = list(symbol = "diamond",
                                               lineColor = "darkgray",
                                               fillColor = "gold",
                                               radius = 3,
                                               lineWidth = 1),
                                 showInLegend = TRUE,
                                 Opacity = 1,
                                 zIndex = 15,
                                 name = "World Champion")  |>
      highcharter::hc_tooltip(useHTML = TRUE,
                              headerFormat = "",
                              pointFormat = "<b>Season:</b> {point.Season} <br>
                                            <b>Team:</b> {point.Team} <br>
                                            <b>Name:</b> {point.TeamName} <br>
                                            <b>W-L:</b> {point.W}-{point.L} <br>
                                            <b>W-L%:</b> {point.WLpct}",
                              borderWidth = 1,
                              borderColor = "#000000")  |>
      highcharter::hc_add_theme(highcharter::hc_theme_smpl())  |>
      # X axis definition
      highcharter::hc_xAxis(plotLines = list(list(value = 0.5, color = "red", width = 2,
                                                  dashStyle = "shortdash")),
                            zIndex = 2,
                            min = min(wl$Season),
                            max = max(wl$Season))  |>
      # Y axis definition
      highcharter::hc_yAxis(plotLines = list(list(value = 0.5, color = "blue", width = 1,
                                                  dashStyle = "shortdash")),
                            min = min(wl$WLpct),
                            max = max(wl$WLpct),
                            tickInterval = 0.1) |>
      highcharter::hc_title(text = paste0(x, "<span style=\"background-color:#002d73\"> - WL% </span>"))  |>
      highcharter::hc_subtitle(text = paste0("In Reg. season, from " , min(wl$Season), " until ", max(wl$Season))) |>
      # # adding credits and date when the chart was build
      highcharter::hc_credits(enabled = TRUE,
                              text = paste0("Source: Fangraph. Using 'bbraphsR' package. Retreived on: ",
                                            lubridate::with_tz(Sys.time(), "US/Eastern")  |>
                                              format("%Y-%m-%d %H:%M %Z")))  |>
      # enable exporting option
      highcharter::hc_exporting(enabled = TRUE)

  }) |>

    highcharter::hw_grid(rowheight = 300, ncol = viz_col) |>
    htmltools::browsable()

}
