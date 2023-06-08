





viz_wlp <- function(from_season, until_season = NULL, league = "all", fran_tm = "franchise") {

  if (is.null(until_season)) {

    until_season <- from_season # meaning just 1 season will be retrieved

  } else {
    seasons <- seq(from_season, until_season, by = 1)
  }

  message(paste0("Getting W-L data about teams/franchises that played between ", from_season, " to ", until_season, " ..."))
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

  # Swap "primary" and "secondary" colors of some teams
  swap_indices <- c(1, 2, 5, 11, 16, 17, 21, 25, 27, 28)
  # 1 ARI, 2 ATL, 5 CHC, 11 HOU, 16 MIL, 17 MIN, 21 PHI, 25 SEA, 27 TBR, 28 TEX

  temp <- teamcolors$primary[swap_indices]
  teamcolors$primary[swap_indices] <- teamcolors$secondary[swap_indices]
  teamcolors$secondary[swap_indices] <- temp

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

  ### Create df teams_meta with all the info about them and joining the teamscolors df
  teams_meta <- teams_meta |>
    dplyr::left_join(teamcolors, by = "name") |>
    dplyr::rename(Season = yearID,
                  Team = teamIDBR,
                  TeamName = name,
                  Franchise = franchName) |>
    dplyr::select(Season, Team, TeamName, franchID, Franchise, active, lgID, divID, DivWin, WCWin, LgWin, WSWin, primary, secondary)

  teams_meta$primary[teams_meta$TeamName == "Brooklyn Dodgers"] <- "#082984"
  teams_meta$secondary[teams_meta$TeamName == "Brooklyn Dodgers"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "Boston Braves"] <- "#C8102E"
  teams_meta$secondary[teams_meta$TeamName == "Boston Braves"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "New York Giants"] <- "#FF3E00"
  teams_meta$secondary[teams_meta$TeamName == "New York Giants"] <- "#000000"
  teams_meta$primary[teams_meta$TeamName == "Philadelphia Athletics"] <- "#150360"
  teams_meta$secondary[teams_meta$TeamName == "Philadelphia Athletics"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "Kansas City Athletics"] <- "#00843D"
  teams_meta$secondary[teams_meta$TeamName == "Kansas City Athletics"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "California Angels"] <- "#001E40"
  teams_meta$secondary[teams_meta$TeamName == "California Angels"] <- "#C1033B"
  teams_meta$primary[teams_meta$TeamName == "Tampa Bay Devil Rays"] <- "#02B189"
  teams_meta$secondary[teams_meta$TeamName == "Tampa Bay Devil Rays"] <- "#D2BC50"
  teams_meta$primary[teams_meta$TeamName == "Los Angeles Angels of Anaheim" |
                       teams_meta$TeamName == "Anaheim Angels"] <- "#BA0021"
  teams_meta$secondary[teams_meta$TeamName == "Los Angeles Angels of Anaheim" |
                         teams_meta$TeamName == "Anaheim Angels"] <- "#003263"
  teams_meta$primary[teams_meta$TeamName == "Cleveland Guardians"] <- "#00385D"
  teams_meta$secondary[teams_meta$TeamName == "Cleveland Guardians"] <- "#E50022"
  teams_meta$primary[teams_meta$TeamName == "Florida Marlins"] <- "#00A3B3"
  teams_meta$secondary[teams_meta$TeamName == "Florida Marlins"] <- "#000000"
  teams_meta$primary[teams_meta$TeamName == "Montreal Expos"] <-"#67ABE5"
  teams_meta$secondary[teams_meta$TeamName == "Montreal Expos"] <-"#E4002B"
  teams_meta$primary[is.na(teams_meta$primary)] <- "#000000"
  teams_meta$secondary[is.na(teams_meta$secondary)] <- "#E6E3E3"


  # Join "teams_meta" to wl
  wl <- wl |>
    dplyr::left_join(teams_meta, by = c("Team", "Season")) |>
    dplyr::mutate(ended = ifelse(WSWin == "Y", "WS Champs",
                                 ifelse(LgWin == "Y", paste0(lgID, " Champs"),
                                        ifelse(DivWin == "Y", paste0("Won ", lgID, " ", divID),
                                               ifelse(WCWin == "Y", "Wild Card", "Eliminated")
                                               )
                                        )
                                 )
                  )

  ### If the Lahman package doesn't have the data for the current season, then the data for the teams in the current Season needs to be filled in ----

  if (is.na(wl[max(wl$Season), 6])) {

    # Create a temp data frame with the second last Season data
    temp_df <- wl |>
      dplyr::filter(Season == max(wl$Season) - 1) |>
      dplyr::mutate(Season = max(wl$Season))  # Change the Season to the last Season

    # Join this data frame to the original data frame to fill the missing data of the last Season
    wl <- wl |>
      dplyr::left_join(temp_df, by = c("Team", "Season"), suffix = c("", ".y")) |>
      dplyr::mutate(
        TeamName = ifelse(is.na(TeamName), TeamName.y, TeamName),
        franchID = ifelse(is.na(franchID), franchID.y, franchID),
        Franchise = ifelse(is.na(Franchise), Franchise.y, Franchise),
        active = ifelse(is.na(active), active.y, active),
        lgID = ifelse(is.na(lgID), lgID.y, lgID),
        divID = ifelse(is.na(divID), divID.y, divID),
        primary = ifelse(is.na(primary), primary.y, primary),
        secondary = ifelse(is.na(secondary), secondary.y, secondary)
      ) |>
      dplyr::select(-ends_with(".y"))  # Remove the extra columns

  }



  ### Creating an ordered vector (not factor) of teams based on accumulated Runs Differential. ----
  ### NOTE: This is because 'highcharter' hc_grid function plots charts in the order they are created and not based on factors (as ggplot)

  if (fran_tm == "team") {

    teams_factor <- wl  |>
      dplyr::group_by(Team)  |>
      dplyr::summarise(W = sum(W), L = sum(L))  |>
      dplyr::mutate(Wp_global = round(W / (W+L), 3))  |>
      dplyr::arrange(dplyr::desc(Wp_global))

  } else if (fran_tm == "franchise") {

    teams_factor <- wl  |>
      dplyr::group_by(franchID)  |>
      dplyr::summarise(W = sum(W), L = sum(L))  |>
      dplyr::mutate(Wp_global = round(W / (W+L), 3))  |>
      dplyr::arrange(dplyr::desc(Wp_global))

  }

  message("The teams/franchises are sorted in descending order by their W% in the whole period, as shown here below (defining their order in the chart)")
  teams_factor |>
    print(n = nrow(teams_factor))

  ## Join the global W% of each team/franchise in the periodof analysis to the wl dataframe

  if (fran_tm == "team") {

    wl <- wl |>
    dplyr::left_join(teams_factor[, c(1,4)], by = "Team")

  } else if (fran_tm == "franchise") {

    wl <- wl |>
      dplyr::left_join(teams_factor[, c(1,4)], by = "franchID")
  }

  ## Converting the teams_factor to a vector
  teams_factor <- teams_factor |>
    dplyr::select(1)  |>
    unlist()

  ### Creating charts for each team ----

  purrr::map(teams_factor, function(x) {

    if (fran_tm == "team") {

      teams_data <- wl[wl$Team == x, ]          # sub-setting wl data frame per team

    } else if (fran_tm == "franchise") {

      teams_data <- wl[wl$franchID == x, ]      # sub-setting wl data frame per franchise

    }

    max_wlpct <- max(teams_data$WLpct)    # calculate max WL% in a season for the team
    min_wlpct <- min(teams_data$WLpct)    # calculate min WL% in a season for the team
    font_color <- ifelse(teams_data$Wp_global[1] >= 0.5,
                         "darkgreen",
                         "red")           # defines the font color for the W% in the subtitle


    teams_data  |>
      # SP line plot representing the W% per season
      highcharter::hchart(showInLegend = FALSE,
                          type = "spline",
                          highcharter::hcaes(x = Season,
                                             y = WLpct),
                          Opacity = 0.9,
                          name = "W%",
                          color = teams_data$primary[length(teams_data$primary)],
                          zIndex = 10) |>
      highcharter::hc_plotOptions(spline = list(lineWidth = 4)) |>  # adjust line width here
      # adding markers if the team ended best as a Wild Card
      highcharter::hc_add_series(teams_data[teams_data$ended == "Wild Card" & complete.cases(teams_data), ],
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
                                 name = "Wild Card") |>
      # adding markers if the team ended best as a Division Winner
      highcharter::hc_add_series(teams_data[grepl("^Won", teams_data$ended) & complete.cases(teams_data), ],
                                 type = "scatter",
                                 highcharter::hcaes(x = Season,
                                                    y = WLpct),
                                 marker = list(symbol = "square",
                                               lineColor = "black",
                                               fillColor = "lightblue",
                                               radius = 2,
                                               lineWidth = 1),
                                 showInLegend = TRUE,
                                 Opacity = 1,
                                 zIndex = 15,
                                 name = "Won Div") |>
      # adding markers if the team clinched the playoffs as a League Champ
      highcharter::hc_add_series(teams_data[grepl("L Champs$", teams_data$ended) & complete.cases(teams_data), ],
                                 type = "scatter",
                                 highcharter::hcaes(x = Season,
                                                    y = WLpct),
                                 marker = list(symbol = "triangle",
                                               lineColor = "black",
                                               fillColor = "darkblue",
                                               radius = 2,
                                               lineWidth = 1),
                                 showInLegend = TRUE,
                                 Opacity = 1,
                                 zIndex = 15,
                                 name = "League Champs") |>
      # adding stars if the team won the World Series
      highcharter::hc_add_series(teams_data[teams_data$ended == "WS Champs" & complete.cases(teams_data), ],
                                 type = "scatter",
                                 highcharter::hcaes(x = Season,
                                                    y = WLpct),
                                 color = "blue",
                                 marker = list(symbol = "diamond",
                                               lineColor = "black",
                                               fillColor = "gold",
                                               radius = 3,
                                               lineWidth = 1),
                                 showInLegend = TRUE,
                                 Opacity = 1,
                                 zIndex = 15,
                                 name = "WS Champs") |>
      highcharter::hc_tooltip(useHTML = TRUE,
                              headerFormat = "",
                              pointFormat = "<b>Season:</b> {point.Season} <br>
                                            <b>Team:</b> {point.Team} <br>
                                            <b>Name:</b> {point.TeamName} <br>
                                            <b>W-L:</b> {point.W}-{point.L} <br>
                                            <b>W%:</b> {point.WLpct} <br>
                                            <b>Ended as:</b> {point.ended} <br>",
                              borderWidth = 1,
                              borderColor = "#000000") |>
      highcharter::hc_add_theme(highcharter::hc_theme_smpl()) |>
      # X axis definition
      highcharter::hc_xAxis(plotLines = list(list(value = 0.5, color = "red", width = 2,
                                                  dashStyle = "shortdash")),
                            zIndex = 2,
                            min = min(wl$Season),
                            max = max(wl$Season)) |>
      # Y axis definition
      highcharter::hc_yAxis(title = list(text = "W%"),
                            plotLines = list(list(value = 0.5, color = "blue", width = 1,
                                                  dashStyle = "shortdash")),
                            min = min(wl$WLpct),
                            max = max(wl$WLpct),
                            tickInterval = 0.1) |>
      highcharter::hc_title(text = paste0(x, "<strong><span style='font-size: 20px; color: black </span></strong>")) |>
      highcharter::hc_subtitle(text = htmltools::HTML(
        paste0("W% in Reg. season (" , min(wl$Season), "-", max(wl$Season), "): <strong><span style='color: ", font_color, ";'>", teams_data$Wp_global[1]), "</span></strong>")) |>
      # # adding credits and date when the chart was build
      highcharter::hc_credits(enabled = TRUE,
                              text = paste0("Source: Fangraph. Using 'bbraphsR' package. Retreived on: ",
                                            lubridate::with_tz(Sys.time(), "US/Eastern") |>
                                              format("%Y-%m-%d %H:%M %Z"))) |>
      # enable exporting option
      highcharter::hc_exporting(enabled = TRUE)

  }) |>

    highcharter::hw_grid(rowheight = 300, ncol = viz_col) |>
    htmltools::browsable()

}
