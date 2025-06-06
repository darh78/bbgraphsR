#' Scrapes MLB teams winning percentage from fangraphs.com along a period of years (at least 2 consecutive seasons) and visualizes a timeline for each team or franchise along that period
#'
#' @param from_season a numeric value, first season you want to analyze (Should be at least a year before the last/current season)
#' @param until_season a numeric value, last season you want to analyze (Default value is 1 year after the input given in the 'from_season' argument)
#' @param league a string input, Option for limiting results to different leagues or overall results. Options are "al", "nl", or "all" (default value).
#' @param fran_tm a string input, Option for indicating if the plot will be grouped by teams or by franchises (useful if period of season is longer)
#' @keywords MLB, performance
#' @importFrom highcharter highchart hchart hc_title hc_subtitle hc_credits hc_yAxis hc_xAxis hc_add_theme hcaes hc_theme_smpl hc_add_series hc_tooltip hc_exporting hc_plotOptions hw_grid
#' @importFrom pbapply pblapply
#' @importFrom htmltools browsable
#' @importFrom lubridate with_tz
#' @importFrom baseballr fg_team_pitcher
#' @importFrom dplyr select mutate rename arrange desc filter pull left_join
#' @importFrom purrr map
#' @export viz_wlp_years
#'
#' @examples viz_wlp_years(2020, 2023)


viz_wlp_years <- function(from_season, until_season = from_season + 1, league = "all", fran_tm = "franchise") {

  ### Check if arguments are valid ----

  # Check from_season is either NULL (default value) or is in the numeric format
  if (!is.null(from_season) && !is.numeric(from_season)) {

    stop(paste0("The 'from_season' argument must be a numeric value representing the first year to analyze. It must be between 1876 and less than 'until_season' argument"))

  }

  # Check if until_season is in the numeric format
  if (!is.numeric(until_season)) {

    stop(paste0("The 'until_season' argument must be a numeric value representing the last year to analyze. It must greater than the 'from_season' argument and less than the current/last season"))

  }

  # Check if from_season is less than until_season
  if (!(is.numeric(until_season) && is.numeric(from_season) && from_season < until_season)) {
    stop(paste0("The 'from_season' argument must be less than the 'until_season'"))
  }

  # Check if league is within the accepted values
  valid_league <- c("nl", "al", "all")

  if (!(league %in% valid_league)) {
    stop("The 'league' argument must be any of the following possibilities:
         nl, al, all")
  }

  # Check if fran_tm is within the accepted values
  valid_type <- c("franchise", "team")

  if (!(fran_tm %in% valid_type)) {
    stop("The 'fran_tm' argument must be any of the following possibilities:
         franchise or team")
  }

  # Check until_season is in the worst case the current year
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  if (!(!is.null(until_season) && is.numeric(until_season) && until_season >= 1876 && until_season <= current_year)) {
    stop(paste0("The 'until_season' argument must be a numeric value between 1876 and the last/current MLB season"))
  }

  # If all arguments are valid, continue with the function

  ### Defining the years to analyze ----
  if (is.null(from_season)) { # If no starting season has been defined, at least two years will be considered to be analyzed
    from_season <- until_season - 1
  }

  # Define a sequence of years to analyze
  seasons <- seq(from_season, until_season, by = 1)   # Moved out of the else block to be always defined


  ### Getting W-L data in the requested period ----
  message(paste0("Getting W-L data about teams/franchises that played between ", from_season, " to ", until_season, " ..."))

  # Use purrr::map to iterate over the seasons and collect data
  wl <- purrr::map_dfr(seasons, function(season) {
    baseballr::fg_team_pitcher(startseason = as.character(season), endseason = as.character(season), lg = league, ind = 1) |>
      dplyr::select(Season, Team = team_name, W, L) |>
      dplyr::mutate(WLpct = round(W / (W+L), 3))
  })

  # Continue with the original grouping and arranging
  wl <- wl |>
    dplyr::group_by(Team) |>
    dplyr::arrange(Season) |>
    dplyr::ungroup()

  wl$Season <- as.numeric(wl$Season)

  ### Create a table of colors by team and also if they clinched the playoffs and won the World Series ----

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

  if (length(unique(wl$Team)) == 1 | length(unique(wl$Team)) == 3 | length(unique(wl$Team)) == 5) {
    viz_col <- 1
  } else if (length(unique(wl$Team)) == 2 | length(unique(wl$Team)) == 4 | length(unique(wl$Team)) == 6) {
    viz_col <- 2
  } else if (length(unique(wl$Team)) > 6 & length(unique(wl$Team)) < 10) {
    viz_col <- 3
  } else if (length(unique(wl$Team)) >= 10) {
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

  # ### Fill in data for current season in the case the Lahman package doesn't have the data for it ----
  #
  # if (is.na(wl[max(wl$Season), 6])) {
  #
  #   # Create a temp data frame with the second last Season data
  #   temp_df <- wl |>
  #     dplyr::filter(Season == max(wl$Season) - 1) |>
  #     dplyr::mutate(Season = max(wl$Season))  # Change the Season to the last Season
  #
  #   # Join this data frame to the original data frame to fill the missing data of the last Season
  #   wl <- wl |>
  #     dplyr::left_join(temp_df, by = c("Team", "Season"), suffix = c("", ".y")) |>
  #     dplyr::mutate(
  #       TeamName = ifelse(is.na(TeamName), TeamName.y, TeamName),
  #       franchID = ifelse(is.na(franchID), franchID.y, franchID),
  #       Franchise = ifelse(is.na(Franchise), Franchise.y, Franchise),
  #       active = ifelse(is.na(active), active.y, active),
  #       lgID = ifelse(is.na(lgID), lgID.y, lgID),
  #       divID = ifelse(is.na(divID), divID.y, divID),
  #       primary = ifelse(is.na(primary), primary.y, primary),
  #       secondary = ifelse(is.na(secondary), secondary.y, secondary)
  #     ) |>
  #     dplyr::select(-ends_with(".y"))  # Remove the extra columns
  #
  # }


  # Identify rows for the most recent season
  recent_season_rows <- wl %>% dplyr::filter(Season == max(Season))

  # Check if 'TeamName' has any missing values in the recent season
  if (any(is.na(recent_season_rows$TeamName))) {
    # Proceed with the data imputation as needed
    # Create a temp data frame with data from the previous season
    temp_df <- wl |>
      dplyr::filter(Season == (max(wl$Season) - 1)) |>
      dplyr::mutate(Season = max(wl$Season))  # Change the Season to the latest Season

    # Join this data frame to the original data frame to fill the missing data of the latest Season
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

  wl_period <- teams_factor

  ## Join the global W% of each team/franchise in the period of analysis to the wl dataframe

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
      wl_whole <- wl_period[wl_period$Team == x, ]

    } else if (fran_tm == "franchise") {

      teams_data <- wl[wl$franchID == x, ]      # sub-setting wl data frame per franchise
      wl_whole <- wl_period[wl_period$franchID == x, ]

    }

    # Check if teams_data is empty
    if (nrow(teams_data) == 0) {
      return(NULL) # or handle the error as you see fit
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
                            allowDecimals = FALSE, # This will force the x-axis to only use integers
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
        paste("W% in Reg. season (" , min(wl$Season), "-", max(wl$Season), "): <strong><span style='color: ", font_color, ";'>", teams_data$Wp_global[1]), "</span></strong> (", wl_whole$W, "-", wl_whole$L, ")")) |>
      # # adding credits and date when the chart was build
      highcharter::hc_credits(enabled = TRUE,
                              text = paste0("Source: Fangraph. Using 'bbraphsR' package. Retrieved on: ",
                                            lubridate::with_tz(Sys.time(), "US/Eastern") |>
                                              format("%Y-%m-%d %H:%M %Z"))) |>
      # enable exporting option
      highcharter::hc_exporting(enabled = TRUE)

  }) |>

    highcharter::hw_grid(rowheight = 300, ncol = viz_col) |>
    htmltools::browsable()

}
