#' Scrape MLB Standings on a specific season and visualize timelines of Games Behind (GB) and Winning percentage on any division or league
#'
#' This function allows you to scrape the standings from MLB teams for a specific season, and visualize two charts: Game Behind (GB) and Winning percentage of teams during that season.
#' @param lg_div a string input, Baseball Reference Team abbreviation or a division (e.g. NL East, NL Central, NL West, NL Overall, AL East, AL Central, AL West or AL Overall)
#' @param year a numeric value, MLB season to be analyzed
#' @keywords MLB, standings
#' @importFrom highcharter highchart hchart hc_title hc_subtitle hc_credits hc_yAxis hc_xAxis hc_add_theme hcaes hc_theme_smpl highchart hc_add_series hc_tooltip hc_exporting
#' @importFrom pbapply pblapply
#' @importFrom tidyr separate unite
#' @importFrom lubridate year
#' @importFrom baseballr bref_standings_on_date bref_team_results
#' @importFrom dplyr select bind_rows mutate bind_rows rename arrange desc filter pull left_join
#' @importFrom purrr map
#' @export viz_standings

viz_standings <- function(lg_div, year) {

  ### Check if arguments are valid ----
  valid_lg_div <- c("AL East", "AL Central", "AL West", "AL Overall",
                   "NL East", "NL Central", "NL West", "NL Overall")

  if (!(lg_div %in% valid_lg_div)) {
    stop("The 'lg_div' argument must be any of these possibilities:
         AL East, AL Central, AL West, AL Overall, NL East, NL Central, NL West or NL Overall")
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  if (!(is.numeric(year) && year >= 1876 && year <= current_year)) {
    stop(paste0("The 'year' must be a numeric value between 1876 and the last/current MLB season"))
  }

  # If both arguments are valid, continue with the function

  ### Identify 'lg_div' input and get names of teams to visualize ----

  if (intersect(grepl("AL|NL", lg_div),
                grepl("East|Central|West|Overall", lg_div))) {
    # Division or leagues in that year
    message(paste0("Retreiving teams that played in ", lg_div, " in ", year, "..."))
    teams <- baseballr::bref_standings_on_date(paste0(year,"-04-30"), lg_div) |>
      as.data.frame() |>
      dplyr::select(1) |>
      unlist()

  }

  ### Get the game's results of each league to be visualized ----

  message("Getting games' data ...")

  # Gets the team's results tables for each team
  standings <- pbapply::pblapply(teams, baseballr::bref_team_results, year)
  # Binds tables for all teams into one data frame
  standings <- do.call("rbind", standings)
  # Remove the parentheses and the number inside them that could be in the Date column
  standings$Date <- gsub("\\s*\\(\\d+\\)", "", standings$Date)

  ####### SAVED -----^^^^^^^^^^

  standings <- standings |>
    dplyr::select(Game = Gm, Date, Team = Tm, R, RA, Record, Rank, GB, Year) |>
    tidyr::unite(Date, c("Date", "Year"), sep = " ", remove = TRUE) |>
    tidyr::separate(Record, c("W", "L"), sep = "-", remove = FALSE)

  # change some variable types
  standings$Date <- as.Date(standings$Date, format = "%A, %b %d %Y")
  standings$W <- as.numeric(standings$W)
  standings$L <- as.numeric(standings$L)

  # Replace "Tied" or "Up ..." with 0 in GB
  standings$GB <- gsub("^Tied$", "0", standings$GB)
  standings$GB <- gsub("^up (.+)$", "0", standings$GB)
  standings$GB <- as.numeric(standings$GB)


  # Add the Wpct and pythWLpct columns to standings, as well as its difference

  standings <- standings |>
    dplyr::mutate(Wpct = W/(W+L),
                  pythWpct = (R^1.81/(R^1.81+RA^1.81)),
                  Delta_Wpct_Pyth = Wpct - pythWpct)

  # Determine which teams are leading each Division when getting Overall data per league
  if (lg_div == "AL Overall" | lg_div == "NL Overall") {

    if (lg_div == "AL Overall") {

      divisions <- c("AL East", "AL Central", "AL West")
      message(paste("Getting AL standings by", max(standings$Date), "..."))

      leaders <- divisions |>
        purrr::map(~ baseballr::bref_standings_on_date(max(standings$Date), .x)) |>
        dplyr::bind_rows() |>
        dplyr::mutate(GB = ifelse(GB == "--", 0, GB)) |>
        dplyr::mutate(GB = as.numeric(GB)) |>
        dplyr::rename(Wpct = 'W-L%', pythwpct = 'pythW-L%') |>
        dplyr::arrange(dplyr::desc(Wpct))

      message(paste("Lines of AL Division Leaders by", max(standings$Date), "will be thicker in the plot"))
      print(leaders)


    } else if (lg_div == "NL Overall") {

      divisions <- c("NL East", "NL Central", "NL West")
      message(paste("Getting NL standings by", max(standings$Date), "..."))

      leaders <- divisions |>
        purrr::map(~ baseballr::bref_standings_on_date(max(standings$Date), .x)) |>
        dplyr::bind_rows() |>
        dplyr::mutate(GB = ifelse(GB == "--", 0, GB)) |>
        dplyr::mutate(GB = as.numeric(GB)) |>
        dplyr::rename(Wpct = 'W-L%', pythwpct = 'pythW-L%') |>
        dplyr::arrange(dplyr::desc(Wpct))

      message(paste("Lines of NL Division Leaders by", max(standings$Date), "will be thicker in the plot"))
      print(leaders)

    }
  } else {
    message(paste("Getting ", lg_div, "standings by", max(standings$Date), "..."))

    leaders <- baseballr::bref_standings_on_date(max(standings$Date), lg_div) |>
      dplyr::mutate(GB = ifelse(GB == "--", 0, GB)) |>
      dplyr::mutate(GB = as.numeric(GB)) |>
      dplyr::rename(Wpct = 'W-L%', pythwpct = 'pythW-L%') |>
      dplyr::arrange(dplyr::desc(Wpct))

    message(paste("Line of ", leaders$Tm[1], "will be thicker in the plot"))
    print(leaders)

  }

leaders <- leaders |>
  dplyr::filter(GB == 0) |>
  dplyr::pull(Tm)


# Create a table of colors by team

Teams_Lahman <- read.csv("data/Teams.csv")[ , -1]

team_palette <- Teams_Lahman  |>
  dplyr::filter(yearID == 2021)  |>
  dplyr::select(name, teamIDBR)

team_palette$name[team_palette$teamIDBR == "LAA"] <- "Los Angeles Angels"

teamcolors <- read.csv("data/teamcolors.csv")[ , -1]

team_palette <- team_palette |>
  dplyr::left_join(teamcolors, by = "name") |>
  dplyr::rename(Team = teamIDBR) |>
  dplyr::select(Team, primary, secondary)


# Replace "primary" color of some teams by its "secondary" color
team_palette[c(1, 2, 5, 11, 16, 17, 21, 25, 27, 28), 2] <- team_palette[c(1, 2, 5, 11, 16, 17, 21, 25, 27, 28), 3]
# 1 ARI, 2 ATL, 5 CHC, 11 HOU, 16 MIL, 17 MIN, 21 PHI, 25 SEA, 27 TBR, 28 TEX
team_palette[ , 3] <- NULL # Remove "secondary" column


# Join "primary" color column to standings data.frame
standings <- standings |>
  dplyr::left_join(team_palette, by = "Team")

unique_teams <- unique(standings$Team)

  # Plot GB timeline ----

  gb <- highcharter::highchart()

  # Add series of GB for each team in the data frame
  for (i in 1:length(unique_teams)) {

    if (lg_div == "AL Overall" | lg_div == "NL Overall") {

      if (unique_teams[i] %in% leaders) {

        gb <- gb |>  # Creates series for teams leading their divisions (When requesting Overall)
          highcharter::hc_add_series(data = standings[standings$Team == unique_teams[i], ],
                                     highcharter::hcaes(x = Date, y = GB),
                                     name = unique_teams[i],
                                     color = team_palette[team_palette$Team == unique_teams[i], 2],
                                     type = "spline",
                                     lineWidth = 4)

      } else {

        gb <- gb |>  # Creates series for remaining teams (When requesting Overall)
          highcharter::hc_add_series(data = standings[standings$Team == unique_teams[i], ],
                                     highcharter::hcaes(x = Date, y = GB),
                                     name = unique_teams[i],
                                     color = team_palette[team_palette$Team == unique_teams[i], 2],
                                     type = "spline",
                                     dashStyle = "ShortDashDotDot",
                                     lineWidth = 2)
      }

    } else {

      gb <- gb |> # Creates plot for requested division
        highcharter::hc_add_series(data = standings[standings$Team == unique_teams[i], ],
                                   highcharter::hcaes(x = Date, y = GB),
                                   name = unique_teams[i],
                                   color = team_palette[team_palette$Team == unique_teams[i], 2],
                                   type = "spline",
                                   lineWidth = 3)
    }

  }

  # Customize the plot
  gb <- gb |>
    highcharter::hc_xAxis(title = list(text = "Date"),
                          type = "datetime") |> # X axis definition
    highcharter::hc_yAxis(title = list(text = "GB"),
                          reversed = TRUE) |> # Y axis definition
    highcharter::hc_title(text = paste("<span style=\"color:#002d73\"> MLB - </span>",
                                       lubridate::year(standings$Date[1]),
                                       lg_div,
                                       "Standings (Games Behind)")) |>
    highcharter::hc_subtitle(text =  "Solid line(s) represent the leader(s) in the division(s)") |>
    highcharter::hc_credits(enabled = TRUE, # add credits
                            text = "Source: Baseball Reference. Using 'baseballr' R package") |>
    highcharter::hc_add_theme(highcharter::hc_theme_smpl()) |>
    highcharter::hc_tooltip(valueDecimals = 1,
                            borderWidth = 1,
                            table = TRUE,
                            valueSuffix = ' GB',
                            sort = FALSE, # Future feature to show all the teams in the tooltip
                            shared = TRUE, # Future feature to show all the teams in the tooltip
                            borderColor = "#000000") |> # color of tooltip border
    highcharter::hc_exporting(enabled = TRUE) # enable exporting option


  #### Plot W% timeline

  wl <- highcharter::highchart()

  # Add series of GB for each team in the data frame
  for (i in 1:length(unique_teams)) {

    if (lg_div == "AL Overall" | lg_div == "NL Overall") {

      if (unique_teams[i] %in% leaders) {

        wl <- wl |>  # Creates series for teams leading their divisions (When requesting Overall)
          highcharter::hc_add_series(data = standings[standings$Team == unique_teams[i], ],
                                     highcharter::hcaes(x = Date, y = Wpct),
                                     name = unique_teams[i],
                                     color = team_palette[team_palette$Team == unique_teams[i], 2],
                                     type = "spline",
                                     lineWidth = 4)

      } else {

        wl <- wl |>  # Creates series for remaining teams (When requesting Overall)
          highcharter::hc_add_series(data = standings[standings$Team == unique_teams[i], ],
                                     highcharter::hcaes(x = Date, y = Wpct),
                                     name = unique_teams[i],
                                     color = team_palette[team_palette$Team == unique_teams[i], 2],
                                     type = "spline",
                                     dashStyle = "ShortDashDotDot",
                                     lineWidth = 2)
      }

    } else {

      wl <- wl |> # Creates plot for requested division
        highcharter::hc_add_series(data = standings[standings$Team == unique_teams[i], ],
                                   highcharter::hcaes(x = Date, y = Wpct),
                                   name = unique_teams[i],
                                   color = team_palette[team_palette$Team == unique_teams[i], 2],
                                   type = "spline",
                                   lineWidth = 3)
    }

  }

  # Customize the W% plot
  wl <- wl |>
    highcharter::hc_xAxis(title = list(text = "Date"),
                          type = "datetime") |> # X axis definition
    highcharter::hc_yAxis(title = list(text = "W-L%"),
                          max = 1,
                          valueDecimals = 3) |> # Y axis definition
    highcharter::hc_title(text = paste("<span style=\"color:#002d73\"> MLB - </span>",
                                       lubridate::year(standings$Date[1]),
                                       lg_div,
                                       " - Winning percentage, by ", max(standings$Date))) |>
    highcharter::hc_subtitle(text =  "Solid line(s) represent the leader(s) in the division(s)") |>
    highcharter::hc_credits(enabled = TRUE,
                            text = paste0("Source: Baseball Reference. Using 'baseballr' R package. Retreived on: ",
                                          lubridate::with_tz(Sys.time(), "US/Eastern")  |>
                                            format("%Y-%m-%d %H:%M %Z")))  |>
    highcharter::hc_add_theme(highcharter::hc_theme_smpl()) |>
    highcharter::hc_tooltip(valueDecimals = 3,
                            borderWidth = 1,
                            table = TRUE, # adds color to team names in the tooltip
                            sort = TRUE, # Future feature to show all the teams in the tooltip
                            shared = TRUE, # Future feature to show all the teams in the tooltip
                            borderColor = "#000000") |> # tooltip border color
    highcharter::hc_exporting(enabled = TRUE) # enable exporting option

  # print viz of Games Behind
  print(gb)

  # print viz of Winning percentage
  print(wl)
}
