#' Scrapes MLB Standings in one specific season and visualizes a timeline of the winning percentage for any division or a league
#'
#' This function allows you to scrape the standings from MLB teams for a specific season, and visualize one chart of Winning percentage of teams during that season.
#' @param lg_div a string input, Baseball Reference Team abbreviation or a division (e.g. NL East, NL Central, NL West, NL Overall, AL East, AL Central, AL West, AL Overall or MLB)
#' @param year a numeric value, MLB season to be analyzed
#' @keywords MLB, standings
#' @importFrom highcharter highchart hchart hc_title hc_subtitle hc_credits hc_yAxis hc_xAxis hc_add_theme hcaes hc_theme_smpl hc_add_series hc_tooltip hc_exporting
#' @importFrom pbapply pblapply
#' @importFrom tidyr separate unite
#' @importFrom lubridate year
#' @importFrom baseballr bref_standings_on_date bref_team_results
#' @importFrom dplyr select bind_rows mutate rename arrange desc filter pull left_join
#' @importFrom purrr map
#' @export viz_wlp_season

viz_wlp_season <- function(lg_div, year) {

  ### Check if arguments are valid ----
  valid_lg_div <- c("AL East", "AL Central", "AL West", "AL Overall",
                    "NL East", "NL Central", "NL West", "NL Overall", "MLB")

  if (!(lg_div %in% valid_lg_div)) {
    stop("The 'lg_div' argument must be any of the following se possibilities:
         AL East, AL Central, AL West, AL Overall, NL East, NL Central, NL West, NL Overall or MLB")
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

  } else if (lg_div == "MLB") {
    message(paste0("Retreiving teams that played in ", lg_div, " in ", year, "..."))
    teams_al <- baseballr::bref_standings_on_date(paste0(year,"-04-30"), "AL Overall")
    teams_nl <- baseballr::bref_standings_on_date(paste0(year,"-04-30"), "NL Overall")
    teams <- rbind(teams_al, teams_nl) |>
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

  # Add the W% and pythWLpct columns to standings, as well as its difference

  standings <- standings |>
    dplyr::mutate(Wpct = W/(W+L),
                  pythWpct = (R^1.81/(R^1.81+RA^1.81)),
                  Delta_Wpct_Pyth = Wpct - pythWpct)


  # Function to process league divisions and return leaders
  get_league_leaders <- function(lg_div) {

    # Define the division based on the lg_div input
    divisions <- if (lg_div %in% c("AL Overall", "NL Overall")) {

      paste0(substr(lg_div, 1, 2), c(" East", " Central", " West"))

    } else if (lg_div %in% c("AL East", "AL Central", "AL West",
                             "NL East", "NL Central", "NL West")) {

      lg_div

    } else if (lg_div == "MLB") {

      c("AL East", "AL Central", "AL West", "NL East", "NL Central", "NL West")

    }

    message(paste("Getting", lg_div, "standings by", max(standings$Date), "..."))

    leaders <- divisions |>
      purrr::map(~ baseballr::bref_standings_on_date(max(standings$Date), .x)) |>
      dplyr::bind_rows() |>
      dplyr::mutate(GB = ifelse(GB == "--", 0, GB),
                    GB = as.numeric(GB)) |>
      dplyr::rename(Wpct = 'W-L%', pythwpct = 'pythW-L%') |>
      dplyr::arrange(dplyr::desc(Wpct))

    message(paste("Lines of", lg_div, "Division leader(s) by", max(standings$Date), "will be thicker in the plot"))

    return(leaders)
  }

  # Then call your function as before:
  if (lg_div %in% c("AL Overall", "NL Overall", "AL East", "AL Central", "AL West", "NL East", "NL Central", "NL West")) {

    leaders <- get_league_leaders(lg_div)

  } else if (lg_div == "MLB") {

    leaders_AL <- get_league_leaders("AL Overall")
    leaders_NL <- get_league_leaders("NL Overall")

    # Combine AL and NL leaders into one data frame
    leaders <- dplyr::bind_rows(leaders_AL, leaders_NL)
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

  #### Plot W% timeline

  wl <- highcharter::highchart()

  # Add series of GB for each team in the data frame
  for (i in 1:length(unique_teams)) {

    if (lg_div == "AL Overall" | lg_div == "NL Overall" | lg_div == "MLB") {

      if (unique_teams[i] %in% leaders) {

        # Creates series for teams leading their divisions (When requesting Overall)
        wl <- wl |>
          highcharter::hc_add_series(data = standings[standings$Team == unique_teams[i], ],
                                     highcharter::hcaes(x = Date, y = Wpct),
                                     name = unique_teams[i],
                                     color = team_palette[team_palette$Team == unique_teams[i], 2],
                                     type = "spline",
                                     lineWidth = 4)

      } else {

        # Creates series for remaining teams (When requesting Overall)
        wl <- wl |>
          highcharter::hc_add_series(data = standings[standings$Team == unique_teams[i], ],
                                     highcharter::hcaes(x = Date, y = Wpct),
                                     name = unique_teams[i],
                                     color = team_palette[team_palette$Team == unique_teams[i], 2],
                                     type = "spline",
                                     dashStyle = "ShortDashDotDot",
                                     lineWidth = 2)
      }

    } else {
      # Creates plot for requested division
      wl <- wl |>
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
    highcharter::hc_yAxis(title = list(text = "W%"),
                          max = 1,
                          valueDecimals = 3) |> # Y axis definition
    highcharter::hc_title(text =
                            if (lg_div == "MLB")  {
                              paste0("<span style=\"color:#002d73\"> MLB Teams - </span>",
                                     lubridate::year(standings$Date[1]),
                                     " - W% after games on ", max(standings$Date))
                            } else {
                              paste0("<span style=\"color:#002d73\"> MLB - </span>",
                                     lubridate::year(standings$Date[1]),
                                     lg_div,
                                     " - W% after games on ", max(standings$Date))
                            }
    ) |>
    highcharter::hc_subtitle(text =  "Solid line(s) represent the Division(s) leader(s)") |>
    highcharter::hc_credits(enabled = TRUE,
                            text = paste0("Source: Baseball Reference. Using 'baseballr' R package. Retrieved on: ",
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


  # print viz of Winning percentage in a season
  print(wl)
}
