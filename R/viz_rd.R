#' @title Graph of Accumulated Runs Differential
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
#' @importFrom dplyr  %>% select group_by mutate ungroup summarise arrange case_when
#' @importFrom tidyr separate unite
#' @importFrom purrr map
#' @importFrom highcharter hchart hc_tooltip hc_add_theme hc_theme_smpl hc_xAxis hc_yAxis hc_title hc_subtitle hc_credits hc_exporting hw_grid
#' @importFrom htmltools browsable
#' @importFrom lubridate with_tz
#'
#' @return A areaspline-type chart with the accumulated run differential for the Team(s) along the season analyzed
#'
#'#' @examples
#' viz_rd("BOS", 2023)
#' viz-rd("AL West", 2021)
#' viz_rd("MLB", 2008)

viz_rd <- function(team, year) {

  ### Check if arguments are valid ----
  valid_teams <- c("AL East", "AL Central", "AL West", "AL Overall", "NL East", "NL Central", "NL West", "NL Overall", "MLB")
  if (!(is.character(team) && ((nchar(team) == 3) | (team %in% valid_teams)))) {
    stop("The 'team' must be the Baseball Reference MLB team abbreviation or any of these ones: AL East, AL Central, AL West, AL Overall, NL East, NL Central, NL West, NL Overall or MLB")
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
    print(paste0("Retreiving teams that played in ", team, " in ", year, "..."))
    teams <- baseballr::bref_standings_on_date(paste0(year,"-04-30"), team) %>%
      as.data.frame() %>%
      select(1) %>%
      unlist()

  } else if (team == "MLB") {

    # All MLB teams in year
    mlb <- c("AL Overall", "NL Overall")

    print(paste0("Retreiving teams that played in ", team, " in ", year, "..."))
    teams <- pbapply::pbsapply(mlb, baseballr::bref_standings_on_date, date = paste0(year,"-04-30"))
    teams <- c(teams[[1,1]], teams[[1,2]])

  } else {
    # Only one team
    teams <- team
  }

  ### Get the game's results of each team to be visualized ----

  print("Getting games' data ...")

  # Gets the team's results tables for each team
  rd <- pblapply(teams, baseballr::bref_team_results, year)
  # Binds tables for all teams into one data frame
  rd <- do.call("rbind", rd)

  # change Game variable to numeric
  rd$Gm <- as.numeric(rd$Gm)
  # change Runs Allowed variable to numeric
  rd$RA <- as.numeric(rd$RA)

  ### Tidying the `rd` data frame ----

  rd <- rd %>%
    tidyr::separate(Date, c("wd", "Date"), sep = ", ") %>%    # wd = weekday
    tidyr::unite(Date, c("Date", "Year"), sep = ", ") %>%
    dplyr::select(Gm, Date, Tm, Opp, R, RA, Record, Rank, GB) %>%
    dplyr::group_by(Tm) %>%
    dplyr::mutate(R_Diff = R - RA,
                  cum_R_Diff = cumsum(R_Diff)) %>%
    dplyr::ungroup()

  names(rd)[c(1,3)] <- c("Game", "Team")

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
  min_RDiff <- round(min(rd$cum_R_Diff)*1.1)
  max_RDiff <- round(max(rd$cum_R_Diff)*1.1)

  ### Defining a data frame with Teams, Division and colors ----

                # teams_info <- teams_lu_table %>%
                #   dplyr::filter(sport.name == "Major League Baseball") %>%
                #   dplyr::mutate(Division = case_when(
                #     division.name == "American League West" ~ "AL West",
                #     division.name == "American League Central" ~ "AL Central",
                #     division.name == "American League East" ~ "AL East",
                #     division.name == "National League West" ~ "NL West",
                #     division.name == "National League Central" ~ "NL Central",
                #     division.name == "National League East" ~ "AL East")) %>%
                #   dplyr::select(bref_abbreviation, Division, name) %>%
                #   merge(teamcolors %>%
                #           dplyr::filter(league == "mlb") %>%
                #           dplyr::select(name, primary, secondary, logo),
                #         by = "name")

  ### Creating an ordered vector (not factor) of teams based on accumulated Runs Differential. ----
  ### NOTE: This is because 'highcharter' hc_grid function plots charts in the order they are created and not based on factors (as ggplot)

  print(paste0("By ", as.character(max(as.Date(rd$Date, format = "%b %d, %Y"))), ", the team(s) are ranked in Runs Differential as shown here below (defining their order in the chart)"))

  teams_factor <- rd %>%
    dplyr::group_by(Team) %>%
    dplyr::summarise(R = sum(R), RA = sum(RA)) %>%
    dplyr::mutate(R_Diff = R - RA) %>%
    dplyr::arrange(desc(R_Diff), desc(R)) %>%
    dplyr::mutate(rd_ranking = min_rank(desc(R_Diff)),
                  ranking_text = case_when(
                    rd_ranking == 1 ~ "1st",
                    rd_ranking == 2 ~ "2nd",
                    rd_ranking == 3 ~ "3rd",
                    TRUE ~ paste0(rd_ranking, "th")
                  )
    )

  teams_factor %>%
    select(Rank = rd_ranking, Team, R, RA, RD = R_Diff) %>%
      print(n = nrow(teams_factor))

  rd <- rd %>%
    inner_join(teams_factor %>%
                 select(Team, ranking_text),
               by = "Team")

  teams_factor <- teams_factor %>%
    dplyr::select(1) %>%
    unlist()

  ### Calculating how many games each team has played (maximum number of games per team) ----
  max_games <- rd %>%
    group_by(Team) %>%
    summarise(max_games = max(Game))

    ###Creating charts for each team ----

  map(teams_factor, function(x) {

    rd[rd$Team == x,] %>%
      highcharter::hchart(showInLegend = FALSE,
                          type = "areaspline",
                          highcharter::hcaes(x = Game,
                                             y = cum_R_Diff),
                          marker = list(enabled = FALSE),
                          color = "#4B5463",
                          fillColor = "#D4DFD0",
                          negativeFillColor = "#FF988C",
                          fillOpacity = 0.4) %>%
      highcharter::hc_tooltip(useHTML = TRUE,
                              headerFormat = "",
                              pointFormat = "<b>Team:</b> {point.Team} <br>
                                            <b>Date:</b> {point.Date} <br>
                                            <b>Game:</b> {point.Game} <br>
                                            <b>Run Diff:</b> {point.cum_R_Diff} <br>
                                            <b>Div Rank</b>: {point.Rank} <br>
                                            <b>W-L:</b> {point.Record} <br>
                                            <b>GB:</b> {point.GB}",
                              borderWidth = 1,
                              borderColor = "#000000") %>%
      highcharter::hc_add_theme(hc_theme_smpl()) %>%
      # X axis definition
      highcharter::hc_xAxis(title = list(text = "Games"),
                            tickInterval = "1") %>%
      # Y axis definition
      highcharter::hc_yAxis(title = list(text = "R Diff"),
                            min = min_RDiff,
                            max = max_RDiff) %>%
      highcharter::hc_title(text = paste0(x, "<span style=\"background-color:#002d73\"> - Runs Differential </span>")) %>%
      highcharter::hc_subtitle(text =
                                 if (length(teams_factor) > 1) {
                                   paste0(year, " Season. After ", max_games$max_games[max_games$Team == x], " games played.", "<br>",
                                          "Ranked as ", unique(rd$ranking_text[rd$Team == x]), " in RD in ", team)
                                 } else {
                                   paste0(year, " Season. After ", max_games$max_games[max_games$Team == x], " games played")
                                 }) %>%
      # adding credits and date when the chart was build
      highcharter::hc_credits(enabled = TRUE,
                              text = paste0("Source: Baseball Reference. Using 'baseballr' R package. Retreived on: ",
                                            with_tz(Sys.time(), "US/Eastern") %>%
                                              format("%Y-%m-%d %H:%M %Z"))) %>%
      # enable exporting option
      highcharter::hc_exporting(enabled = TRUE)
    }
    ) %>%

    # faceting all charts
    highcharter::hw_grid(rowheight = 400,
                           ncol = viz_col)  %>%
    browsable()
}
