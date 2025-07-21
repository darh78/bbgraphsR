#' @title Graph of Accumulated Runs Differential for teams
#' @description
#' Function to plot the accumulated runs differential for an MLB team (or a group of them) on a specific MLB season
#'
#' @param team a string input, Baseball Reference Team abbreviation, a division or the whole League
#' @param year a numeric value, MLB season to be analyzed
#' @param parallel Logical. Whether to use parallel processing (default is TRUE).
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
#' @importFrom future.apply future_lapply
#'
#' @return A areaspline-type chart with the accumulated run differential for the Team(s) along the season analyzed
#'
#' @examples
#' viz_rd("AL West", 2021)
#' ## returns an RD chart for all the AL West Teams in 2021, in descending order
#' \dontrun{
#' viz_rd("BOS", 2023)
#' ## returns an RD chart for Boston Red Sox in the 2023 Season
#' viz_rd("NL Central", 2008)
#' ## returns an RD chart for all the NL Central Teams in 2008, in descending order
#'  }

# Setup in-memory caching for team results
if (!requireNamespace("memoise", quietly = TRUE)) {
  stop("Package 'memoise' is required but not installed.")
}

# Create a memoised version of the function
cached_bref_team_results <- memoise::memoise(baseballr::bref_team_results)

#### Function viz_rd ----

viz_rd <- function(team, year, parallel = TRUE) {

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
    # Build a cache filename
    cache_file <- file.path(tempdir(), paste0("standings_", gsub(" ", "_", team), "_", year, ".rds"))

    if (file.exists(cache_file)) {
      message("🗂️  Loading standings from cache...")
      teams_df <- readRDS(cache_file)
    } else {
      message("🌐 Downloading standings from Baseball Reference...")
      Sys.sleep(runif(1, 1.5, 3.5))  # Delay to avoid rate limit
      teams_df <- tryCatch({
        baseballr::bref_standings_on_date(paste0(year,"-04-30"), team)
      }, error = function(e) {
        stop(sprintf("❌ Failed to fetch standings: %s", e$message))
      })
      saveRDS(teams_df, cache_file)
    }

    teams <- teams_df |>
      as.data.frame() |>
      dplyr::select(1) |>
      unlist()

  } else if (team == "MLB") {

    # All MLB teams in year
    mlb <- c("AL Overall", "NL Overall")
    teams_list <- list()

    message(paste0("Retreiving teams that played in ", team, " in ", year, "..."))

    for (lg in mlb) {
      cache_file <- file.path(tempdir(), paste0("standings_", gsub(" ", "_", lg), "_", year, ".rds"))

      ## Loading the teams, either from the cache or from the page
      if (file.exists(cache_file)) {
        message(paste0("🗂️  Loading cached standings for ", lg, "..."))
        lg_standings <- readRDS(cache_file)
      } else {
        message(paste0("🌐 Fetching standings for ", lg, " ..."))
        Sys.sleep(runif(1, 1.5, 3.5))  # random delay
        lg_standings <- tryCatch({
          baseballr::bref_standings_on_date(date = paste0(year, "-04-30"), division = lg)
        }, error = function(e) {
          stop(sprintf("❌ Failed to retrieve standings for %s — %s", lg, e$message))
        })
        saveRDS(lg_standings, cache_file)
      }

      teams_list[[lg]] <- lg_standings[[1]]
    }

    teams <- unlist(teams_list)

  } else {
    # Only one team
    teams <- team
  }

  ### Get the game's results of each team to be visualized ----

  # Define temporary cache file for full RD data
  cache_file_rd <- file.path(tempdir(), paste0("games_rd_", gsub(" ", "_", team), "_", year, ".rds"))

  if (file.exists(cache_file_rd)) {
    message("🗂️  Loading full run differential data from cache...")
    rd <- readRDS(cache_file_rd)
  } else {
    message("🌐 Fetching all game results from Baseball Reference...")

    # Setup parallel plan if requested
    if (parallel) {
      future::plan(future::multisession)
      rd <- future.apply::future_lapply(teams, function(t) {
        cached_bref_team_results(t, year)
      })
    } else {
      rd <- lapply(teams, function(t) {
        cached_bref_team_results(t, year)
      })
    }

    # Binds all individual teams' data into one dataframe
    rd <- do.call("rbind", rd)

    # Convert relevant columns to numeric
    rd$Gm <- as.numeric(rd$Gm)
    rd$RA <- as.numeric(rd$RA)

    # Save the processed full RD table to temp cache
    saveRDS(rd, cache_file_rd)
  }

  ### Tidying the `rd` data frame ----

  # Separate weekday and date
  rd <- rd |>
    tidyr::separate(Date, c("wd", "month_day"), sep = ", ")

  # Remove trailing text like " (1)"
  rd$month_day <- gsub("\\s*\\(.*\\)$", "", rd$month_day)

  # Clean and convert to proper Date object
  rd <- rd |>
    dplyr::mutate(
      month_day = trimws(month_day),
      Date = as.Date(paste(month_day, year), format = "%b %d %Y")
    ) |>
    dplyr::select(Gm, Date, Tm, Opp, R, RA, Record, Rank, GB) |>
    dplyr::group_by(Tm) |>
    dplyr::mutate(RD = R - RA,
                  cum_RD = cumsum(RD)) |>
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
  min_RD <- floor(min(rd$cum_RD)/10)*10
  max_RD <- ceiling(max(rd$cum_RD)/10)*10

  # interval in y axis to allow a better adjustment of y min and max
  if (max_RD <= 120) {
    y_interval <- 25
  } else if (max_RD <= 250) {
    y_interval <- 50
  } else if (max_RD <= 350) {
    y_interval <- 100
  } else {
    y_interval <- 200
  }


  ### Creating an ordered vector (not factor) of teams based on accumulated Runs Differential. ----
       ### NOTE: This is because 'highcharter' hc_grid function plots charts in the order they are created and not based on factors (as ggplot)

  teams_factor <- rd  |>
    dplyr::group_by(Team)  |>
    dplyr::summarise(R = sum(R), RA = sum(RA))  |>
    dplyr::mutate(RD = R - RA)  |>
    dplyr::arrange(dplyr::desc(RD), dplyr::desc(R))  |>
    dplyr::mutate(Rank = dplyr::min_rank(dplyr::desc(RD)),
                  Rank_RD = case_when(
                    Rank == 1 ~ "1st",
                    Rank == 2 ~ "2nd",
                    Rank == 3 ~ "3rd",
                    TRUE ~ paste0(Rank, "th")
                  )
    )

  ## Printing the teams factor in the console, with the ranking, R, RA and RD
  if (nrow(teams_factor) > 1) {
    # If there is more than one team being analyzed, show a message to indicate that the chart will be ordered by descending order of RD
    message(paste0("By ", as.character(max(as.Date(rd$Date, format = "%b %d, %Y"))), ", the team(s) are ranked in Runs Differential as shown here below (defining their order in the chart)"))
    teams_factor |>
      dplyr::select(Rank_RD, Team, R, RA, RD) |>
      print(n = nrow(teams_factor))
  } else {
    # If there is only one team, show a message to indicate their results
    message(paste0("By ", as.character(max(as.Date(rd$Date, format = "%b %d, %Y"))), ", ", team, " had scored and allowed runs as shown here below"))
    teams_factor |>
      dplyr::select(Team, R, RA, RD) |>
      print()
      }

  ## Joining the RD's ranking to the rd dataframe
  rd <- rd  |>
    dplyr::inner_join(teams_factor  |>
                        dplyr::select(Team, Rank_RD),
                      by = "Team")

  ## Getting the vector of ordered teams
  teams_factor <- teams_factor  |>
    dplyr::select(1)  |>
    unlist()

  ### Calculating how many games each team has played (maximum number of games per team) ----
  max_games <- rd  |>
    group_by(Team)  |>
    summarise(max_games = max(Game))

    ###Creating charts for each team ----

  map(teams_factor, function(x) {

    team_data <- rd[rd$Team == x,]        # store team data in a new variable
    max_diff <- max(team_data$cum_RD) # calculate max cum_RD for the team
    min_diff <- min(team_data$cum_RD) # calculate min cum_RD for the team

    team_data  |>
      # adding the area chart for the accumulated run differential
      highcharter::hchart(showInLegend = FALSE,
                          type = "areaspline",
                          highcharter::hcaes(x = Game,
                                             y = cum_RD),
                          marker = list(enabled = FALSE),
                          color = "#4B5463",
                          fillColor = "#D4DFD0",
                          negativeFillColor = "#FF988C",
                          fillOpacity = 0.4,
                          name = "RD") |>
      # adding points on the maximum of run differentials
      highcharter::hc_add_series(team_data[team_data$cum_RD == max_diff, ],
                                 type = "scatter",
                                 highcharter::hcaes(x = Game,
                                                    y = cum_RD),
                                 color = "blue",
                                 marker = list(symbol = "triangle",
                                               radius = 4,
                                               lineWidth = 1),
                                 showInLegend = TRUE,
                                 Opacity = 1,
                                 zIndex = 3,
                                 name = "Max")  |>
      # adding points on the minimum of run differentials
      highcharter::hc_add_series(team_data[team_data$cum_RD == min_diff, ],
                                 type = "scatter",
                                 highcharter::hcaes(x = Game,
                                                    y = cum_RD),
                                 color = "darkred",
                                 marker = list(symbol = "triangle-down",
                                               radius = 4,
                                               lineWidth = 1),
                                 showInLegend = TRUE,
                                 Opacity = 1,
                                 zIndex = 3,
                                 name = "Min")  |>
      highcharter::hc_tooltip(useHTML = TRUE,
                              headerFormat = "",
                              pointFormat = "<b>Team:</b> {point.Team} <br>
                                            <b>Date:</b> {point.Date} <br>
                                            <b>Game:</b> {point.Game} <br>
                                            <b>Run Diff:</b> {point.cum_RD} <br>
                                            <b>Div Rank</b>: {point.Rank} <br>
                                            <b>W-L:</b> {point.Record} <br>
                                            <b>GB:</b> {point.GB}",
                              borderWidth = 1,
                              borderColor = "#000000")  |>
      highcharter::hc_add_theme(hc_theme_smpl())  |>
      # X axis definition
      highcharter::hc_xAxis(title = list(text = "Games"),
                            tickInterval = "1")  |>
      # Y axis definition
      highcharter::hc_yAxis(title = list(text = "R Diff"),
                            min = min_RD,
                            max = max_RD,
                            tickInterval = y_interval)  |>
      highcharter::hc_title(text = paste0(x, "<span style=\"background-color:#002d73\"> - Run Differential </span>"))  |>
      highcharter::hc_subtitle(text =
                                 if (length(teams_factor) > 1) {
                                   paste0(year, " Season. After ", max_games$max_games[max_games$Team == x], " games played.", "<br>",
                                          "Ranked as ", unique(rd$Rank_RD[rd$Team == x]), " in RD in ", team, ", with ", team_data$cum_RD[length(team_data$cum_RD)])
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
