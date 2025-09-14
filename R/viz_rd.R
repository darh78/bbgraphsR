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
#'
#' @return A areaspline-type chart with the accumulated run differential for the Team(s) along the season analyzed
#'
#' @examples
#' \dontrun{
#' viz_rd("BOS", 2023)
#' ## returns an RD chart for Boston Red Sox in the 2023 Season
#' viz_rd("NL Central", 2008)
#' ## returns an RD chart for all the NL Central Teams in 2008, in descending order
#'  }

#### Function viz_rd ----

viz_rd <- function(team, year) {
  on.exit(try(closeAllConnections(), silent = TRUE), add = TRUE)

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

  # Helper to fetch earliest standings from season start with a progress bar.
  # Phase 1: Search daily from Mar 25 to Apr 05 (earliest typical Opening Day window).
  # Phase 2: If none found, widen search to Apr 06–May 15 and use first available.
  fetch_division_teams <- function(div, yr) {
    search_and_pick <- function(start_str, end_str) {
      dates <- seq(as.Date(sprintf("%s-%s", yr, start_str)),
                   as.Date(sprintf("%s-%s", yr, end_str)), by = "1 day")
      message(sprintf("  Probing standings dates from %s to %s...", format(dates[1], "%Y-%m-%d"), format(dates[length(dates)], "%Y-%m-%d")))
      pb <- utils::txtProgressBar(min = 0, max = length(dates), style = 3)
      on.exit(try(close(pb), silent = TRUE), add = TRUE)
      last_err <- NULL
      for (i in seq_along(dates)) {
        dt <- dates[i]
        res <- suppressWarnings(tryCatch({
          baseballr::bref_standings_on_date(date = format(dt, "%Y-%m-%d"), division = div)
        }, error = function(e) e))
        utils::setTxtProgressBar(pb, i)
        if (!inherits(res, "error") && !is.null(res)) {
          df <- as.data.frame(res)
          if (nrow(df) > 0) return(list(df = df, last_err = last_err))
        } else if (inherits(res, "error")) {
          last_err <- res$message
        }
      }
      return(list(df = NULL, last_err = last_err))
    }

    # Phase 1: Mar 25–Apr 05
    r1 <- search_and_pick("03-25", "04-05")
    if (!is.null(r1$df)) return(r1$df)

    # Phase 2: Apr 06–May 15 (fallback)
    r2 <- search_and_pick("04-06", "05-15")
    if (!is.null(r2$df)) return(r2$df)

    last_err <- r2$last_err
    stop(sprintf(" Failed to fetch standings for %s in %s (searched Mar 25–May 15). Last error: %s", div, yr, ifelse(is.null(last_err), "unknown", last_err)))
  }

  if (grepl("AL|NL", team) & grepl("East|Central|West|Overall", team)) {
    # Division or leagues in that year
    message(paste0("Retreiving teams that played in ", team, " in ", year, "..."))
    # Build a cache filename
    cache_file <- file.path(tempdir(), paste0("standings_", gsub(" ", "_", team), "_", year, ".rds"))

    if (file.exists(cache_file)) {
      message("  Loading standings from cache...")
      teams_df <- readRDS(cache_file)
    } else {
      message(" Downloading standings from Baseball Reference...")
      Sys.sleep(runif(1, 1.5, 3.5))  # Delay to avoid rate limit
      teams_df <- tryCatch({
        fetch_division_teams(team, year)
      }, error = function(e) {
        message("  Standings not available up to May 15: ", conditionMessage(e))
        NULL
      })
      if (!is.null(teams_df)) saveRDS(teams_df, cache_file)
    }

    # Fallback mapping for early-season if standings are unavailable
    fallback_division_teams <- function(div) {
      al_east <- c("BAL","BOS","NYY","TBR","TOR")
      al_central <- c("CHW","CLE","DET","KCR","MIN")
      al_west <- c("HOU","LAA","OAK","SEA","TEX")
      nl_east <- c("ATL","MIA","NYM","PHI","WSN")
      nl_central <- c("CHC","CIN","MIL","PIT","STL")
      nl_west <- c("ARI","COL","LAD","SDP","SFG")
      switch(div,
             "AL East" = al_east,
             "AL Central" = al_central,
             "AL West" = al_west,
             "NL East" = nl_east,
             "NL Central" = nl_central,
             "NL West" = nl_west,
             "AL Overall" = c(al_east, al_central, al_west),
             "NL Overall" = c(nl_east, nl_central, nl_west),
             character(0))
    }

    if (!is.null(teams_df)) {
      # Prefer 'Tm' column if available, otherwise take first col
      teams_df <- as.data.frame(teams_df)
      first_col <- if ("Tm" %in% names(teams_df)) "Tm" else names(teams_df)[1]
      teams <- unlist(teams_df[[first_col]])
    } else {
      message("  Using fallback team list for ", team, ".")
      teams <- fallback_division_teams(team)
    }
    if (length(teams) == 0 || all(is.na(teams))) {
      stop(sprintf(" No teams found for %s in %s using standings up to May 15.", team, year))
    }

  } else if (team == "MLB") {

    # All MLB teams in year
    mlb <- c("AL Overall", "NL Overall")
    teams_list <- list()

    message(paste0("Retreiving teams that played in ", team, " in ", year, "..."))

    for (lg in mlb) {
      cache_file <- file.path(tempdir(), paste0("standings_", gsub(" ", "_", lg), "_", year, ".rds"))

      ## Loading the teams, either from the cache or from the page
      if (file.exists(cache_file)) {
        message(paste0("  Loading cached standings for ", lg, "..."))
        lg_standings <- readRDS(cache_file)
      } else {
        message(paste0(" Fetching standings for ", lg, " ..."))
        Sys.sleep(runif(1, 1.5, 3.5))  # random delay
        lg_standings <- tryCatch({
          fetch_division_teams(lg, year)
        }, error = function(e) {
          message("  Standings not available up to May 15 for ", lg, ": ", conditionMessage(e))
          NULL
        })
        if (!is.null(lg_standings)) saveRDS(lg_standings, cache_file)
      }

      if (!is.null(lg_standings)) {
        teams_list[[lg]] <- if ("Tm" %in% names(lg_standings)) lg_standings$Tm else lg_standings[[1]]
      } else {
        # Fallback to static mapping if standings unavailable
        teams_list[[lg]] <- if (lg == "AL Overall") c("BAL","BOS","NYY","TBR","TOR","CHW","CLE","DET","KCR","MIN","HOU","LAA","OAK","SEA","TEX")
                            else c("ATL","MIA","NYM","PHI","WSN","CHC","CIN","MIL","PIT","STL","ARI","COL","LAD","SDP","SFG")
      }
    }

    teams <- unlist(teams_list)

  } else {
    # Only one team
    teams <- team
  }

  ### Get the game's results of each team to be visualized ----

  # Setup in-memory caching for team results
  if (!requireNamespace("memoise", quietly = TRUE)) {
    stop("Package 'memoise' is required but not installed.")
  }

  # Create a memoised version of the function
  cached_bref_team_results <- memoise::memoise(baseballr::bref_team_results)

  # Define temporary cache file for full RD data
  cache_file_rd <- file.path(tempdir(), paste0("games_rd_", gsub(" ", "_", team), "_", year, ".rds"))

  if (file.exists(cache_file_rd)) {
    message("  Loading full run differential data from cache...")
    rd <- readRDS(cache_file_rd)
  } else {
    message(" Fetching all game results from Baseball Reference...")

    # Fetch each team's results with a progress bar
    get_team_results_safe <- function(t, yr) {
      tryCatch({
        cached_bref_team_results(t, yr)
      }, error = function(e) {
        message(sprintf("  Skipping %s: %s", t, conditionMessage(e)))
        NULL
      })
    }

    # Sequential with progress bar
    rd <- pbapply::pblapply(teams, function(t) get_team_results_safe(t, year))

    # Binds all individual teams' data into one dataframe
    rd <- Filter(Negate(is.null), rd)
    if (length(rd) == 0) {
      stop(" No game results could be retrieved for the selected teams.")
    }
    rd <- do.call("rbind", rd)

    # Convert relevant columns to numeric
    rd$Gm <- as.numeric(rd$Gm)
    rd$R <- as.numeric(rd$R)
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
  min_RD <- floor(min(rd$cum_RD, na.rm = TRUE)/10)*10
  max_RD <- ceiling(max(rd$cum_RD, na.rm = TRUE)/10)*10

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
                            tickInterval = 1)  |>
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
                              text = paste0("Source: Baseball Reference. Using 'baseballr' R package. Retreived on: ",
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
