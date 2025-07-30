#' @title Retrieve MLB Standings and Team Results
#' @description
#' This function retrieves the game results and standings for teams in a given MLB division/league and season.
#'
#' @param lg_div A string specifying the division or league. Valid options are:
#'   "AL East", "AL Central", "AL West", "AL Overall",
#'   "NL East", "NL Central", "NL West", "NL Overall", or "MLB".
#' @param year A numeric value indicating the MLB season year.
#'
#' @return A data frame containing team standings and game results with columns:
#'   Game, Date, Team, R, RA, Record, Rank, GB, W, L, Wpct, R_cum, RA_cum, pythWpct, Delta_Wpct_Pyth.
#'
#' @export
#'
#' @importFrom baseballr bref_standings_on_date bref_team_results
#' @importFrom dplyr select mutate arrange group_by ungroup
#' @importFrom tidyr unite separate
#' @importFrom pbapply pblapply
#'
#' @examples
#' \dontrun{
#' get_standings_season("AL East", 2024)
#' get_standings_season("MLB", 2023)
#' }

get_standings_season <- function(lg_div, year, parallel = TRUE) {

  ### Validate inputs ----
  valid_lg_div <- c(
    "AL East", "AL Central", "AL West", "AL Overall",
    "NL East", "NL Central", "NL West", "NL Overall",
    "MLB"
  )
  if (!(lg_div %in% valid_lg_div)) {
    stop("⚠️ The 'lg_div' argument must be one of these: ", paste(valid_lg_div, collapse = ", "))
  }

  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (!(is.numeric(year) && year >= 1876 && year <= current_year)) {
    stop(paste0("⚠️ The 'year' must be a numeric value between 1876 and ", current_year))
  }

  ### Identify teams ----
  if (grepl("AL|NL", lg_div) && grepl("East|Central|West|Overall", lg_div)) {
    message(paste0("⏳ Retrieving teams that played in ", lg_div, " in ", year, "..."))
    teams <- baseballr::bref_standings_on_date(paste0(year, "-04-30"), lg_div) |>
      as.data.frame() |>
      dplyr::select(1) |>
      unlist()
  } else if (lg_div == "MLB") {
    message(paste0("⏳ Retrieving teams that played in MLB in ", year, "..."))
    teams_al <- baseballr::bref_standings_on_date(paste0(year, "-04-30"), "AL Overall")
    teams_nl <- baseballr::bref_standings_on_date(paste0(year, "-04-30"), "NL Overall")
    teams <- rbind(teams_al, teams_nl) |>
      as.data.frame() |>
      dplyr::select(1) |>
      unlist()
  }

  ### Fetch game results ----
  message("⏳ Fetching games' data ...")

  standings <- NULL
  if (parallel) {
    # Use progressr for progress bars
    progressr::with_progress({
      p <- progressr::progressor(along = teams)
      future::plan(future::multisession)
      standings <- future.apply::future_lapply(teams, function(t) {
        p(sprintf("Loading %s", t))
        baseballr::bref_team_results(t, year)
      })
    })
  } else {
    standings <- pbapply::pblapply(teams, baseballr::bref_team_results, year)
  }

  standings <- do.call("rbind", standings)
  standings$Date <- gsub("\\s*\\(\\d+\\)", "", standings$Date)

  ### Transform data ----
  standings <- standings |>
    dplyr::select(Game = Gm, Date, Team = Tm, R, RA, Record, Rank, GB, Year) |>
    tidyr::unite(Date, c("Date", "Year"), sep = " ", remove = TRUE) |>
    tidyr::separate(Record, c("W", "L"), sep = "-", remove = FALSE)

  standings$Date <- as.Date(standings$Date, format = "%A, %b %d %Y")
  standings$W <- as.numeric(standings$W)
  standings$L <- as.numeric(standings$L)

  standings <- standings |>
    dplyr::mutate(Wpct = W / (W + L)) |>
    dplyr::arrange(Date) |>
    dplyr::group_by(Team) |>
    dplyr::mutate(
      R_cum = cumsum(R),
      RA_cum = cumsum(RA),
      pythWpct = (R_cum^1.81 / (R_cum^1.81 + RA_cum^1.81)),
      Delta_Wpct_Pyth = Wpct - pythWpct
    ) |>
    dplyr::ungroup()

  return(standings)
}
