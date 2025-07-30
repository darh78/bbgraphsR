#' @title Retrieve MLB Expanded Standings for Multiple Seasons
#'
#' @description
#' Scrapes the "expanded standings" table from Baseball-Reference for each season
#' between `start_season` and `end_season`. The results are combined into a single
#' data frame. Automatically adjusts `end_season` if the current year's standings
#' are not yet available (before April).
#'
#' @param start_season Numeric. First MLB season to retrieve (e.g., 2020).
#' @param end_season Numeric. Last MLB season to retrieve (e.g., 2024).
#'
#' @return A data frame combining expanded standings from all requested seasons.
#' Includes a `Year` column indicating the season.
#'
#' @examples
#' \dontrun{
#' get_standings_years(2020, 2023)
#' }
#'
#' @export
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text html_table
#' @importFrom dplyr mutate filter bind_rows
#' @importFrom pbapply pblapply
#'
#' @examples
#' \dontrun{
#' get_standings_years(2021, 2024)
#' }

get_standings_years <- function(start_season, end_season) {
  # Validate input years
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (end_season > current_year || start_season > end_season) {
    stop("Invalid range of seasons.")
  }

  # If current year is selected but season likely hasn't started yet
  if (end_season == current_year && as.numeric(format(Sys.Date(), "%m")) < 4) {
    message("⚠️ Season not started yet. Adjusting end_season to previous year.")
    end_season <- current_year - 1
  }

  # Vector of years
  years <- start_season:end_season

  # Function to scrape a single season
  fetch_season <- function(year) {

    url <- paste0("https://www.baseball-reference.com/leagues/majors/", year, "-standings.shtml")
    page <- tryCatch(xml2::read_html(url), error = function(e) NULL)
    if (is.null(page)) {
      warning(paste("❌ Failed to fetch page for", year))
      return(NULL)
    }

    # Extract HTML comments
    comments <- rvest::html_nodes(page, xpath = "//comment()") |> rvest::html_text()
    expanded_html <- comments[grep("expanded_standings_overall", comments)]
    if (length(expanded_html) == 0) {
      warning(paste("❌ Expanded standings table not found for", year))
      return(NULL)
    }

    expanded_doc <- xml2::read_html(expanded_html)
    tables <- rvest::html_table(expanded_doc, fill = TRUE)
    table <- tables[[1]]

    # Add Year column and remove NA rows
    table <- dplyr::mutate(table, Year = year, .before = 1)
    table <- dplyr::filter(table, !is.na(Rk))

    # Rename columns safely
    colnames(table) <- c("Year", "Rank", "Team", "W", "L", "Wpct", "R_pG", "RA_pG", "Rdiff_pG", "SOS", "SRS", "pythWL",
                         "Luck", "vEast", "vCent", "vWest", "Inter", "Home", "Road", "ExInn", "OneRun", "vRHP", "vLHP",
                         "Greater_500", "Less_500", "Streak", "last10", "last20", "last30")

    # Convert numeric columns
    num_cols <- setdiff(names(table), c("Year", "Rank", "Team", "Streak")) # Keep text columns as is
    table[num_cols] <- lapply(table[num_cols], function(x) suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x))))

    return(table)
  }

  message(paste0("Fetching expanded standings from ", start_season, " until ", end_season))

  # Use pblapply to show progress
  standings_list <- pbapply::pblapply(years, fetch_season)

  # Filter NULL results
  standings_list <- standings_list[!sapply(standings_list, is.null)]

  if (length(standings_list) == 0) {
    stop("No standings data could be retrieved.")
  }

  # Combine all years into one data frame
  return(dplyr::bind_rows(standings_list))
}
