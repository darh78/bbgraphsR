#' @title Scrape MLB Players Born in a Given Country
#'
#' @description
#' Scrapes the Baseball Reference website for a table of all MLB players born
#' in the specified country. The function returns the full table with cleaned
#' columns, converts birth dates and debut dates to `Date` class, splits the
#' birthplace into city and state (if available), and sorts the players by
#' debut date (ascending).
#'
#' @param country A character vector of country names (e.g., "Dominican Republic", "Venezuela").
#'
#' @return A data frame with all players listed for each country, including:
#' - All columns from the batting table on Baseball Reference
#' - A `Country` column
#' - `City` and `State` columns extracted from `Birthplace`
#'
#' @details
#' The function accesses pages like `https://www.baseball-reference.com/bio/<country>_born.shtml`.
#' It handles multiple countries and skips any unavailable pages gracefully.
#'
#' @examples
#' \dontrun{
#'   scrape_country_birth_players("Dominican Republic")
#'   scrape_country_birth_players(c("Venezuela", "Cuba"))
#' }
#'
#' @importFrom rvest read_html html_node html_table
#' @importFrom dplyr mutate filter arrange bind_rows
#' @importFrom stringr str_replace_all str_trim word str_detect
#' @importFrom lubridate mdy
#' @export
scrape_country_birth_players <- function(country) {

  scrape_single_country <- function(cn) {
    slug <- str_replace_all(cn, " ", "-")
    url <- paste0("https://www.baseball-reference.com/bio/", slug, "_born.shtml")

    tryCatch({
      page <- read_html(url)
      tbl <- page |>
        html_node("table") |>
        html_table(fill = TRUE)

      # Filter and clean
      tbl <- tbl |>
        filter(!is.na(Rk)) |>
        mutate(
          Birthdate = suppressWarnings(mdy(Birthdate)),
          Debut = suppressWarnings(mdy(Debut)),
          Country = cn
        )

      # Extract City and State if possible
      if ("Birthplace" %in% names(tbl)) {
        tbl <- tbl |> mutate(
          City = str_trim(word(Birthplace, 1, sep = ",")),
          State = ifelse(str_detect(Birthplace, ","),
                         str_trim(word(Birthplace, 2, sep = ",")),
                         NA_character_)
        )
      }

      tbl |> arrange(Debut)

    }, error = function(e) {
      message("Failed to scrape ", cn, ": ", conditionMessage(e))
      return(NULL)
    })
  }

  # Loop over all countries and bind results
  result <- lapply(country, scrape_single_country) |> bind_rows()
  return(result)
}
