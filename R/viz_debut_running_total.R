#' @title Plot Running Total of MLB Debuts by Country
#'
#' @description
#' Fetches player debut data from Baseball Reference and plots a cumulative line chart
#' of MLB player debuts over time. Countries can be compared via color-coded lines.
#'
#' @param countries A character vector of country names (e.g. "Dominican Republic", "Venezuela")
#'
#' @return A ggplot2 object (line chart of cumulative MLB debuts).
#'
#' @examples
#' \dontrun{
#'   plot_debut_running_total(c("Venezuela", "Dominican Republic", "Cuba"))
#' }
#'
#' @importFrom dplyr filter mutate count group_by arrange ungroup
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' @importFrom lubridate year
#' @importFrom ggimage geom_image
#' @export
viz_debut_running_total <- function(countries) {

  # Flag image URLs (you can expand or customize this table)
  country_flags <- data.frame(
    Country = c("Dominican Republic", "Venezuela", "Cuba", "Puerto Rico", "Japan", "Mexico"),
    FlagURL = c(
      "https://flagcdn.com/w40/do.png",
      "https://flagcdn.com/w40/ve.png",
      "https://flagcdn.com/w40/cu.png",
      "https://flagcdn.com/w40/pr.png",
      "https://flagcdn.com/w40/jp.png",
      "https://flagcdn.com/w40/mx.png"
    ),
    stringsAsFactors = FALSE
  )

  # Step 1: Scrape player debut data
  df <- get_players_by_country(countries)

  # Step 2: Prepare yearly cumulative data
  df <- df |>
    filter(!is.na(Debut)) |>
    mutate(Year = year(Debut))

  debut_counts <- df |>
    count(Country, Year, name = "Debuts") |>
    arrange(Country, Year) |>
    group_by(Country) |>
    mutate(RunningTotal = cumsum(Debuts)) |>
    ungroup()

  # Step 3: Get final points per country to place flags
  label_points <- debut_counts |>
    group_by(Country) |>
    filter(Year == max(Year)) |>
    left_join(country_flags, by = "Country")

  # Step 4: Plot
  ggplot(debut_counts, aes(x = Year, y = RunningTotal, color = Country)) +
    geom_line(linewidth = 1) +
    ggimage::geom_image(
      data = label_points,
      aes(x = Year + 3, y = RunningTotal, image = FlagURL),
      size = 0.04,
      inherit.aes = FALSE
    ) +
    labs(
      title = "Cumulative MLB Player Debuts by Country",
      x = "Year",
      y = "Cumulative Debuts"
    ) +
    theme_minimal()
}
