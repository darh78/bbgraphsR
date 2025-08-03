#' @title Plot Running Total of MLB Debuts by Country
#'
#' @description
#' Fetches player debut data from Baseball Reference and plots a cumulative line chart
#' of MLB player debuts over time. Countries can be compared via color-coded lines.
#'
#' @param countries A character vector of country names (e.g. "Dominican Republic", "Venezuela")
#' @param start Optional. An integer year (e.g. 1990) to begin the plot. Default: show all available data.
#' @param end Optional. An integer year (e.g. 2020) to end the plot. Default: show all available data.
#'
#' @return A ggplot2 object (line chart of cumulative MLB debuts).
#'
#' @examples
#' \dontrun{
#'   viz_debut_running_total(c("Venezuela", "Dominican Republic", "Cuba"))
#' }
#'
#' @importFrom dplyr filter mutate count group_by arrange ungroup
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' @importFrom lubridate year
#' @importFrom ggimage geom_image
#' @export
viz_debut_running_total <- function(countries, start = NULL, end = NULL) {

  # Step 1: Scrape player debut data
  df <- get_players_by_country(countries)
  scrape_date <- with_tz(Sys.time(), "US/Eastern")  |>
    format("%Y-%m-%d %H:%M %Z")

  # Step 2.1: Identify if there are players without a known debut date, and inform the user they will be dismissed

  if(sum(is.na(df$Debut)) > 0) {
    message("There are some players that don't have a known Debut date, thus they are removed in the visualization")
    players_no_date <- df |>
      filter(is.na(Debut)) |>
      group_by(Country) |>
      summarise(Players = dplyr::n(), .groups = "drop") |>
      arrange(desc(Players))

    print(players_no_date)
  }

  # Step 2.2: Clean and extract year
  df <- df |>
    filter(!is.na(Debut)) |>
    mutate(Year = year(Debut))

  # Step 3: Apply year range filtering
  if (!is.null(start)) {
    df <- df |> filter(Year >= start)
  }
  if (!is.null(end)) {
    df <- df |> filter(Year <= end)
  }

  # Step 3.1: Identifying min a max Year
  min_year <- min(df$Year)
  max_year <- max(df$Year)
  first_debut <- format(min(df$Debut), "%d-%b-%Y")
  last_debut <- format(max(df$Debut), "%d-%b-%Y")

  # Step 4: Stop if no data remains
  if (nrow(df) == 0) {
    stop("No player debut data found for the specified year range.")
  }

  # Step 5: Proceed with count and cumsum
  debut_counts <- df |>
    count(Country, Year, name = "Debuts") |>
    arrange(Country, Year) |>
    group_by(Country) |>
    mutate(RunningTotal = cumsum(Debuts)) |>
    ungroup()

  # Step 6: Get final points per country to place flags
  label_points <- debut_counts |>
    mutate(YearMin = min_year) |>
    group_by(Country) |>
    filter(Year == max(Year)) |>
    left_join(country_flags, by = "Country")

  # Step 7: Plot
  p <- ggplot(debut_counts, aes(x = Year, y = RunningTotal, color = Country)) +
    geom_line(linewidth = 1.5, alpha = 0.6) +
    ggimage::geom_image(
      data = label_points,
      aes(x = (Year - YearMin)*1.05 + YearMin, y = RunningTotal, image = FlagURL),
      size = 0.04,
      inherit.aes = FALSE
    ) +
    labs(
      title = "Cumulative MLB Player Debuts by Country",
      subtitle = paste("First player debut on", first_debut, "and last one on", last_debut),
      x = "Year",
      y = "Cumulative Debuts",
      caption = paste("Data retrived on:", scrape_date)
    ) +
    theme_bbgraphs() +
    theme(legend.position = "none")

  p

    ggsave("output/mlb_debuts.png", plot = p, width = 12, height = 8, dpi = 320, bg = "gray90")

}
