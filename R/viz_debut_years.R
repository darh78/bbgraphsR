#' @title Plot Yearly MLB Debuts by Country (Grouped Columns with Annotation)
#'
#' @description
#' Fetches MLB player debut data by country and plots a grouped bar chart showing the number of debuts per year.
#' Each country is shown as a separate colored column within each year.
#' An embedded summary table is also included in the top-left corner of the plot, indicating the peak debut year for each selected country.
#'
#' @param countries A character vector of country names (e.g. "Dominican Republic", "Venezuela").
#' These names must match the values returned by the `Country` column in the `get_players_by_country()` function.
#'
#' @param start Optional. An integer year (e.g. `1990`) to begin the plot. Only debuts from this year onward are shown.
#'
#' @param end Optional. An integer year (e.g. `2020`) to end the plot. Only debuts up to this year are shown.
#'
#' @return A `ggplot2` object representing the grouped bar chart with annotations and summary table.
#'
#' @details
#' - Players with missing debut dates are excluded from the plot.
#' - The peak debut year per country is computed and embedded as a `ggtexttable` object on the chart.
#' - The X-axis uses a smart sequence of decade-based labels with the first and last year always included.
#'
#' @section Output:
#' The plot is saved automatically to `output/mlb_debuts_yearly_grouped.png` (12x8 inches, 320 dpi).
#'
#' @examples
#' \dontrun{
#'   viz_debut_years(c("Venezuela", "Dominican Republic", "Cuba"))
#'   viz_debut_years(c("Japan", "Mexico", "Puerto Rico"), start = 1980, end = 2025)
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom lubridate year
#' @importFrom ggpubr ggtexttable ttheme
#' @importFrom grid annotation_custom
#' @importFrom ggplot2 annotate
#' @export
viz_debut_years <- function(countries, start = NULL, end = NULL) {

  # Step 1: Load data
  df <- get_players_by_country(countries)

  scrape_date <- with_tz(Sys.time(), "US/Eastern") |>
    format("%Y-%m-%d %H:%M %Z")

  # Step 2: Handle NAs
  if (sum(is.na(df$Debut)) > 0) {
    message("There are some players that don't have a known Debut date, thus they are removed in the visualization")
    players_no_date <- df |>
      filter(is.na(Debut)) |>
      group_by(Country) |>
      summarise(Players = dplyr::n(), .groups = "drop") |>
      arrange(desc(Players))
    print(players_no_date)
  }

  # Step 3: Prepare date
  df <- df |>
    filter(!is.na(Debut)) |>
    mutate(Year = year(Debut))

  # Step 4: Filter by year
  if (!is.null(start)) df <- df |> filter(Year >= start)
  if (!is.null(end))   df <- df |> filter(Year <= end)

  # Step 5: Abort if empty
  if (nrow(df) == 0) stop("No player debut data found for the specified year range.")

  # Step 6: Count debuts
  debut_yearly <- df |>
    count(Year, Country, name = "Debuts")

  # 📌 Step 7: Create table of peak debut year per country
  # Peak table and text
  peak_table <- debut_yearly |>
    group_by(Country) |>
    dplyr::slice_max(order_by = Debuts, n = 1) |>
    arrange(desc(Debuts)) |>
    select(Country, Year, Debuts)

  # Format column names
  colnames(peak_table) <- c("Country", "Year", "Debuts")

  # Create the ggtexttable object
  peak_tbl <- ggtexttable(
    peak_table,
    rows = NULL,
    theme = ttheme("classic")  # or use another theme
  )

  # Peak text
  peak_text <- peak_table |>
    mutate(Text = paste0(Country, ": ", Debuts, " debuts in ", Year)) |>
    pull(Text) |>
    paste(collapse = "\n")

  # Step 8: Summary for subtitle
  first_debut <- format(min(df$Debut), "%d-%b-%Y")
  last_debut  <- format(max(df$Debut), "%d-%b-%Y")

  # Step 9: Get min and max years
  years_all <- sort(unique(debut_yearly$Year))
  min_year <- min(years_all)
  max_year <- max(years_all)

  # Step 10: Compute decade breaks and add first and last year
  breaks_decades <- seq(ceiling(min_year / 10) * 10, floor(max_year / 10) * 10, by = 10)
  breaks_final <- sort(unique(c(min_year, breaks_decades, max_year)))

  # Step 11: Fallback font setting (do this once)
  fallback_font <- if ("Roboto" %in% systemfonts::system_fonts()$family) "Roboto" else "sans"

  # Step 12: Plot (grouped columns)
  p <- p <- ggplot(debut_yearly, aes(x = Year, y = Debuts, fill = Country)) +
    geom_col(position = "dodge", width = 0.8) +
    labs(
      title = "MLB Debuts Per Year by Country",
      subtitle = paste("First player debut on", first_debut, "and last one on", last_debut),
      x = "Year", y = "Player Debuts",
      caption = paste("Data retrieved on:", scrape_date)
    ) +
    scale_x_continuous(breaks = pretty(debut_yearly$Year, n = 10)) +
    theme_bbgraphs(font_family = fallback_font) +
    labs(
      title = "Yearly MLB Player Debuts by Country",
      subtitle = paste("First player debut on", first_debut, "and last one on", last_debut),
      x = "Year",
      y = "Number of Debuts",
      caption = paste("Data retrieved on:", scrape_date)
    ) +
    theme(legend.position = "bottom")

  # Convert to grob and place it in the top-left area
  p <- p +
    annotation_custom(
      grob = ggplotGrob(peak_tbl),
      xmin = min(debut_yearly$Year) + 2,  # adjust for padding
      xmax = min(debut_yearly$Year) + 45,
      ymin = max(debut_yearly$Debuts) - 5,
      ymax = max(debut_yearly$Debuts)
    ) +
    annotate("text",
             x = min(debut_yearly$Year) + 2,
             y = max(debut_yearly$Debuts) + 2,
             label = "Years with Most Debuts",
             hjust = 0, size = 4, fontface = "bold")
  p

  # Save
  ggsave("output/mlb_debuts_yearly_grouped.png", plot = p, width = 12, height = 8, dpi = 320, bg = "gray90")

  return(p)
}
