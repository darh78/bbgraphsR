#' @title Plot Running Total of MLB Debuts by Country
#'
#' @description
#' Fetches player debut data from Baseball Reference and plots a cumulative line chart
#' of MLB player debuts over time. Countries can be compared via color-coded lines.
#'
#' @param countries A character vector of country names (e.g. "Dominican Republic", "Venezuela")
#' @param start Optional. An integer year (e.g. 1990) to begin the plot. Default: show all available data.
#' @param end Optional. An integer year (e.g. 2020) to end the plot. Default: show all available data.
#' @param save Optional. Logical. If TRUE (default) save PNG in /output.
#'
#' @return A ggplot2 object.
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate count group_by arrange ungroup summarise left_join n
#' @importFrom ggplot2 ggplot aes geom_line labs annotation_custom
#' @importFrom lubridate year with_tz
#' @importFrom ggimage geom_image
#' @importFrom tidyr replace_na
#' @export
viz_debut_running_total_test <- function(countries, start = NULL, end = NULL, save = TRUE) {

  # Step 1: data
  df <- get_players_by_country(countries)
  scrape_date <- with_tz(Sys.time(), "US/Eastern") |> format("%Y-%m-%d %H:%M %Z")

  # Unknown-debut table (always create; print only if >0)
  players_no_date <- df |>
    dplyr::filter(is.na(Debut)) |>
    dplyr::group_by(Country) |>
    dplyr::summarise(Players = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(Players))

  if (nrow(players_no_date) > 0) {
    message("There are players without a known Debut date; they are excluded from the timeline.")
    print(players_no_date)
  }

  # Keep only dated rows for the timeline + extract year
  df <- df |>
    dplyr::filter(!is.na(Debut)) |>
    dplyr::mutate(Year = lubridate::year(Debut))

  # Optional filter
  if (!is.null(start)) df <- dplyr::filter(df, Year >= start)
  if (!is.null(end))   df <- dplyr::filter(df, Year <= end)

  if (nrow(df) == 0) stop("No player debut data found for the specified year range.")

  # Bookends + subtitle dates
  min_year <- min(df$Year); max_year <- max(df$Year)
  first_debut <- format(min(df$Debut), "%d-%b-%Y")
  last_debut  <- format(max(df$Debut), "%d-%b-%Y")

  # Counts + running totals
  debut_counts <- df |>
    dplyr::count(Country, Year, name = "Debuts") |>
    dplyr::arrange(Country, Year) |>
    dplyr::group_by(Country) |>
    dplyr::mutate(RunningTotal = cumsum(Debuts)) |>
    dplyr::ungroup()

  # --- Zoom-proof spacing: compute dx/dy from data range ---
  xr <- range(debut_counts$Year, na.rm = TRUE)
  yr <- range(debut_counts$RunningTotal, na.rm = TRUE)
  dx <- diff(xr) * 0.03   # 3% of x-range between elements
  dy <- diff(yr) * 0.01   # 1% of y-range vertical nudge

  # Final point per country + positions for bracket/label/flag
  label_points <- debut_counts |>
    dplyr::group_by(Country) |>
    dplyr::filter(Year == max(Year)) |>
    dplyr::ungroup() |>
    dplyr::left_join(country_flags, by = "Country") |>
    dplyr::left_join(players_no_date, by = "Country") |>
    dplyr::mutate(
      Players    = tidyr::replace_na(Players, 0L),
      x_lineend  = Year,
      x_bracket  = x_lineend + 1*dx,
      x_label    = x_lineend + 2*dx,
      x_flag     = x_lineend + 3.2*dx
    )

  # Plot
  p <- ggplot2::ggplot(debut_counts, ggplot2::aes(x = Year, y = RunningTotal, color = Country)) +
    ggplot2::geom_line(linewidth = 1.5, alpha = 0.6) +

    # Flag to the far right
    ggimage::geom_image(
      data = label_points,
      ggplot2::aes(x = x_flag, y = RunningTotal, image = FlagURL),
      size = 0.035, inherit.aes = FALSE
    ) +

    # Bracket bar for unknowns (if any)
    ggplot2::geom_segment(
      data = dplyr::filter(label_points, Players > 0),
      ggplot2::aes(x = x_bracket, xend = x_bracket,
                   y = RunningTotal, yend = RunningTotal + Players),
      linewidth = 1, color = "grey50", inherit.aes = FALSE
    ) +

    # +N label above the bracket
    ggplot2::geom_text(
      data = dplyr::filter(label_points, Players > 0),
      ggplot2::aes(x = x_bracket, y = RunningTotal + Players + 1*dy,
                   label = paste0("+", Players)),
      hjust = 0.5, vjust = 0, size = 3.2, color = "grey30", inherit.aes = FALSE
    ) +

    # Cumulative total, left aligned (so it doesn't lean into the bracket)
    ggplot2::geom_text(
      data = label_points,
      ggplot2::aes(x = x_label, y = RunningTotal,
                   label = scales::comma(RunningTotal), color = Country),
      size = 3.8, fontface = "bold", hjust = 0, inherit.aes = FALSE
    ) +

    ggplot2::labs(
      title = paste0("Cumulative MLB Player Debuts by Country (Period ", min_year, " - ", max_year, ")"),
      subtitle = paste("First player debuted on", first_debut, "and last one on", last_debut),
      x = "Year", y = "Cumulative Debuts",
      caption = paste(
        "Data retrieved on:", scrape_date,
        "\nGrey brackets indicate players with unknown debut years, not placed on the timeline."
      )
    ) +
    theme_bbgraphs() +
    ggplot2::theme(legend.position = "none") +
    # add right-side breathing room so nothing hits the edge
    ggplot2::scale_x_continuous(
      breaks = pretty(debut_counts$Year, n = 10),
      expand = ggplot2::expansion(mult = c(0.02, 0.20))
    )

  if (save) {
    ggplot2::ggsave("output/mlb_running_debuts.png", plot = p,
                    width = 12, height = 8, dpi = 320, bg = "gray90")
  }

  return(p)
}
