#' @title Plot Running Total of MLB Debuts by Country
#'
#' @description
#' Fetches player debut data from Baseball Reference and plots a cumulative line chart
#' of MLB player debuts over time. Countries can be compared via color-coded lines.
#'
#' @param countries A character vector of country names (e.g. "Dominican Republic", "Venezuela")
#' @param start Optional. An integer year (e.g. 1990) to begin the plot. Default: show all available data.
#' @param end Optional. An integer year (e.g. 2020) to end the plot. Default: show all available data.
#' @param save Optional. A Boolean option. To exclude saving the chart to the /output folder
#'
#' @return A ggplot2 object (line chart of cumulative MLB debuts).
#'
#' @examples
#' \dontrun{
#'   viz_debut_running_total(c("Venezuela", "Dominican Republic", "Cuba"))
#' }
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate count group_by arrange ungroup
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal annotation_custom
#' @importFrom lubridate year
#' @importFrom ggimage geom_image
#' @importFrom tidyr replace_na
#' @export
viz_debut_running_total <- function(countries, start = NULL, end = NULL, save = TRUE) {

  # Step 1: Scrape player debut data
  df <- get_players_by_country(countries)
  scrape_date <- with_tz(Sys.time(), "US/Eastern")  |>
    format("%d-%b-%Y %H:%M %Z")

  # Step 2.1: Identify if there are players without a known debut date, and inform the user they will be dismissed in the chart

  players_no_date <- df |>
    filter(is.na(Debut)) |>
    group_by(Country) |>
    summarise(Players_Unknown_debut = dplyr::n(), .groups = "drop") |>
    arrange(desc(Players_Unknown_debut))

  if(sum(is.na(players_no_date$Debut)) > 0) {
    message("There are some players that don't have a known Debut date (see below), thus they will not be included in the visualization")
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

  # Step 3.1: Identifying min, max Year and Debut
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

  # Create zoom‑proof spacing using fractions of the data range: dx (x spacing) and dy (vertical nudge)
  xr <- range(debut_counts$Year, na.rm = TRUE)
  yr <- range(debut_counts$RunningTotal, na.rm = TRUE)
  dx <- diff(xr) * 0.0025   # ~3% of x-range
  dy <- diff(yr) * 0.01   # ~1% of y-range

  # use the farther of flag or label as the driver
  right_target <- max(c(label_points$x_flag, label_points$x_label), na.rm = TRUE)
  right_extra_x <- right_target - xr[2]
  right_mult <- max(right_extra_x / diff(xr) + 0.01, 0.04)  # min ~4%
  right_mult <- min(right_mult, 0.12)                       # cap at 12%

  # slightly pull things in so we need less space
  label_points <- label_points |>
    dplyr::mutate(
      x_flag  = Year + 1.6*dx,
      x_label = Year + 2.3*dx   # was ~3.2*dx before
    )

  best_years <- debut_counts |>
    group_by(Country) |>
    slice_max(Debuts, n = 1, with_ties = TRUE) |> # keep ties on value
    slice_max(Year, n = 1, with_ties = FALSE) |> # # pick most recent
    ungroup() |>
    select(Year, Country, Max_Debuts = Debuts, RunningTotal) |>
    arrange(Year)

  # label_points (replace your Step 6 block)
  label_points <- debut_counts |>
    dplyr::group_by(Country) |>
    dplyr::filter(Year == max(Year)) |>
    dplyr::ungroup() |>
    dplyr::left_join(country_flags, by = "Country") |>
    dplyr::left_join(players_no_date, by = "Country") |>
    dplyr::mutate(
      Players_Unknown_debut = tidyr::replace_na(Players_Unknown_debut, 0L),
      label_text = ifelse(Players_Unknown_debut > 0,
                          paste0(RunningTotal, " (+", Players_Unknown_debut, ")"),
                          as.character(RunningTotal)),
      x_line_end = Year,
      x_flag  = x_line_end + 2.2*dx,   # flag a bit right of the line end
      x_label = x_line_end + 3.2*dx    # label further right than the flag
    ) |>
    # stagger labels vertically when end totals are close
    dplyr::arrange(RunningTotal) |>
    dplyr::mutate(
      # create an alternating sequence: 0, +1, -1, +2, -2, ...
      alt = (seq_len(dplyr::n()) - 1L),
      alt = ifelse(alt == 0, 0,
                   ifelse(alt %% 2 == 1,  (alt + 1) / 2, -(alt / 2))),
      y_label = RunningTotal + alt * (2.5 * dy)   # tweak 2.5 if still tight
    )

  # build custom grid lines
  xbreaks <- pretty(debut_counts$Year, n = 10)
  xbreaks <- xbreaks[xbreaks >= min(xr) & xbreaks <= max(xr)]  # keep <= max_year
  ybreaks <- pretty(debut_counts$RunningTotal, n = 6)


  # Step 7: Plot
  p <- ggplot(debut_counts, aes(x = Year, y = RunningTotal, color = Country)) +
    geom_line(linewidth = 1.5, alpha = 0.6) +
    ggimage::geom_image(
      data = label_points,
      aes(
        x = x_flag,
        y = RunningTotal,
        image = FlagURL
      ),
      size = 0.04,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = best_years,
      aes(x = Year, y = RunningTotal),
      shape = 23,         # a star shape
      size = 1,           # bigger point
      color = "black",    # border
      fill = "yellow",       # fill color
      stroke = .5        # border width
    ) +
    geom_text(
      data = label_points,
      aes(
        x = x_label,
        y = RunningTotal,
        label = label_text
      ),
      hjust = 0, vjust = 0.5,
      size = 3.5,
      fontface = "bold",
      color = "grey10",
      inherit.aes = FALSE
    ) +
    # add right-side breathing room and allow drawing past panel
    scale_x_continuous(
      breaks = pretty(debut_counts$Year, n = 10),
      expand = expansion(mult = c(0.02, right_mult))
    ) +
    coord_cartesian(clip = "off") +
  # vertical grid segments (from y min to y max, but only up to max_year)
  geom_segment(
    data = data.frame(x = xbreaks),
    aes(x = x, xend = x, y = min(yr), yend = max(yr)),
    inherit.aes = FALSE, color = "grey85", linewidth = 0.35
  ) +
    # horizontal grid segments (from min_year to max_year)
    geom_segment(
      data = data.frame(y = ybreaks),
      aes(x = min(xr), xend = max(xr), y = y, yend = y),
      inherit.aes = FALSE, color = "grey85", linewidth = 0.35
    ) +
    # turn off the default panel grid so we only see our segments
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(
      title = paste0("Cumulative MLB Player Debuts by Country (Period ", min_year, " - ", max_year, ")"),
      subtitle = paste("First player debuted on", first_debut, "and last one on", last_debut),
      x = "Year",
      y = "Cumulative Debuts",
      caption = paste(
        "Data retrieved on:", scrape_date,
        "\nNumbers in brackets indicate additional players with unknown debut dates, not plotted in the timeline."
      )
    ) +
    theme_bbgraphs() +
    theme(legend.position = "none")

  if (save) {
    ggsave("output/mlb_running_debuts.png", plot = p, width = 12, height = 8, dpi = 320, bg = "gray90")
  }

    return(p)
}
