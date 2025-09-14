#' @title Cumulative MLB Debuts by Country (Highcharter)
#' @description Interactive cumulative lines with a bracket for unknowns and flags per country.
#'
#' @param countries Character vector of countries.
#' @param start,end Optional integer years to filter range.
#' @param save_html Logical; if TRUE saves an HTML widget to `file`.
#' @param file Path for HTML when `save_html = TRUE`.
#' @return A highcharter htmlwidget.
#'
#' @examples
#' \dontrun{
#'   viz_debut_running_total_hc(c("Venezuela","Dominican Republic","Cuba"))
#' }
#'
#' @import highcharter
#' @importFrom dplyr filter mutate count group_by arrange ungroup summarise left_join n
#' @importFrom lubridate year with_tz
#' @importFrom tidyr replace_na
#' @importFrom purrr pmap
#' @export
viz_debut_running_total_hc <- function(countries, start = NULL, end = NULL,
                                       save_html = FALSE,
                                       file = "output/mlb_running_debuts.html") {

  # --- data -------------------------------------------------------------------
  df <- get_players_by_country(countries)

  scrape_date <- lubridate::with_tz(Sys.time(), "US/Eastern") |>
    format("%Y-%m-%d %H:%M %Z")

  # unknown-debut counts (always build)
  players_no_date <- df |>
    dplyr::filter(is.na(Debut)) |>
    dplyr::group_by(Country) |>
    dplyr::summarise(Players = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(Players))

  if (nrow(players_no_date) > 0) {
    message("There are players without a known Debut date; they are excluded from the timeline.")
    print(players_no_date)
  }

  # dated rows + year
  df <- df |>
    dplyr::filter(!is.na(Debut)) |>
    dplyr::mutate(Year = lubridate::year(Debut))

  if (!is.null(start)) df <- dplyr::filter(df, Year >= start)
  if (!is.null(end))   df <- dplyr::filter(df, Year <= end)
  if (nrow(df) == 0) stop("No player debut data found for the specified year range.")

  min_year <- min(df$Year); max_year <- max(df$Year)
  first_debut <- format(min(df$Debut), "%d-%b-%Y")
  last_debut  <- format(max(df$Debut), "%d-%b-%Y")

  # counts + cumulative
  debut_counts <- df |>
    dplyr::count(Country, Year, name = "Debuts") |>
    dplyr::arrange(Country, Year) |>
    dplyr::group_by(Country) |>
    dplyr::mutate(RunningTotal = cumsum(Debuts)) |>
    dplyr::ungroup()

  # zoom-proof horizontal offsets (fraction of x-range)
  xr <- range(debut_counts$Year, na.rm = TRUE)
  dx <- diff(xr) * 0.03  # 3% of x-range for spacing

  # last point per country + flags + unknowns + positions
  label_points <- debut_counts |>
    dplyr::group_by(Country) |>
    dplyr::filter(Year == max(Year)) |>
    dplyr::ungroup() |>
    dplyr::left_join(country_flags, by = "Country") |>
    dplyr::left_join(players_no_date, by = "Country") |>
    dplyr::mutate(
      Players   = tidyr::replace_na(Players, 0L),
      x_lineend = Year,
      x_bracket = x_lineend,           # bracket exactly at the line end
      x_label   = x_lineend + 1.6*dx,  # number to the right
      x_flag    = x_lineend + 2.8*dx   # flag further right
    )

  # --- highcharter ------------------------------------------------------------
  hc <- highcharter::highchart() |>
    highcharter::hc_chart(type = "line") |>
    highcharter::hc_title(
      text = sprintf("Cumulative MLB Player Debuts by Country (%s–%s)", min_year, max_year)
    ) |>
    highcharter::hc_subtitle(
      text = sprintf("First debut: %s • Last debut: %s", first_debut, last_debut)
    ) |>
    highcharter::hc_xAxis(
      title = list(text = "Year"),
      min = min_year,
      max = max_year + round(4*dx),  # room for number + flag
      tickAmount = 10
    ) |>
    highcharter::hc_yAxis(title = list(text = "Cumulative Debuts")) |>
    highcharter::hc_tooltip(shared = TRUE, valueDecimals = 0) |>
    highcharter::hc_legend(enabled = TRUE) |>
    highcharter::hc_exporting(enabled = TRUE) |>
    highcharter::hc_credits(
      enabled = TRUE,
      text = paste0("Data retrieved ", scrape_date, " • Brackets show players with unknown debut years"),
      href = NULL
    )

  # match your ggplot theme colors (replace with your package palette if different)
  bbgraphs_colors <- c(
    "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
    "#66A61E", "#E6AB02", "#A6761D", "#666666"
  )
  hc <- highcharter::hc_colors(hc, bbgraphs_colors)

  # add each country as a proper line series (no hcaes, pass points explicitly)
  for (cty in unique(debut_counts$Country)) {
    df_cty <- debut_counts[debut_counts$Country == cty, c("Year","RunningTotal")]
    # build [{x: ..., y: ...}, ...]
    pts <- purrr::pmap(
      list(x = df_cty$Year, y = df_cty$RunningTotal),
      function(x, y) list(x = x, y = y)
    )
    hc <- highcharter::hc_add_series(
      hc,
      data  = highcharter::list_parse2(pts),
      type  = "line",
      name  = cty,
      marker = list(enabled = FALSE)
    )
  }

  # thin columnrange "brackets" at line ends (needs highcharts-more)
  hc <- highcharter::hc_add_dependency(hc, "modules/highcharts-more.js")
  if (any(label_points$Players > 0)) {
    sel <- label_points$Players > 0
    cr_points <- purrr::pmap(
      list(
        xb   = label_points$x_bracket[sel],
        y    = label_points$RunningTotal[sel],
        unk  = label_points$Players[sel],
        ctry = label_points$Country[sel]
      ),
      function(xb, y, unk, ctry) {
        list(x = xb, low = y, high = y + unk, country = ctry)
      }
    )
    if (length(cr_points)) {
      hc <- highcharter::hc_add_series(
        hc,
        data = highcharter::list_parse2(cr_points),
        type = "columnrange",
        name = "+ Unknown",
        color = "#808080",
        tooltip = list(pointFormat = "<b>{point.country}</b><br/>+{point.high - point.low} unknown"),
        pointWidth = 2,
        grouping = FALSE,
        showInLegend = TRUE
      )
    }
  }

  # numeric labels to the right (as scatter with dataLabels)
  lab_points <- purrr::pmap(
    list(
      xl   = label_points$x_label,
      y    = label_points$RunningTotal,
      ctry = label_points$Country
    ),
    function(xl, y, ctry) {
      list(x = xl, y = y, name = ctry, label = format(y, big.mark = ","))
    }
  )
  hc <- highcharter::hc_add_series(
    hc,
    data = highcharter::list_parse2(lab_points),
    type = "scatter",
    name = "Totals",
    color = "transparent",
    enableMouseTracking = FALSE,
    marker = list(enabled = FALSE),
    dataLabels = list(
      enabled = TRUE, align = "left", crop = FALSE, overflow = "allow",
      style = list(fontWeight = "bold"),
      formatter = htmlwidgets::JS("function(){ return this.point.label; }")
    ),
    showInLegend = FALSE
  )

  # flags to the far right (per-point image markers)
  if (!is.null(label_points$FlagURL)) {
    flags_points <- purrr::pmap(
      list(
        xf   = label_points$x_flag,
        y    = label_points$RunningTotal,
        ctry = label_points$Country,
        url  = label_points$FlagURL
      ),
      function(xf, y, ctry, url) {
        if (is.na(url) || !nzchar(url)) return(NULL)
        list(
          x = xf, y = y, name = ctry,
          marker = list(symbol = paste0("url(", url, ")"), radius = 10)
        )
      }
    )
    flags_points <- Filter(Negate(is.null), flags_points)
    if (length(flags_points)) {
      hc <- highcharter::hc_add_series(
        hc,
        data = highcharter::list_parse2(flags_points),
        type = "scatter",
        name = "Flags",
        tooltip = list(pointFormat = "<b>{point.name}</b>"),
        showInLegend = FALSE
      )
    }
  }

  if (isTRUE(save_html)) {
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
    htmlwidgets::saveWidget(hc, file = file, selfcontained = TRUE)
  }

  hc
}
