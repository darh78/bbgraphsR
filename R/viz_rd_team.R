#' @title Graph of Accumulated Runs Differential for a team over several seasons
#'
#' @param team a string input, the Baseball Reference Team abbreviation
#' @param start_year a numeric value, the starting MLB season to be analyzed
#' @param end_year a numeric value, the ending MLB season to be analyzed
#' @param highlight_year a numeric value, one of the seasons (within the period start-end) which will be highlithed in the chart
#'
#' @return A areaspline & line types chart with the accumulated run differential for the Team along the seasons analyzed
#' @export
#'
#' @importFrom baseballr bref_team_results
#' @importFrom pbapply pblapply
#' @importFrom dplyr select group_by mutate ungroup summarise
#' @importFrom tidyr separate unite
#' @importFrom highcharter hchart hcaes hc_tooltip hc_add_theme hc_theme_smpl hc_xAxis hc_yAxis hc_title hc_subtitle hc_credits hc_exporting hc_add_series hc_annotations
#'
#' @examples
#' viz_rd_team("BOS", 2017, 2022, 2018)
#' ## returns an RD chart for BOS between 2010-2022, highlighting 2018
#' \dontrun{
#' viz_rd_team("TBR", 2019, 2023)
#' ## returns an RD chart for TBR between 2015-2023, highlighting 2023
#' }

viz_rd_team <- function(team, start_year, end_year, highlight_year = NULL) {

  ### Get the team's game results along the period to be visualized ----

  print(paste0("Getting games' data for ", team ," between ", start_year, " and ", end_year, " ..."))

  if (is.null(highlight_year)) {
    # If no highlighted year is given, by default it's the end year
    highlight_year <- end_year
  }

  # creating a vector of all the years to be analyzed
  years <- as.character(seq(start_year, end_year, 1))

  # Getting the team results along the years requested
  rd_tm <- pblapply(years, function(year) {
    baseballr::bref_team_results(Tm = team, year = year)
  })

  # Bind tables for all the years into one data frame ----
  rd_tm <- do.call("rbind", rd_tm)

  ### Tidying the `rd_tm` data frame ----

  rd_tm_2 <- rd_tm  |>
    tidyr::separate(Date, c("wd", "Date"), sep = ", ")  |>
    tidyr::separate(Record, c("W", "L"), sep = "-", remove = FALSE)  |>     # wd = weekday
    tidyr::unite(Date, c("Date", "Year"), sep = ", ", remove = FALSE)  |>
    dplyr::select(Gm, Date, Tm, Opp, R, RA, Record, W, L, Rank, GB, Year)  |>
    dplyr::group_by(Year)  |>
    dplyr::mutate(WLpct = round(as.numeric(W)/(as.numeric(W)+as.numeric(L)), 3),
                  RD = R - RA,
                  cum_RD = cumsum(RD))  |>
    dplyr::ungroup()

  names(rd_tm_2)[c(1,3)] <- c("Game", "Team")

  min_RD <- round(min(rd_tm_2$cum_RD)*1.1)
  max_RD <- round(max(rd_tm_2$cum_RD)*1.1)

  ### Creating the chart for the team for all the years requested ----

  ## Adding the line charts for the years requested (except the highlighted year)
  rd_tm_viz <- highcharter::hchart(rd_tm_2[rd_tm_2$Year != as.character(highlight_year),],
                                   "spline",
                                   highcharter::hcaes(x = Game,
                                                      y = cum_RD,
                                                      group = Year),
                                   lineWidth = 0.8,
                                   zoomType = "xy",
                                   color = "gray",
                                   marker = list(enabled = FALSE),
                                   showInLegend = FALSE,
                                   Opacity = 0.3)  |>
    ## Adding the areaspline chart for the highlighted year
    highcharter::hc_add_series(rd_tm_2[rd_tm_2$Year == as.character(highlight_year),],
                               "areaspline",
                               highcharter::hcaes(x = Game,
                                                  y = cum_RD),
                               marker = list(enabled = FALSE),
                               lineWidth = 3,
                               color = "#4B5463",
                               # setting the fill color for positive values and a opacity of 0.75
                               fillColor = "rgba(212, 223, 208, 0.75)",
                               # setting the fill color for negative values and a opacity of 0.75
                               negativeFillColor = "rbga(255, 152, 140, 0.8)",
                               showInLegend = FALSE,
                               # defining the highlighted year to be on top
                               zIndex = 100)  |>
    # adding points on the maximum of run differentials for the highlighted year
    highcharter::hc_add_series(rd_tm_2 |>
                                 dplyr::filter(Year == highlight_year) |>
                                 dplyr::filter(cum_RD == max(cum_RD)),
                               type = "scatter",
                               highcharter::hcaes(x = Game,
                                                  y = cum_RD),
                               color = "blue",
                               marker = list(symbol = "triangle",
                                             radius = 4,
                                             lineWidth = 1),
                               showInLegend = TRUE,
                               Opacity = 1,
                               zIndex = 120,
                               name = "Max in highligthed year")  |>
    # adding points on the minimum of run differentials for the highlighted year
    highcharter::hc_add_series(rd_tm_2 |>
                                 dplyr::filter(Year == highlight_year) |>
                                 dplyr::filter(cum_RD == min(cum_RD)),
                               type = "scatter",
                               highcharter::hcaes(x = Game,
                                                  y = cum_RD),
                               color = "darkred",
                               marker = list(symbol = "triangle-down",
                                             radius = 4,
                                             lineWidth = 1),
                               showInLegend = TRUE,
                               Opacity = 1,
                               zIndex = 120,
                               name = "Min in highligthed year") |>
    ## Adding X axis customization
    highcharter::hc_xAxis(title = list(text = "Games"),
                          lineWidth = 4,
                          tickInterval = "1",
                          max = max(rd_tm_2$Game))  |>
    ## Adding Y axis customization
    highcharter::hc_yAxis(title = list(text = "Accum R Diff"),
                          min = min_RD,
                          max = max_RD)  |>
    ## Adding a general tooltip for the points in the chart
    highcharter::hc_tooltip(useHTML = TRUE,
                            headerFormat = "",
                            pointFormat = "<b>{point.Year}</b> season <br>
                                           <b>Date:</b> {point.Date} <br>
                                           <b>Games played:</b> {point.Game} <br>
                                           <b>Accum Run Diff:</b> {point.cum_RD} <br>
                                           <b>W-L (%):</b> {point.Record} ({point.WLpct})<br>
                                           <b>Div Rank</b>: {point.Rank} <br>
                                           <b>GB:</b> {point.GB}",
                            borderWidth = 1,
                            borderColor = "#000000")  |>
    ## Adding annotations for min and max values
    highcharter::hc_annotations(
      list(
        # Type of annotation for the min and max
        labelOptions = list(
          shape = "connector",
          align = "right",
          justify = FALSE,
          crop = TRUE,
          style = list(fontSize = "1em",
                       textOutline = "1px white")
        ),
        labels = list(
          # Annotation for the maximum accumulated RD for all the period in analysis
          list(
            point = list(x = rd_tm_2$Game[which.max(rd_tm_2$cum_RD)],
                         y = max(rd_tm_2$cum_RD),
                         xAxis = 0,
                         yAxis = 0),
            text = paste0("<b>Max: </b>", max(rd_tm_2$cum_RD), " (in ", rd_tm_2$Year[which.max(rd_tm_2$cum_RD)], ")")
          ),
          # Annotation for the minimum accumulated RD for all the period in analysis
          list(
            point = list(x = rd_tm_2$Game[which.min(rd_tm_2$cum_RD)],
                         y = min(rd_tm_2$cum_RD),
                         xAxis = 0,
                         yAxis = 0),
            x = 50,
            y = 40,
            text = paste0("<b>Min: </b> ", min(rd_tm_2$cum_RD), " (in ", rd_tm_2$Year[which.min(rd_tm_2$cum_RD)], ")")
          )
          )
        )
        )  |>
    ## Adding a theme for the chart
    highcharter::hc_add_theme(hc_theme_smpl())  |>
    ## Adding a title for the chart
    highcharter::hc_title(text = paste(team, "<span style=\"color:#002d73\"> - Runs Differential </span>"))  |>
    ## Adding a subtitle for the chart
    highcharter::hc_subtitle(text = paste("Between ", start_year, " and", end_year, ", highlighting the ", highlight_year, "season."))  |>
    ## Adding notes in the bottom of  the chart
    highcharter::hc_credits(enabled = TRUE,                          # add credits
                            text = paste0("Source: Baseball Reference. Using 'baseballr' R package. Retrieved on: ",
                                          with_tz(Sys.time(), "US/Eastern")  |>
                                            format("%Y-%m-%d %H:%M %Z")))  |>
    ## Enabling the menu to export the charts
    highcharter::hc_exporting(enabled = TRUE)

  rd_tm_viz

}
