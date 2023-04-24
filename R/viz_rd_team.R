



viz_rd_team <- function(team, start_year, end_year, highlight_year = NULL) {

  ### Get the team's game results along the period to be visualized ----

  print(paste0("Getting games' data for ", team ," between ", start_year, " and ", end_year, " ..."))

  if (is.null(highlight_year)) {
    highlight_year <- end_year # If no highlighted year is given, by default it's the end year
  }

  years <- as.character(seq(start_year, end_year, 1))

  rd_tm <- pblapply(years, function(year) {
    baseballr::bref_team_results(Tm = team, year = year)
  })

  # Bind tables for all the years into one data frame ----
  rd_tm <- do.call("rbind", rd_tm)

  ### Tidying the `rd_tm` data frame ----

  rd_tm_2 <- rd_tm %>%
    tidyr::separate(Date, c("wd", "Date"), sep = ", ") %>%
    tidyr::separate(Record, c("W", "L"), sep = "-", remove = FALSE) %>%    # wd = weekday
    tidyr::unite(Date, c("Date", "Year"), sep = ", ", remove = FALSE) %>%
    dplyr::select(Gm, Date, Tm, Opp, R, RA, Record, W, L, Rank, GB, Year) %>%
    dplyr::group_by(Year) %>%
    dplyr::mutate(WLpct = round(as.numeric(W)/(as.numeric(W)+as.numeric(L)), 3),
                  R_Diff = R - RA,
                  cum_R_Diff = cumsum(R_Diff)) %>%
    dplyr::ungroup()

  names(rd_tm_2)[c(1,3)] <- c("Game", "Team")

  min_RDiff <- round(min(rd_tm_2$cum_R_Diff)*1.1)
  max_RDiff <- round(max(rd_tm_2$cum_R_Diff)*1.1)

  annot_years <- rd_tm_2  |>
    dplyr::group_by(Year)  |>
    dplyr::summarise(Game = max(Game) + 2,
              cum_R_Diff = sum(R_Diff))


  ### Creating the chart for the team for all the years requested ----

  rd_tm_2 <- rd_tm_2 %>%
    dplyr::mutate(line_type = ifelse(rd_tm_2$Year == as.character(highlight_year), "solid", "dashed"))

  ## Adding the area chart for the highlighted year
  rd_tm_viz <- highcharter::hchart(rd_tm_2[rd_tm_2$Year == as.character(highlight_year),],
                        "areaspline",
                        highcharter::hcaes(x = Game,
                                           y = cum_R_Diff),
                        marker = list(enabled = FALSE),
                        color = "#4B5463",
                        fillColor = "#D4DFD0",
                        negativeFillColor = "#FF988C",
                        showInLegend = FALSE,
                        fillOpacity = 0.2) %>%
    ## Adding the line charts for the remaining (grouped) years requested
    highcharter::hc_add_series(rd_tm_2[rd_tm_2$Year != as.character(highlight_year),],
                               "spline",
                               highcharter::hcaes(x = Game,
                                                  y = cum_R_Diff,
                                                  group = Year),
                               lineWidth = 1,
                               color = "gray",
                               marker = list(enabled = FALSE),
                               showInLegend = FALSE,
                               Opacity = 0.8) %>%
    ## Adding X axis customization
    highcharter::hc_xAxis(title = list(text = "Games"),
                          lineWidth = 4,
                          tickInterval = "1",
                          max = max(rd_tm_2$Game) + 30) %>%
    ## Adding Y axis customization
    highcharter::hc_yAxis(title = list(text = "Accum R Diff"),
                          min = min_RDiff,
                          max = max_RDiff) %>%
    ## Adding a general tooltip for the points in the chart
    highcharter::hc_tooltip(useHTML = TRUE,
                            headerFormat = "",
                            pointFormat = "<b>{point.Year}</b> season <br>
                                           <b>Date:</b> {point.Date} <br>
                                           <b>Games played:</b> {point.Game} <br>
                                           <b>Accum Run Diff:</b> {point.cum_R_Diff} <br>
                                           <b>W-L (%):</b> {point.Record} ({point.WLpct})<br>
                                           <b>Div Rank</b>: {point.Rank} <br>
                                           <b>GB:</b> {point.GB}",
                            borderWidth = 1,
                            borderColor = "#000000") %>%
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
            point = list(x = rd_tm_2$Game[which.max(rd_tm_2$cum_R_Diff)],
                         y = max(rd_tm_2$cum_R_Diff),
                         xAxis = 0,
                         yAxis = 0),
            text = paste0("<b>Max: </b>", max(rd_tm_2$cum_R_Diff), " (in ", rd_tm_2$Year[which.max(rd_tm_2$cum_R_Diff)], ")")
          ),
          # Annotation for the minimum accumulated RD for all the period in analysis
          list(
            point = list(x = rd_tm_2$Game[which.min(rd_tm_2$cum_R_Diff)],
                         y = min(rd_tm_2$cum_R_Diff),
                         xAxis = 0,
                         yAxis = 0),
            x = 50,
            y = 40,
            text = paste0("<b>Min: </b> ", min(rd_tm_2$cum_R_Diff), " (in ", rd_tm_2$Year[which.min(rd_tm_2$cum_R_Diff)], ")")
          )
          )
        )
        ) %>%
    ## Adding a theme for the chart
    highcharter::hc_add_theme(hc_theme_smpl()) %>%
    ## Adding a title for the chart
    highcharter::hc_title(text = paste(team, "<span style=\"color:#002d73\"> - Runs Differential </span>")) %>%
    ## Adding a subtitle for the chart
    highcharter::hc_subtitle(text = paste("Between ", start_year, " and", end_year, ", highlighting the ", highlight_year, "season.")) %>%
    ## Adding notes in the bottom of  the chart
    highcharter::hc_credits(enabled = TRUE,                          # add credits
                            text = paste0("Source: Baseball Reference. Using 'baseballr' R package. Retreived on: ",
                                          with_tz(Sys.time(), "US/Eastern") %>%
                                            format("%Y-%m-%d %H:%M %Z"))) %>%
    ## Enabling the menu to export the charts
    highcharter::hc_exporting(enabled = TRUE)

  # rd_tm_viz
  #
  # ## Create a list of annotations for each Year (except the highlighted)
  # annot_list <- lapply(1:nrow(annot_years), function(i) {
  #   list(
  #     point = list(x = annot_years$Game[i],
  #                  y = annot_years$cum_R_Diff[i],
  #                  xAxis = 0,
  #                  yAxis = 0),
  #     text = annot_years$Year[i],
  #     zIndex = 9999,
  #     labelOptions = list(
  #       backgroundColor = "gray",
  #       borderColor = "black",
  #       style = list(color = "white",
  #                    fontSize = "1em")
  #     )
  #   )
  #   }
  #   )
  #
  # rd_tm_viz <- rd_tm_viz %>%
  #   highcharter::hc_add_annotations(annot_list)

  rd_tm_viz %>%
  browsable()

}


