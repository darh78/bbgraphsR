







viz_bp <- function(startseason, endseason = NULL, lg = "all", bat_metric = "wOBA", pit_metric = "ERA") {

  if (is.null(endseason)) {

    until_season <- from_season # meaning just 1 season will be retrieved

  } else {
    seasons <- seq(startseason, endseason, by = 1)
  }

  bat <- baseballr::fg_team_batter(startseason, endseason, lg)
  pit <- baseballr::fg_team_pitcher(startseason, endseason, lg, ind = 1)

  bp <- dplyr::left_join(bat, pit,
                         by = c("Season", "Team"),
                         suffix = c(".bat", ".pit"))


  # Create a table of colors by team

  Teams_Lahman <- read.csv("data/Teams.csv")[ , -1]

  team_palette <- Teams_Lahman  |>
    dplyr::filter(yearID == 2022)  |>
    dplyr::select(name, teamIDBR)

  team_palette$name[team_palette$teamIDBR == "LAA"] <- "Los Angeles Angels"

  teamcolors <- read.csv("data/teamcolors.csv")[ , -1]

  team_palette <- team_palette |>
    dplyr::left_join(teamcolors, by = "name") |>
    dplyr::rename(Team = teamIDBR) |>
    dplyr::select(Team, primary, secondary)

  # Replace "primary" color of some teams by its "secondary" color
  team_palette[c(1, 2, 5, 11, 16, 17, 21, 25, 27, 28), 2] <- team_palette[c(1, 2, 5, 11, 16, 17, 21, 25, 27, 28), 3]
  # 1 ARI, 2 ATL, 5 CHC, 11 HOU, 16 MIL, 17 MIN, 21 PHI, 25 SEA, 27 TBR, 28 TEX


  # Join "primary" color column to bp data.frame
  bp <- bp |>
    dplyr::left_join(team_palette, by = "Team") |>
    dplyr::select(Season, Team, AB, PA, H.bat, X1B = '1B', X2B = '2B', X3B = '3B', HR.bat, R.bat, RBI, BB.bat, IBB.bat, SO.bat, HBP.bat,
                  SF, SH, GDP, SB, CS, AVG.bat, GB.bat, LD.bat, BB_pct.bat, K_pct.bat, BB_K, OBP, SLG, OPS, ISO, BABIP.bat, GB_FB.bat,
                  LD_pct.bat, GB_pct.bat, FB_pct.bat, HR_FB.bat, wOBA, wRAA, wRC, WAR.bat, wRC_plus,
                  W, L, ERA, GS, CG, ShO, SV, BS, IP, H.pit, R.pit, ER, HR.pit, BB.pit, IBB.pit, HBP.pit, WP, BK, GB.pit, FB.pit, LD.pit,
                  K_9, BB_9, K_BB, H_9, HR_9, AVG.pit, WHIP, BABIP.pit, FIP, GB_FB.pit, LD_pct.pit, GB_pct.pit, FB_pct.pit, HR_FB.pit,
                  WAR.pit, ERA_adjusted = 'ERA-', FIP_adjusted = 'FIP-', primary, secondary) |>
    dplyr::mutate(WAR = WAR.bat + WAR.pit,
                  WLpct = round(W / (W+L), 3))

  bp <- bp[complete.cases(bp), ]

  ### Determine the columns for the grid chart, based on number of teams ----

  if (length(seasons) == 1 | length(seasons) == 3 | length(seasons) == 5) {
    viz_col <- 1
  } else if (length(seasons) == 2 | length(seasons) == 4 | length(seasons) == 6) {
    viz_col <- 2
  } else if (length(seasons) > 6 & length(seasons) < 10) {
    viz_col <- 3
  } else if (length(seasons) >= 10) {
    viz_col <- 4
  }

  ###Creating charts for each team ----

  purrr::map(seasons, function(x) {

    season_data <- bp[bp$Season == x, ]      # store team data in a new variable

    season_data  |>
      # Scatter plot for the two metrics: x axis are batting and y axis pitching
      highcharter::hchart(showInLegend = FALSE,
                          type = "scatter",
                          highcharter::hcaes(x = wRC_plus,
                                             y = ERA_adjusted,
                                             color = primary),
                          marker = list(symbol = "circle",
                                        radius = 7,
                                        lineWidth = 2),
                          Opacity = 0.75,
                          name = "Team",
                          zIndex = 10) |>
    highcharter::hc_tooltip(useHTML = TRUE,
                              headerFormat = "",
                              pointFormat = "<b>Season:</b> {point.Season} <br>
                                            <b>Team:</b> {point.Team} <br>
                                            <b>wRC+:</b> {point.wRC_plus} <br>
                                            <b>ERA-:</b> {point.ERA_adjusted} <br>
                                            <b>W-L%:</b> {point.WLpct}",
                              borderWidth = 1,
                              borderColor = "#000000")  |>
    highcharter::hc_add_theme(highcharter::hc_theme_smpl())  |>
    # X axis definition
      highcharter::hc_xAxis(plotLines = list(list(value = 100, color = "red", width = 2,
                                                  dashStyle = "shortdash")),
                            zIndex = 2,
                            min = 0.95*min(bp$wRC_plus),
                            max = 1.05*max(bp$wRC_plus))  |>
      # Y axis definition
      highcharter::hc_yAxis(plotLines = list(list(value = 100, color = "red", width = 2,
                                                  dashStyle = "shortdash")),
                            min = min(bp$ERA_adjusted),
                            max = max(bp$ERA_adjusted)) |>
      highcharter::hc_title(text = paste0("<span style=\"background-color:#002d73\">MLB Teams wRC+ vs. ERA-</span>"))  |>
      highcharter::hc_subtitle(text = paste0(x, " Season")) |>
      # # adding credits and date when the chart was build
      highcharter::hc_credits(enabled = TRUE,
                              text = paste0("Source: Baseball Reference. Using 'baseballr' R package. Retrieved on: ",
                                            lubridate::with_tz(Sys.time(), "US/Eastern")  |>
                                              format("%Y-%m-%d %H:%M %Z")))  |>
      # enable exporting option
      highcharter::hc_exporting(enabled = TRUE)

  }) |>
    highcharter::hw_grid(rowheight = 400, ncol = viz_col) |>
    htmltools::browsable()

## ggplot version of the chart for 1 season

 #  plots <- purrr::map(seasons, function(x) {
 #
 #    season_data <- bp[bp$Season == x,]      # store team data in a new variable
 #    # max_diff <- max(team_data$cum_RD)   # calculate max cum_RD for the team
 #    # min_diff <- min(team_data$cum_RD)   # calculate min cum_RD for the team
 #
 #    season_data  |>
 #      ggplot2::ggplot(ggplot2::aes(x = wRC_plus,
 #                                   y = ERA_adjusted)) +
 #    ggplot2::geom_vline(xintercept = 100, linetype = 1, linewidth = 1, color = "darkblue") +
 #    ggplot2::geom_hline(yintercept = 100, linetype = 1, linewidth = 1, color = "darkblue") +
 #    ggplot2::geom_point(#ggplot2::aes(size = WLpct),
 #               fill = season_data$primary,
 #               color = season_data$secondary,
 #               stroke = 2,
 #               shape = 21,
 #               alpha = 0.95) +
 #      ggplot2::xlim(0.95*min(bp$wRC_plus), 1.05*max(bp$wRC_plus)) +
 #      ggplot2::ylim(0.95*min(bp$ERA_adjusted), 1.05*max(bp$ERA_adjusted)) +
 #    ggplot2::scale_size(range = c(4, 12), guide = "none") +  # Adjust the range of point sizes
 #    #scale_fill_teams(guide = FALSE) +
 #    #scale_color_teams(2, guide = FALSE) +
 #    ggrepel::geom_text_repel(ggplot2::aes(label = Team),
 #                             box.padding = 0.6,
 #                             max.overlaps = 50) +
 #    ggplot2::geom_text(
 #      label = "Bad batting & Good pitching",
 #      x = mean(range(season_data$wRC_plus[season_data$wRC_plus <= 100])),
 #      y = mean(range(season_data$ERA_adjusted[season_data$ERA_adjusted <= 100])),
 #      hjust = 0.5,
 #      vjust = 0.5,
 #      color = "black",
 #      fill = "lightyellow"
 #    ) +
 #    ggplot2::geom_text(
 #      label = "Bad batting & pitching",
 #      x = mean(range(season_data$wRC_plus[season_data$wRC_plus <= 100])),
 #      y = mean(c(max(season_data$ERA_adjusted), 100)),
 #      hjust = 0.5,
 #      vjust = 0.5,
 #      color = "black",
 #      fill = "darkred"
 #    ) +
 #    ggplot2::geom_text(
 #      label = "Good batting & Bad pitching",
 #      x = mean(range(season_data$wRC_plus[season_data$wRC_plus > 100])),
 #      y = mean(c(max(season_data$ERA_adjusted), 100)),
 #      hjust = 0.5,
 #      vjust = 0.5,
 #      color = "black",
 #      fill = "lightyellow"
 #    ) +
 #    ggplot2::geom_text(
 #      label = "Good batting & pitching",
 #      x = mean(range(season_data$wRC_plus[season_data$wRC_plus > 100])),
 #      y = mean(range(season_data$ERA_adjusted[season_data$ERA_adjusted <= 100])),
 #      hjust = 0.5,
 #      vjust = 0.5,
 #      color = "black",
 #      fill = "darkgreen"
 #      ) +
 #    ggplot2::theme_light() +
 #    ggplot2::labs(
 #      title = "MLB Teams wRC+ vs. ERA-",
 #      subtitle = paste0(season_data$Season[1], " Season"),
 #      caption = paste0("Source: Baseball Reference. Using 'baseballr' R package. Retrieved on: ",
 #                       lubridate::with_tz(Sys.time(), "US/Eastern")))
 #
 # } )
 #
 #  multiplot <- cowplot::plot_grid(plotlist = plots, ncol = 2)
 #
 #  # Print the multiplot
 #  multiplot

}

