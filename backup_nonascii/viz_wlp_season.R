#' Scrapes MLB Standings in one specific season and visualizes a timeline of the winning percentage for any division or a league
#'
#' This function allows you to scrape the standings from MLB teams for a specific season, and visualize one chart of Winning percentage of teams during that season.
#' @param lg_div a string input, Baseball Reference Team abbreviation or a division (e.g. NL East, NL Central, NL West, NL Overall, AL East, AL Central, AL West, AL Overall or MLB)
#' @param year a numeric value, MLB season to be analyzed
#' @keywords MLB, standings
#' @importFrom highcharter highchart hchart hc_title hc_subtitle hc_credits hc_yAxis hc_xAxis hc_add_theme hcaes hc_theme_smpl hc_add_series hc_tooltip hc_exporting
#' @importFrom pbapply pblapply
#' @importFrom tidyr separate unite
#' @importFrom lubridate year
#' @importFrom baseballr bref_standings_on_date bref_team_results
#' @importFrom dplyr select bind_rows mutate rename arrange desc filter pull left_join
#' @importFrom purrr map
#' @import ggplot2
#' @importFrom mlbplotR geom_mlb_logos
#' @export viz_wlp_season

viz_wlp_season <- function(lg_div, year, type = "ggplot") {

  standings <- get_standings_season(lg_div, year)

  # Identifying leaders

  leaders <- standings |>
    dplyr::filter(Date == max(Date), Rank == 1) |>
    dplyr::select(Team)

  # Create a table of colors by team

  Teams_Lahman <- read.csv("data/Teams.csv")[ , -1]

  team_palette <- Teams_Lahman  |>
    dplyr::filter(yearID == 2021)  |>
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
  team_palette[ , 3] <- NULL # Remove "secondary" column


  # Join "primary" color column to standings data.frame
  standings <- standings |>
    dplyr::left_join(team_palette, by = "Team")

  unique_teams <- unique(standings$Team)

  #### Plot W% timeline

  ### In the case the chart is using highcharter

  if (type == "highcharter") {

    wl <- highcharter::highchart()

    # Add series of GB for each team in the data frame
    for (i in 1:length(unique_teams)) {

      if (lg_div == "AL Overall" | lg_div == "NL Overall" | lg_div == "MLB") {

        if (unique_teams[i] %in% leaders) {

          # Creates series for teams leading their divisions (When requesting Overall)
          wl <- wl |>
            highcharter::hc_add_series(data = standings[standings$Team == unique_teams[i], ],
                                       highcharter::hcaes(x = Date, y = Wpct),
                                       name = unique_teams[i],
                                       color = team_palette[team_palette$Team == unique_teams[i], 2],
                                       type = "spline",
                                       lineWidth = 4)

        } else {

          # Creates series for remaining teams (When requesting Overall)
          wl <- wl |>
            highcharter::hc_add_series(data = standings[standings$Team == unique_teams[i], ],
                                       highcharter::hcaes(x = Date, y = Wpct),
                                       name = unique_teams[i],
                                       color = team_palette[team_palette$Team == unique_teams[i], 2],
                                       type = "spline",
                                       dashStyle = "ShortDashDotDot",
                                       lineWidth = 2)
        }

      } else {
        # Creates plot for requested division
        wl <- wl |>
          highcharter::hc_add_series(data = standings[standings$Team == unique_teams[i], ],
                                     highcharter::hcaes(x = Date, y = Wpct),
                                     name = unique_teams[i],
                                     color = team_palette[team_palette$Team == unique_teams[i], 2],
                                     type = "spline",
                                     lineWidth = 3)
      }

    }

    # Customize the W% plot
    wl <- wl |>
      highcharter::hc_xAxis(title = list(text = "Date"),
                            type = "datetime") |> # X axis definition
      highcharter::hc_yAxis(title = list(text = "W%"),
                            max = 1,
                            valueDecimals = 3) |> # Y axis definition
      highcharter::hc_title(text =
                              if (lg_div == "MLB")  {
                                paste0("<span style=\"color:#002d73\"> MLB Teams - </span>",
                                       lubridate::year(standings$Date[1]),
                                       " - W% after games on ", max(standings$Date))
                              } else {
                                paste0("<span style=\"color:#002d73\"> MLB - </span>",
                                       lubridate::year(standings$Date[1]),
                                       lg_div,
                                       " - W% after games on ", max(standings$Date))
                              }
      ) |>
      highcharter::hc_subtitle(text =  "Solid line(s) represent the Division(s) leader(s)") |>
      highcharter::hc_credits(enabled = TRUE,
                              text = paste0("Source: Baseball Reference. Using 'baseballr' R package. Retreived on: ",
                                            lubridate::with_tz(Sys.time(), "US/Eastern")  |>
                                              format("%Y-%m-%d %H:%M %Z")))  |>
      highcharter::hc_add_theme(highcharter::hc_theme_smpl()) |>
      highcharter::hc_tooltip(valueDecimals = 3,
                              borderWidth = 1,
                              table = TRUE, # adds color to team names in the tooltip
                              sort = TRUE, # Future feature to show all the teams in the tooltip
                              shared = TRUE, # Future feature to show all the teams in the tooltip
                              borderColor = "#000000") |> # tooltip border color
      highcharter::hc_exporting(enabled = TRUE) # enable exporting option


    # print viz of Winning percentage in a season
    print(wl)

  }

  if (type == "ggplot") {

    # Ensure mlbplotR is available
    if (!requireNamespace("mlbplotR", quietly = TRUE)) {
      stop("The package 'mlbplotR' is required to use this plot type. Please install it with install.packages('mlbplotR')")
    }

    standings_plot <- standings |>
      dplyr::filter(!is.na(Date)) |>
      dplyr::mutate(
        leader = ifelse(Team %in% leaders$Team, TRUE, FALSE),
        linewidth = ifelse(leader, 1.5, 0.6),
        linetype = ifelse(leader, "solid", "dashed"),
        alpha = ifelse(leader, 0.9, 0.65),
        Date = as.Date(Date)
      )

    # Get last game per team
    max_date <- max(standings_plot$Date, na.rm = TRUE)
    # Add to `last_point` if not already done
    last_point <- standings_plot |>
      dplyr::group_by(Team) |>
      dplyr::filter(Date == max(Date)) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        Date = Date + 2, # shifts logos right by 2 day
        alpha = ifelse(leader, 1, 0.6),  # make leaders fully opaque
        width = ifelse(leader, 0.015, 0.01)  # make all logos smaller
      )

    # Define a mapping vector
    abbreviation_map <- c( "ATH" = "OAK",
                           "CHW" = "CWS",
                           "KCR" = "KC",
                           "TBR" = "TB",
                           "SDP" = "SD",
                           "SFG" = "SF",
                           "ARI" = "AZ",
                           "WSN" = "WSH")

    # Recode abbreviations in all datasets
    team_palette$Team <- dplyr::recode(team_palette$Team, !!!abbreviation_map)
    standings_plot$Team <- dplyr::recode(standings$Team, !!!abbreviation_map)
    last_point$Team <- dplyr::recode(last_point$Team, !!!abbreviation_map)

    # Filter last_point before plotting
    valid_teams <- mlbplotR::valid_team_names()

    missing_teams <- setdiff(last_point$Team, valid_teams)
    if (length(missing_teams) > 0) {
      warning("⚠️ Some teams have no logos available: ", paste(missing_teams, collapse = ", "))}

    last_point <- last_point |> dplyr::filter(Team %in% valid_teams)

    # Create leaderboard table (logos + W% at top-right)
    table_data <- last_point |>
      dplyr::select(Team, Wpct, leader) |>
      dplyr::filter(Team %in% valid_teams) |>
      dplyr::arrange(desc(Wpct)) |>
      dplyr::mutate(
        rank = dplyr::row_number(),
        n = dplyr::n(),
        spacing = min(0.045, 0.9 / n),  # adjust spacing if too many teams
        x = max(standings_plot$Date) + 2,
        y = 0.5 + spacing * ((n - 1) / 2 - (rank - 1)),  # centered layout
        label = sprintf("%.3f", Wpct),
        alpha = ifelse(Team %in% leaders$Team, 1, 0.5), # 🔍 Set alpha based on leader status
        text_color = ifelse(Wpct >= 0.5, "darkgreen", "firebrick"),
        fontface = ifelse(leader, "bold", "plain")
        )


    # Create ggplot
    # Use team-specific colors
    p <- ggplot(standings_plot, aes(x = Date,
                                    y = Wpct,
                                    group = Team)) +
      geom_line(aes(linewidth = linewidth,
                    alpha = alpha,
                    color = team_palette$primary[match(Team, team_palette$Team)]),
                show.legend = FALSE) +
      scale_color_identity() +
      scale_linewidth_identity() +  # use fixed size values from data
      scale_linetype_identity() +   # use "solid"/"dashed" from data
      scale_alpha_identity() +      # use 1/0.75 from data
      scale_y_continuous(limits = c(0, 1), name = "W%") +
      scale_x_date(name = "Date") +
      mlbplotR::geom_mlb_scoreboard_logos(data = table_data,
                                          aes(x = x,
                                              y = y,
                                              team_abbr = Team,
                                              alpha = alpha),
                                          width = 0.02,
                                          inherit.aes = FALSE) +
      geom_text(data = table_data,
                aes(x = x + 2, y = y,
                    label = label,
                    color = text_color,
                    fontface = fontface),
                hjust = 0,
                size = 3,
                inherit.aes = FALSE) +
      # scale_color_identity() +
      labs(title = if (lg_div == "MLB") {
          paste0("MLB Teams - ", lubridate::year(min(standings_plot$Date)), " - W%, after games on ", max(standings_plot$Date))
        } else {
          paste0("MLB - ", lubridate::year(min(standings_plot$Date)), " ", lg_div, " - W%, after games on ", max(standings_plot$Date))
        },
        subtitle = "Thicker lines represent the Division(s) leader(s)",
        caption = paste0("Source: Baseball Reference via baseballr | Retrieved on: ",
                         format(lubridate::with_tz(Sys.time(), "US/Eastern"), "%Y-%m-%d %H:%M %Z"))
      ) +
      theme_minimal(base_size = 13)

    print(p)
  }

}
