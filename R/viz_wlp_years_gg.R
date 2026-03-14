#' Visualize MLB Winning Percentage by Season (ggplot)
#'
#' Produces the same winning-percentage timelines as `viz_wlp_years()` but
#' rendered with `ggplot2`, adding official team logos from `mlbplotR` in the
#' top-right corner of each panel.
#'
#' @inheritParams viz_wlp_years
#'
#' @return A `ggplot` object created with `ggpubr::ggarrange()` that combines
#'   one panel per requested team or franchise.
#'
#' @export
viz_wlp_years_gg <- function(start_season, end_season, fran_tm = "franchise") {
  if (!requireNamespace("mlbplotR", quietly = TRUE)) {
    stop(
      "The package 'mlbplotR' is required to use this plot type. ",
      "Please install it with install.packages('mlbplotR')."
    )
  }

  valid_type <- c("franchise", "team")
  if (!(fran_tm %in% valid_type)) {
    stop(
      "The 'fran_tm' argument must be any of the following possibilities: ",
      "franchise or team"
    )
  }

  wl <- get_standings_years(start_season, end_season) |>
    dplyr::select(Season = Year, TeamName = Team, W, L, WLpct = Wpct)

  wl <- wl |>
    dplyr::group_by(TeamName) |>
    dplyr::arrange(Season) |>
    dplyr::ungroup()

  Teams_Lahman <- read.csv("data/Teams.csv")[, -1]
  Teams_Franchises <- read.csv("data/TeamsFranchises.csv")[, -1]

  teams_meta <- Teams_Lahman |>
    dplyr::left_join(Teams_Franchises, by = "franchID") |>
    dplyr::select(
      yearID,
      teamIDBR,
      name,
      franchID,
      franchName,
      active,
      lgID,
      divID,
      DivWin,
      WCWin,
      LgWin,
      WSWin
    ) |>
    dplyr::filter(active == "Y", lgID == "AL" | lgID == "NL")

  teams_meta$name[teams_meta$teamIDBR == "LAA"] <- "Los Angeles Angels"

  teamcolors <- read.csv("data/teamcolors.csv")[, -1] |>
    dplyr::filter(league == "mlb")

  swap_indices <- c(1, 2, 5, 11, 16, 17, 21, 25, 27, 28)
  temp <- teamcolors$primary[swap_indices]
  teamcolors$primary[swap_indices] <- teamcolors$secondary[swap_indices]
  teamcolors$secondary[swap_indices] <- temp

  team_count <- length(unique(wl$TeamName))
  viz_col <- dplyr::case_when(
    team_count <= 1 ~ 1,
    team_count == 2 ~ 2,
    team_count <= 6 ~ 3,
    team_count <= 12 ~ 4,
    team_count <= 24 ~ 5,
    TRUE ~ 6
  )

  teams_meta <- teams_meta |>
    dplyr::left_join(teamcolors, by = "name") |>
    dplyr::rename(
      Season = yearID,
      TeamAbbr = teamIDBR,
      TeamName = name,
      Franchise = franchName
    ) |>
    dplyr::select(
      Season,
      TeamAbbr,
      TeamName,
      franchID,
      Franchise,
      active,
      lgID,
      divID,
      DivWin,
      WCWin,
      LgWin,
      WSWin,
      primary,
      secondary
    )

  teams_meta$primary[teams_meta$TeamName == "Brooklyn Dodgers"] <- "#082984"
  teams_meta$secondary[teams_meta$TeamName == "Brooklyn Dodgers"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "Boston Braves"] <- "#C8102E"
  teams_meta$secondary[teams_meta$TeamName == "Boston Braves"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "New York Giants"] <- "#FF3E00"
  teams_meta$secondary[teams_meta$TeamName == "New York Giants"] <- "#000000"
  teams_meta$primary[teams_meta$TeamName == "Philadelphia Athletics"] <- "#150360"
  teams_meta$secondary[teams_meta$TeamName == "Philadelphia Athletics"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "Kansas City Athletics"] <- "#00843D"
  teams_meta$secondary[teams_meta$TeamName == "Kansas City Athletics"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "California Angels"] <- "#001E40"
  teams_meta$secondary[teams_meta$TeamName == "California Angels"] <- "#C1033B"
  teams_meta$primary[teams_meta$TeamName == "Tampa Bay Devil Rays"] <- "#02B189"
  teams_meta$secondary[teams_meta$TeamName == "Tampa Bay Devil Rays"] <- "#D2BC50"
  teams_meta$primary[
    teams_meta$TeamName == "Los Angeles Angels of Anaheim" |
      teams_meta$TeamName == "Anaheim Angels"
  ] <- "#BA0021"
  teams_meta$secondary[
    teams_meta$TeamName == "Los Angeles Angels of Anaheim" |
      teams_meta$TeamName == "Anaheim Angels"
  ] <- "#003263"
  teams_meta$primary[teams_meta$TeamName == "Cleveland Guardians"] <- "#00385D"
  teams_meta$secondary[teams_meta$TeamName == "Cleveland Guardians"] <- "#E50022"
  teams_meta$primary[teams_meta$TeamName == "Florida Marlins"] <- "#00A3B3"
  teams_meta$secondary[teams_meta$TeamName == "Florida Marlins"] <- "#000000"
  teams_meta$primary[teams_meta$TeamName == "Montreal Expos"] <- "#67ABE5"
  teams_meta$secondary[teams_meta$TeamName == "Montreal Expos"] <- "#E4002B"
  teams_meta$primary[is.na(teams_meta$primary)] <- "#000000"
  teams_meta$secondary[is.na(teams_meta$secondary)] <- "#E6E3E3"

  team_palette <- teams_meta |>
    dplyr::arrange(dplyr::desc(Season)) |>
    dplyr::distinct(TeamName, .keep_all = TRUE) |>
    dplyr::select(TeamName, primary, secondary)

  wl <- wl |>
    dplyr::left_join(teams_meta, by = c("TeamName", "Season")) |>
    dplyr::group_by(TeamName) |>
    dplyr::arrange(Season, .by_group = TRUE) |>
    tidyr::fill(
      TeamAbbr,
      TeamName,
      franchID,
      Franchise,
      active,
      lgID,
      divID,
      DivWin,
      WCWin,
      LgWin,
      WSWin,
      primary,
      secondary,
      .direction = "downup"
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(Team = dplyr::coalesce(TeamAbbr, TeamName)) |>
    dplyr::mutate(
      ended = dplyr::case_when(
        WSWin == "Y" ~ "WS Champs",
        LgWin == "Y" ~ paste0(lgID, " Champs"),
        DivWin == "Y" ~ paste0("Won ", lgID, " ", divID),
        WCWin == "Y" ~ "Wild Card",
        TRUE ~ "Eliminated"
      )
    )

  wl <- wl |>
    dplyr::left_join(team_palette, by = "TeamName", suffix = c("", ".palette")) |>
    dplyr::mutate(
      primary = dplyr::coalesce(primary, primary.palette),
      secondary = dplyr::coalesce(secondary, secondary.palette)
    ) |>
    dplyr::select(-primary.palette, -secondary.palette)

  team_labels <- wl |>
    dplyr::group_by(franchID) |>
    dplyr::arrange(Season, .by_group = TRUE) |>
    dplyr::summarise(
      TeamLabel = {
        abbrs <- TeamAbbr[!is.na(TeamAbbr)]
        if (length(abbrs)) abbrs[[1]] else NA_character_
      },
      TeamLabelName = {
        names <- TeamName[!is.na(TeamName)]
        if (length(names)) names[[1]] else NA_character_
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(TeamLabel = dplyr::coalesce(TeamLabel, TeamLabelName)) |>
    dplyr::select(franchID, TeamLabel)

  wl <- wl |>
    dplyr::left_join(team_labels, by = "franchID") |>
    dplyr::mutate(
      TeamAbbr = dplyr::case_when(
        !is.na(TeamLabel) ~ TeamLabel,
        TRUE ~ TeamAbbr
      ),
      Team = dplyr::coalesce(TeamAbbr, Team)
    ) |>
    dplyr::select(-TeamLabel)

  recent_season_rows <- wl |>
    dplyr::filter(Season == max(Season))

  if (any(is.na(recent_season_rows$TeamName))) {
    temp_df <- wl |>
      dplyr::filter(Season == (max(wl$Season) - 1)) |>
      dplyr::mutate(Season = max(wl$Season))

    wl <- wl |>
      dplyr::left_join(
        temp_df,
        by = c("Team", "Season"),
        suffix = c("", ".y")
      ) |>
      dplyr::mutate(
        TeamName = ifelse(is.na(TeamName), TeamName.y, TeamName),
        franchID = ifelse(is.na(franchID), franchID.y, franchID),
        Franchise = ifelse(is.na(Franchise), Franchise.y, Franchise),
        active = ifelse(is.na(active), active.y, active),
        lgID = ifelse(is.na(lgID), lgID.y, lgID),
        divID = ifelse(is.na(divID), divID.y, divID),
        primary = ifelse(is.na(primary), primary.y, primary),
        secondary = ifelse(is.na(secondary), secondary.y, secondary)
      ) |>
      dplyr::select(-ends_with(".y"))
  }

  if (fran_tm == "team") {
    teams_factor <- wl |>
      dplyr::group_by(Team) |>
      dplyr::summarise(
        W = sum(W, na.rm = TRUE),
        L = sum(L, na.rm = TRUE)
      ) |>
      dplyr::mutate(Wp_global = round(W / (W + L), 3)) |>
      dplyr::arrange(dplyr::desc(Wp_global)) |>
      dplyr::filter(!is.na(Team))
  } else {
    teams_factor <- wl |>
      dplyr::group_by(franchID) |>
      dplyr::summarise(
        W = sum(W, na.rm = TRUE),
        L = sum(L, na.rm = TRUE)
      ) |>
      dplyr::mutate(Wp_global = round(W / (W + L), 3)) |>
      dplyr::arrange(dplyr::desc(Wp_global)) |>
      dplyr::filter(!is.na(franchID))
  }

  message(
    "The teams/franchises are sorted in descending order by their W% in the whole period, as shown here below (defining their order in the chart)"
  )
  teams_factor |>
    print(n = nrow(teams_factor))

  wl_period <- teams_factor

  if (fran_tm == "team") {
    wl <- wl |>
      dplyr::left_join(teams_factor[, c(1, 4)], by = "Team")
  } else {
    wl <- wl |>
      dplyr::left_join(teams_factor[, c(1, 4)], by = "franchID")
  }

  teams_factor <- teams_factor |>
    dplyr::select(1) |>
    unlist(use.names = FALSE)

  global_x_min <- min(wl$Season, na.rm = TRUE)
  global_x_max <- max(wl$Season, na.rm = TRUE)
  global_y_min <- min(wl$WLpct, na.rm = TRUE)
  global_y_max <- max(wl$WLpct, na.rm = TRUE)

  x_span <- max(1, global_x_max - global_x_min)
  y_span <- max(0.05, global_y_max - global_y_min)

  plot_x_max <- global_x_max + x_span * 0.15
  plot_y_max <- global_y_max + y_span * 0.22

  logo_anchor_x <- global_x_max + x_span * 0.075
  logo_anchor_y <- global_y_max + y_span * 0.13
  logo_width_units <- x_span * 0.06
  logo_height_units <- y_span * 0.16

  postseason_levels <- c("Wild Card", "Won Div", "League Champs", "WS Champs")
  postseason_colors <- c(
    "Wild Card" = "white",
    "Won Div" = "lightblue",
    "League Champs" = "darkblue",
    "WS Champs" = "gold"
  )

  source_stamp <- lubridate::with_tz(Sys.time(), "US/Eastern") |>
    format("%Y-%m-%d %H:%M %Z")

  global_caption <- paste0(
    "Source: Fangraphs. Using 'bbgraphsR'. Retrieved on: ",
    source_stamp
  )

  missing_logos <- character(0)

  valid_logo_codes <- mlbplotR::valid_team_names()
  alias_abbr <- c(
    ANA = "LAA",
    CAL = "LAA",
    FLA = "MIA",
    FLO = "MIA",
    MON = "WSH",
    WSN = "WSH",
    WS1 = "MIN",
    WS2 = "TEX",
    TBD = "TB",
    TBA = "TB",
    KCA = "ATH",
    KCF = "ATH",
    PHA = "ATH",
    PHQ = "ATH",
    BRO = "LAD",
    BSN = "ATL",
    MLN = "ATL",
    MLA = "ATL",
    NYG = "SF",
    NY1 = "SF",
    NY2 = "SF",
    SLA = "BAL",
    SLB = "BAL",
    SE1 = "MIL"
  )
  alias_name <- c(
    "Montreal Expos" = "WSH",
    "Florida Marlins" = "MIA",
    "Tampa Bay Devil Rays" = "TB",
    "California Angels" = "LAA",
    "Anaheim Angels" = "LAA",
    "Los Angeles Angels of Anaheim" = "LAA",
    "Brooklyn Dodgers" = "LAD",
    "Boston Braves" = "ATL",
    "Milwaukee Braves" = "ATL",
    "Kansas City Athletics" = "ATH",
    "Philadelphia Athletics" = "ATH",
    "Washington Senators" = "MIN",
    "Washington Senators (1961)" = "TEX"
  )
  alias_franchise <- c(
    ANA = "LAA",
    CAL = "LAA",
    FLA = "MIA",
    FLO = "MIA",
    MON = "WSH",
    WSN = "WSH",
    TBD = "TB",
    TBA = "TB",
    PHA = "ATH",
    KCA = "ATH",
    KCF = "ATH",
    BRO = "LAD",
    BSN = "ATL",
    MLN = "ATL",
    MLA = "ATL",
    NYG = "SF",
    NY1 = "SF",
    SLA = "BAL",
    SLB = "BAL",
    SE1 = "MIL",
    WS1 = "MIN",
    WS2 = "TEX"
  )

  resolve_logo_code <- function(clean_abbr, team_name, franchise, original_abbr = clean_abbr) {
    lookup_alias <- function(code) {
      if (is.na(code) || !nzchar(code)) {
        return(NA_character_)
      }
      if (code %in% names(alias_abbr)) {
        return(alias_abbr[[code]])
      }
      code
    }

    candidate <- lookup_alias(clean_abbr)
    if (!is.na(candidate) && candidate %in% valid_logo_codes) {
      return(candidate)
    }

    raw_candidate <- lookup_alias(original_abbr)
    if (!is.na(raw_candidate) && raw_candidate %in% valid_logo_codes) {
      return(raw_candidate)
    }

    if (!is.na(franchise) && franchise %in% names(alias_franchise)) {
      alt <- alias_franchise[[franchise]]
      if (alt %in% valid_logo_codes) {
        return(alt)
      }
    }

    if (!is.na(team_name) && team_name %in% names(alias_name)) {
      alt <- alias_name[[team_name]]
      if (alt %in% valid_logo_codes) {
        return(alt)
      }
    }

    if (!is.na(clean_abbr) && clean_abbr %in% valid_logo_codes) {
      return(clean_abbr)
    }
    if (!is.na(original_abbr) && original_abbr %in% valid_logo_codes) {
      return(original_abbr)
    }

    NA_character_
  }

  plot_list <- purrr::map(teams_factor, function(x) {
    if (fran_tm == "team") {
      teams_data <- wl[wl$Team == x, ]
      wl_whole <- wl_period[wl_period$Team == x, ]
    } else {
      teams_data <- wl[wl$franchID == x, ]
      wl_whole <- wl_period[wl_period$franchID == x, ]
    }

    if (!nrow(wl_whole) || nrow(teams_data) == 0) {
      return(NULL)
    }

    teams_data <- teams_data |>
      dplyr::arrange(Season) |>
      dplyr::distinct(Season, .keep_all = TRUE)

    total_w <- dplyr::coalesce(wl_whole$W, 0)
    total_l <- dplyr::coalesce(wl_whole$L, 0)

    period_wp <- ifelse((total_w + total_l) > 0,
      round(total_w / (total_w + total_l), 3),
      NA_real_
    )
    period_wp_label <- ifelse(is.na(period_wp), "NA", sprintf("%.3f", period_wp))

    line_color <- teams_data[["primary"]]
    line_color <- line_color[!is.na(line_color) & nzchar(line_color)]
    if (!length(line_color)) {
      line_color <- "#000000"
    } else {
      line_color <- line_color[[1]]
      rgb_primary <- tryCatch(grDevices::col2rgb(line_color), error = function(...) NA)
      if (is.matrix(rgb_primary) && all(rgb_primary > 230)) {
        alt_color <- teams_data[["secondary"]]
        alt_color <- alt_color[!is.na(alt_color) & nzchar(alt_color)]
        if (length(alt_color)) {
          line_color <- alt_color[[1]]
        }
      }
    }

    postseason_points <- dplyr::bind_rows(
      teams_data |>
        dplyr::filter(ended == "Wild Card", !is.na(WLpct)) |>
        dplyr::mutate(type = "Wild Card"),
      teams_data |>
        dplyr::filter(grepl("^Won", ended), !is.na(WLpct)) |>
        dplyr::mutate(type = "Won Div"),
      teams_data |>
        dplyr::filter(grepl("Champs$", ended) & grepl("^[ALN]", ended), !is.na(WLpct)) |>
        dplyr::mutate(type = "League Champs"),
      teams_data |>
        dplyr::filter(ended == "WS Champs", !is.na(WLpct)) |>
        dplyr::mutate(type = "WS Champs")
    ) |>
      dplyr::mutate(type = factor(type, levels = postseason_levels))

    display_label <- if (fran_tm == "team") {
      x
    } else {
      labels <- teams_data$Franchise[!is.na(teams_data$Franchise)]
      if (length(labels)) labels[[1]] else x
    }

    logo_candidate <- teams_data$TeamAbbr[!is.na(teams_data$TeamAbbr)]
    if (!length(logo_candidate)) {
      logo_candidate <- teams_data$Team[!is.na(teams_data$Team)]
    }
    logo_candidate <- if (length(logo_candidate)) logo_candidate[[1]] else NA_character_
    logo_name_candidate <- teams_data$TeamName[!is.na(teams_data$TeamName)]
    logo_name_candidate <- if (length(logo_name_candidate)) {
      logo_name_candidate[[1]]
    } else {
      NA_character_
    }

    franchise_candidate <- teams_data$franchID[!is.na(teams_data$franchID)]
    franchise_candidate <- if (length(franchise_candidate)) {
      franchise_candidate[[1]]
    } else {
      NA_character_
    }

    clean_logo_abbr <- if (!is.na(logo_candidate)) {
      mlbplotR::clean_team_abbrs(logo_candidate, keep_non_matches = TRUE)
    } else {
      NA_character_
    }

    resolved_logo <- resolve_logo_code(
      clean_abbr = clean_logo_abbr,
      team_name = logo_name_candidate,
      franchise = franchise_candidate,
      original_abbr = logo_candidate
    )

    if (is.na(resolved_logo)) {
      missing_logos <<- unique(c(missing_logos, display_label))
    }

    logo_data <- if (!is.na(resolved_logo)) {
      tibble::tibble(
        Season = logo_anchor_x,
        WLpct = logo_anchor_y,
        team_abbr = resolved_logo
      )
    } else {
      NULL
    }

    subtitle_text <- sprintf(
      "W%% (%s-%s): %s (%s-%s)",
      global_x_min,
      global_x_max,
      period_wp_label,
      total_w,
      total_l
    )

    p <- ggplot2::ggplot(teams_data, ggplot2::aes(x = Season, y = WLpct)) +
      ggplot2::geom_line(color = line_color, linewidth = 1.8, alpha = 1) +
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue", linewidth = 0.4)

    if (nrow(postseason_points)) {
      p <- p +
        ggplot2::geom_point(
          data = postseason_points,
          ggplot2::aes(x = Season, y = WLpct, shape = type, fill = type),
          color = "black",
          size = 3,
          stroke = 0.8,
          inherit.aes = FALSE
        )
    }

    p <- p +
      ggplot2::scale_shape_manual(
        name = "Postseason",
        values = c(
          "Wild Card" = 21,
          "Won Div" = 22,
          "League Champs" = 24,
          "WS Champs" = 23
        ),
        drop = FALSE,
        guide = ggplot2::guide_legend(
          title.position = "top",
          nrow = 1,
          override.aes = list(
            shape = c(21, 22, 24, 23),
            fill = postseason_colors[postseason_levels]
          )
        )
      ) +
      ggplot2::scale_fill_manual(
        values = postseason_colors,
        drop = FALSE,
        guide = "none"
      )

    if (!nrow(postseason_points)) {
      p <- p + ggplot2::guides(shape = "none", fill = "none")
    }

    if (!is.null(logo_data)) {
      p <- p +
        mlbplotR::geom_mlb_logos(
          data = logo_data,
          ggplot2::aes(x = Season, y = WLpct, team_abbr = team_abbr),
          width = logo_width_units,
          height = logo_height_units,
          inherit.aes = FALSE
        )
    }

    p <- p +
      ggplot2::scale_x_continuous(
        limits = c(global_x_min, plot_x_max),
        breaks = pretty(c(global_x_min, global_x_max)),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        limits = c(global_y_min, plot_y_max),
        breaks = seq(
          floor(global_y_min * 10) / 10,
          ceiling(global_y_max * 10) / 10,
          by = 0.1
        ),
        expand = c(0, 0)
      ) +
      ggplot2::labs(
        title = NULL,
        subtitle = subtitle_text,
        x = "Season",
        y = "W%"
      ) +
      theme_bbgraphs(font_size = 10) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 9),
        legend.text = ggplot2::element_text(size = 8),
        plot.title = ggplot2::element_blank(),
        plot.subtitle = ggplot2::element_text(size = 8, hjust = 0.5),
        axis.title = ggplot2::element_text(size = 9),
        axis.text = ggplot2::element_text(size = 8),
        panel.grid.major.x = ggplot2::element_line(colour = "#d7dce3"),
        panel.grid.major.y = ggplot2::element_line(colour = "#d7dce3"),
        panel.grid.minor = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(6, 6, 6, 6)
      )

    p
  }) |>
    purrr::compact()

  if (!length(plot_list)) {
    warning("No data available to plot with the supplied parameters.")
    return(NULL)
  }

  if (length(missing_logos)) {
    warning(
      "Logos were not available for: ",
      paste(sort(unique(missing_logos)), collapse = ", ")
    )
  }

  n_rows <- ceiling(length(plot_list) / viz_col)

  arranged <- ggpubr::ggarrange(
    plotlist = plot_list,
    ncol = viz_col,
    nrow = n_rows,
    common.legend = TRUE,
    legend = "bottom"
  )

  ggpubr::annotate_figure(
    arranged,
    bottom = ggpubr::text_grob(
      global_caption,
      size = 9,
      color = "#3a3a3a"
    )
  )
}
