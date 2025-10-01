#' Plot MLB career spans by player birthplace state
#'
#' Takes a player metadata data frame (e.g. the output of
#' `get_players_by_country()`) and keeps the players with the longest careers
#' (measured by total seasons played). Those careers are visualised in a
#' Datawrapper-style timeline chart, with each player's segment coloured by the
#' birthplace state.
#'
#' @param metadata_df A data frame with at least `Name`, `From`, `To`, `Yrs`,
#'   `State`, and `PlayerID` columns.
#' @param top_n Integer giving how many of the longest-tenured players to show.
#'   Defaults to 30.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#'   players <- get_players_by_country("Dominican Republic")
#'   viz_years_country(players, top_n = 25)
#' }
#'
#' @importFrom dplyr arrange case_when desc filter group_by if_else left_join mutate
#'   rename select slice_head slice_tail ungroup
#' @importFrom ggplot2 aes expansion geom_point geom_segment geom_text ggplot guides
#'   labs scale_colour_manual scale_x_continuous scale_y_discrete theme element_blank
#' @importFrom glue glue
#' @importFrom scales pretty_breaks
#' @importFrom mlbplotR geom_mlb_logos clean_team_abbrs
#' @export
viz_years_country <- function(metadata_df, top_n = 30) {
  if (is.null(metadata_df) || !is.data.frame(metadata_df)) {
    stop("`metadata_df` must be a data frame.")
  }

  required_cols <- c("Name", "From", "To", "Yrs", "State", "PlayerID")
  missing_cols <- setdiff(required_cols, names(metadata_df))
  if (length(missing_cols)) {
    stop("`metadata_df` is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!nrow(metadata_df)) {
    stop("`metadata_df` has no rows after filtering.")
  }

  top_n <- as.integer(top_n[1])
  if (is.na(top_n) || top_n <= 0) {
    stop("`top_n` must be a positive integer.")
  }

  current_year <- as.integer(format(Sys.Date(), "%Y"))

  plot_data <- metadata_df |>
    rename(
      Player = Name,
      Start = From,
      End = To,
      Seasons = Yrs
    ) |>
    mutate(
      Start = as.integer(Start),
      End = as.integer(End),
      Seasons = as.integer(Seasons),
      State = if_else(is.na(State) | State == "", "Unknown", State)
    ) |>
    filter(
      !is.na(Start),
      !is.na(End),
      !is.na(State),
      Seasons > 0
    ) |>
    arrange(desc(Seasons), desc(End)) |>
    slice_head(n = top_n)

  if (!nrow(plot_data)) {
    stop("No players met the filtering criteria for plotting.")
  }

  top_metadata <- metadata_df |>
    dplyr::semi_join(plot_data |> dplyr::select(PlayerID), by = "PlayerID") |>
    dplyr::distinct(PlayerID, .keep_all = TRUE)

  logs_df <- tryCatch(
    get_career_game_logs(
      top_metadata,
      include_postseason = FALSE,
      split_postseason_result = FALSE,
      verbose = FALSE
    ),
    error = function(e) {
      warning("Failed to retrieve career game logs for logo placement: ", conditionMessage(e))
      NULL
    }
  )

  team_info <- NULL
  if (!is.null(logs_df)) {
    if (is.list(logs_df) && "regular" %in% names(logs_df) &&
        !inherits(logs_df, "data.frame")) {
      logs_df <- logs_df$regular
    }

    if (is.list(logs_df) && !inherits(logs_df, "data.frame")) {
      logs_df <- tryCatch(dplyr::bind_rows(logs_df), error = function(e) NULL)
    }

    if (!inherits(logs_df, "data.frame")) {
      logs_df <- tryCatch(tibble::as_tibble(logs_df), error = function(e) NULL)
    }

    if (!is.null(logs_df) && inherits(logs_df, "data.frame") && nrow(logs_df)) {
      if (!("Tm" %in% names(logs_df))) logs_df$Tm <- NA_character_
      if (!("Team" %in% names(logs_df))) logs_df$Team <- NA_character_
      if (!("Gcar" %in% names(logs_df))) logs_df$Gcar <- seq_len(nrow(logs_df))

      team_info <- logs_df |>
        filter(!is.na(PlayerID)) |>
        mutate(
          Date = as.Date(Date),
          Gcar_num = suppressWarnings(as.numeric(Gcar)),
          team_abbr = dplyr::coalesce(Tm, Team),
          team_abbr = dplyr::na_if(team_abbr, "TOT"),
          team_abbr = dplyr::case_when(
            team_abbr %in% c("FLO", "FLA") ~ "MIA",
            team_abbr == "ANA" ~ "LAA",
            team_abbr == "CAL" ~ "LAA",
            team_abbr == "MON" ~ "WSN",
            team_abbr == "KCA" ~ "KCR",
            team_abbr == "TBD" ~ "TBR",
            TRUE ~ team_abbr
          ),
          team_abbr = mlbplotR::clean_team_abbrs(team_abbr, keep_non_matches = FALSE)
        ) |>
    filter(!is.na(Date), !is.na(team_abbr)) |>
    arrange(PlayerID, Date, Gcar_num, Gcar)

      if (!is.null(team_info) && nrow(team_info)) {
        debut_teams <- team_info |>
          group_by(PlayerID) |>
          slice_head(n = 1) |>
          ungroup() |>
          select(PlayerID, team_debut = team_abbr)

        final_teams <- team_info |>
          group_by(PlayerID) |>
          slice_tail(n = 1) |>
          ungroup() |>
          select(PlayerID, team_final = team_abbr)

        plot_data <- plot_data |>
          left_join(debut_teams, by = "PlayerID") |>
          left_join(final_teams, by = "PlayerID")
      }
    }
  }

  if (!"team_debut" %in% names(plot_data)) {
    plot_data <- plot_data |>
      mutate(
        team_debut = NA_character_,
        team_final = NA_character_
      )
  }

  plot_data <- plot_data |>
    mutate(
      Active = !is.na(End) & End == current_year,
      Player_chr = Player
    )

  label_map <- setNames(
    if_else(plot_data$Active,
            paste0("⬤ ", plot_data$Player_chr),
            plot_data$Player_chr),
    plot_data$Player_chr
  )

  plot_data <- plot_data |>
    mutate(
      Player = factor(Player_chr, levels = rev(unique(Player_chr)))
    )

  if (!nrow(plot_data)) {
    stop("No players met the filtering criteria for plotting.")
  }

  country_label <- if ("Country" %in% names(plot_data)) {
    glue::glue_collapse(unique(plot_data$Country), sep = ", ", last = " & ")
  } else {
    "Selected Players"
  }

  state_levels <- sort(unique(plot_data$State))
  state_palette <- setNames(
    grDevices::hcl.colors(length(state_levels), palette = "Harmonic"),
    state_levels
  )

  logo_width <- 0.045
  logo_height <- 0.045

  name_labels <- plot_data |> mutate(label = label_map[Player_chr])

  ggplot(plot_data, aes(y = Player, colour = State)) +
    geom_segment(
      aes(x = Start, xend = End, yend = Player),
      linewidth = 2.4,
      lineend = "round"
    ) +
    geom_point(
      aes(x = Start),
      size = 3,
      shape = 21,
      fill = "white",
      colour = "black",
      stroke = 1
    ) +
    geom_point(
      aes(x = End),
      size = 3,
      shape = 21,
      fill = "white",
      colour = "black",
      stroke = 1
    ) +
    geom_text(
      aes(x = (Start + End) / 2, label = glue("{Seasons} yrs")),
      vjust = -0.6,
      size = 3
    ) +
    mlbplotR::geom_mlb_logos(
      data = plot_data |> filter(!is.na(team_debut)),
      inherit.aes = FALSE,
      aes(x = Start, y = Player, team_abbr = team_debut),
      width = logo_width,
      height = logo_height
    ) +
    mlbplotR::geom_mlb_logos(
      data = plot_data |> filter(!is.na(team_final)),
      inherit.aes = FALSE,
      aes(x = End, y = Player, team_abbr = team_final),
      width = logo_width,
      height = logo_height
    ) +
    scale_colour_manual(values = state_palette, na.translate = FALSE) +
    scale_x_continuous(
      breaks = pretty_breaks(8),
      expand = expansion(mult = c(0.01, 0.12))
    ) +
    scale_y_discrete(
      expand = expansion(mult = c(0.12, 0.05)),
      labels = function(x) unname(label_map[as.character(x)])
    ) +
    labs(
      title = glue("MLB Career Spans for Players from {country_label}"),
      subtitle = glue("Top {nrow(plot_data)} players ranked by seasons played"),
      x = "Season",
      y = NULL,
      colour = "Birth State"
    ) +
    guides(colour = ggplot2::guide_legend(override.aes = list(linewidth = 4))) +
    theme_bbgraphs() +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "bottom"
    )
}
