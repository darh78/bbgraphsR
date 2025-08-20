#' Convert get_career_game_logs list output to a single tibble
#'
#' @param logs_list The output from get_career_game_logs() when
#'                  split_postseason_result = TRUE
#' @param season_type Character string: "regular" or "postseason"
#'
#' @return A tibble combining all players' logs of the given season type.
#'         Returns an empty tibble if no data found.
#' @export
logs_list_to_tibble <- function(logs_list, season_type = c("regular", "postseason")) {
  season_type <- match.arg(season_type)

  # Filter only the chosen season type
  selected <- lapply(logs_list, function(player_data) {
    if (!is.list(player_data)) return(NULL)
    if (!season_type %in% names(player_data)) return(NULL)
    player_data[[season_type]]
  })

  # Remove NULL entries
  selected <- Filter(Negate(is.null), selected)

  if (length(selected) == 0L) {
    return(tibble::tibble())
  }

  # Bind into single tibble
  dplyr::bind_rows(selected)
}

#' Add career-wide game counter (Gcar_real) across Regular+Post
#' @keywords internal
#' Add a career-wide game index (Gcar_real) across Regular + Postseason
#' @keywords internal
.add_career_gcar <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  req <- c("PlayerID", "Date")
  if (!all(req %in% names(df))) return(df)

  # Ensure Date is Date
  if (!inherits(df$Date, "Date")) {
    suppressWarnings(df$Date <- as.Date(df$Date))
  }

  # Make SeasonType ordering deterministic if present
  if ("SeasonType" %in% names(df)) {
    df$SeasonType <- factor(df$SeasonType, levels = c("Regular", "Postseason"), ordered = TRUE)
  }

  # Robust tie-breakers after Date:
  #  - SeasonType (Regular before Postseason if same date)
  #  - Gcar (original BR per-season counter) if available
  #  - Rk (row index) if available
  df |>
    dplyr::arrange(
      .data$PlayerID,
      .data$Date,
      dplyr::across(dplyr::any_of("SeasonType")),
      dplyr::coalesce(.data$Gcar, Inf),
      dplyr::coalesce(.data$Rk,  Inf)
    ) |>
    dplyr::group_by(.data$PlayerID) |>
    dplyr::mutate(Gcar_real = dplyr::row_number()) |>
    dplyr::ungroup()
}
