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
