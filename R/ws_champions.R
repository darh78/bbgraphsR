#' Retrieve the MLB World Series champion for a single season
#'
#' @keywords internal
#' @noRd
get_ws_for_year <- function(yr) {
  # World Series = game_type "W" per baseballr docs
  df <- suppressWarnings(
    suppressMessages(
      tryCatch(
        baseballr::mlb_schedule_postseason(season = yr, game_type = "W"),
        error = function(e) NULL
      )
    )
  )
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  finished <- df %>%
    dplyr::filter(
      .data$status_detailed_state %in%
        c("Final", "Completed Early", "Game Over")
    )

  if (nrow(finished) == 0) {
    return(NULL)
  }

  wins <- dplyr::bind_rows(
    finished %>%
      dplyr::transmute(
        season = as.integer(season),
        team_id = teams_home_team_id,
        team_name = teams_home_team_name,
        w = as.integer(teams_home_is_winner),
        l = as.integer(!teams_home_is_winner)
      ),
    finished %>%
      dplyr::transmute(
        season = as.integer(season),
        team_id = teams_away_team_id,
        team_name = teams_away_team_name,
        w = as.integer(teams_away_is_winner),
        l = as.integer(!teams_away_is_winner)
      )
  ) %>%
    dplyr::group_by(season, team_id, team_name) %>%
    dplyr::summarise(
      wins = sum(w, na.rm = TRUE),
      losses = sum(l, na.rm = TRUE),
      .groups = "drop"
    )

  wins_sorted <- wins %>% dplyr::arrange(dplyr::desc(wins))

  if (nrow(wins_sorted) == 0) {
    return(NULL)
  }

  champ_season <- as.integer(wins_sorted$season[1])
  champion_team_id <- as.integer(wins_sorted$team_id[1])
  champion_name <- wins_sorted$team_name[1]
  champion_wins <- as.integer(wins_sorted$wins[1])

  has_runner_up <- nrow(wins_sorted) >= 2
  runner_up_team_id <- if (has_runner_up) as.integer(wins_sorted$team_id[2]) else NA_integer_
  runner_up_name <- if (has_runner_up) wins_sorted$team_name[2] else NA_character_
  runner_up_wins <- if (has_runner_up) as.integer(wins_sorted$wins[2]) else NA_integer_

  if (!is.na(runner_up_wins) && !is.na(champion_wins) && champion_wins == runner_up_wins) {
    runner_up_team_id <- NA_integer_
    runner_up_name <- NA_character_
    runner_up_wins <- NA_integer_
  }

  series_result <- if (!is.na(runner_up_wins)) {
    sprintf("%d-%d", champion_wins, runner_up_wins)
  } else {
    NA_character_
  }

  tibble::tibble(
    season = champ_season,
    champion_team_id = champion_team_id,
    champion = champion_name,
    runner_up_team_id = runner_up_team_id,
    runner_up = runner_up_name,
    champion_wins = champion_wins,
    runner_up_wins = runner_up_wins,
    series_result = series_result
  )
}

#' Retrieve the MLB World Series champion from Lahman Teams data
#'
#' Provides a fallback for seasons where the Stats API does not return data.
#'
#' @keywords internal
#' @noRd
get_ws_for_year_fallback <- function(yr, teams_data) {
  year_data <- teams_data %>%
    dplyr::filter(.data$yearID == yr)

  if (nrow(year_data) == 0) {
    return(NULL)
  }

  champ_row <- year_data %>%
    dplyr::filter(.data$WSWin == "Y") %>%
    dplyr::slice(1)

  if (nrow(champ_row) == 0 || is.na(champ_row$name[1])) {
    return(NULL)
  }

  runner_row <- year_data %>%
    dplyr::filter(.data$LgWin == "Y", .data$WSWin != "Y") %>%
    dplyr::slice(1)

  runner_name <- if (nrow(runner_row) == 0) NA_character_ else runner_row$name[1]

  tibble::tibble(
    season = as.integer(yr),
    champion_team_id = NA_integer_,
    champion = champ_row$name[1],
    runner_up_team_id = NA_integer_,
    runner_up = runner_name,
    champion_wins = NA_integer_,
    runner_up_wins = NA_integer_,
    series_result = NA_character_
  )
}

#' Fetch MLB World Series champions data
#'
#' Downloads postseason schedules for the given season range and collapses
#' results into a tibble with champion and runner-up information.
#'
#' No requests are made for seasons outside the supplied range, which defaults
#' to the first modern World Series (1903) through the most recently completed
#' MLB season.
#'
#' @param start_season Integer season to start from. Defaults to 1903.
#' @param end_season Integer season to end at. Defaults to the last completed
#'   MLB season (current year minus one).
#' @return A tibble with one row per season.
#' @examples
#' \dontrun{
#' ws <- fetch_ws_champions()
#' }
#' @export
fetch_ws_champions <- function(start_season = 1903L, end_season = NULL) {
  if (is.null(end_season)) {
    end_season <- as.integer(format(Sys.Date(), "%Y")) - 1L
  }

  if (end_season < start_season) {
    stop("`end_season` must be greater than or equal to `start_season`.", call. = FALSE)
  }

  seasons <- seq.int(start_season, end_season)

  ws_list <- seasons %>%
    purrr::map(get_ws_for_year) %>%
    purrr::compact()

  ws_champions <- if (length(ws_list) > 0) {
    purrr::list_rbind(ws_list)
  } else {
    tibble::tibble(
      season = integer(),
      champion_team_id = integer(),
      champion = character(),
      runner_up_team_id = integer(),
      runner_up = character(),
      champion_wins = integer(),
      runner_up_wins = integer(),
      series_result = character()
    )
  }

  missing_seasons <- setdiff(seasons, ws_champions$season)

  if (length(missing_seasons) > 0) {
    teams_path <- file.path("data", "Teams.csv")

    if (file.exists(teams_path)) {
      teams_data <- utils::read.csv(teams_path, stringsAsFactors = FALSE)[, -1]
      teams_data$yearID <- as.integer(teams_data$yearID)

      fallback_list <- missing_seasons %>%
        purrr::map(get_ws_for_year_fallback, teams_data = teams_data) %>%
        purrr::compact()

      if (length(fallback_list) > 0) {
        fallback_tbl <- purrr::list_rbind(fallback_list)

        ws_champions <- ws_champions %>%
          dplyr::bind_rows(fallback_tbl) %>%
          dplyr::mutate(
            has_team_id = !is.na(.data$champion_team_id)
          ) %>%
          dplyr::arrange(.data$season, dplyr::desc(.data$has_team_id)) %>%
          dplyr::distinct(.data$season, .keep_all = TRUE) %>%
          dplyr::select(-"has_team_id")
      }
    }
  }

  no_ws <- tibble::tibble(
    season = c(1904L, 1994L),
    champion_team_id = NA_integer_,
    champion = NA_character_,
    runner_up_team_id = NA_integer_,
    runner_up = NA_character_,
    champion_wins = NA_integer_,
    runner_up_wins = NA_integer_,
    series_result = NA_character_
  ) %>%
    dplyr::filter(season >= start_season, season <= end_season)

  dplyr::bind_rows(ws_champions, no_ws) %>%
    dplyr::arrange(.data$season) %>%
    dplyr::distinct(.data$season, .keep_all = TRUE)
}
