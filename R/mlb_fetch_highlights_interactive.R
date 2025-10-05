#' Convenience wrapper: fetch highlights and then interactively view/download
#'
#' @description
#' Calls `mlb_fetch_highlights()` and then `mlb_browse_highlights()` to let the user
#' choose rows to view/download. Saves to `./videos` by default.
#'
#' @inheritParams mlb_fetch_highlights
#' @param action Passed to `mlb_browse_highlights()` ("view" or "download"). If missing, prompts.
#' @param include_hls Logical. From `mlb_fetch_highlights()`. If `FALSE`, keeps MP4s only.
#' @param destdir Directory for downloads (defaults to `./videos` if `NULL`).
#'
#' @return Invisibly, the subset acted upon (same columns as input).
#' @export
mlb_fetch_highlights_interactive <- function(start_date,
                                             end_date,
                                             team,
                                             player,
                                             play_description = NULL,
                                             action,
                                             include_hls = FALSE,
                                             destdir = NULL) {
  df <- mlb_fetch_highlights(
    start_date = start_date,
    end_date   = end_date,
    team       = team,
    player     = player,
    play_description = play_description,
    include_hls = include_hls
  )
  if (!nrow(df)) {
    message("No matching highlights found.")
    return(invisible(df))
  }
  mlb_browse_highlights(df, action = action, destdir = destdir)
}
