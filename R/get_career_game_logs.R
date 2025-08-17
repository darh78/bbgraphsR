#' Retrieve career batting game logs for players
#'
#' Downloads and caches batting game logs from Baseball Reference for all players
#' in a given metadata data frame. Logs are saved to Parquet cache and also returned
#' as R objects. Supports both regular-season and postseason logs.
#'
#' @param metadata_df A data frame with at least the columns:
#'   `Name`, `From`, `To`, `PlayerID`, and `Country`.
#' @param include_postseason Logical, default `TRUE`. If `TRUE`, also scrape
#'   postseason logs (one request per player).
#' @param split_postseason_result Logical, default `TRUE`. If `TRUE`, the return
#'   value is a list per player, with separate `regular` and `postseason` tibbles.
#'   If `FALSE`, only regular season data is returned as a combined tibble.
#' @param sleep_sec Number of seconds to wait between requests (to be polite).
#' @param jitter_sec Random jitter (uniform 0–`jitter_sec`) added to `sleep_sec`.
#' @param overwrite_cache Logical, default `FALSE`. If `TRUE`, forces re-download
#'   even if data exists in the Parquet cache.
#' @param compression Compression algorithm for Parquet files.
#'   One of `"zstd"` (default) or `"snappy"`.
#' @param verbose Logical, default `interactive()`. If `TRUE`, prints progress
#'   messages during scraping and caching.
#'
#' @return
#' If `split_postseason_result = TRUE`, a named list keyed by `PlayerID`, where each
#' element is itself a list with `$regular` and `$postseason` tibbles (or `NULL` if absent).
#' If `split_postseason_result = FALSE`, a single tibble combining all regular season logs.
#'
#' @examples
#' \dontrun{
#' # Example metadata
#' players <- tibble::tibble(
#'   Name = c("Miguel Cabrera"),
#'   From = 2003, To = 2023,
#'   PlayerID = "cabremi01",
#'   Country = "Venezuela"
#' )
#'
#' # Get logs (both reg + postseason, returned separately)
#' logs <- get_career_game_logs(players)
#'
#' # Convert list of postseason logs into one tibble
#' post_tbl <- logs_list_to_tibble(logs, "postseason")
#' }
#'
#' @export
get_career_game_logs <- function(metadata_df,
                                 include_postseason = TRUE,
                                 split_postseason_result = TRUE,   # prefer separate by default
                                 sleep_sec = 3,
                                 jitter_sec = 0.5,
                                 overwrite_cache = FALSE,
                                 compression = c("zstd","snappy"),
                                 verbose = interactive()) {

  compression <- match.arg(compression)
  md <- metadata_df |> dplyr::distinct(PlayerID, .keep_all = TRUE)

  out_list <- vector("list", length = nrow(md))
  names(out_list) <- md$PlayerID

  for (i in seq_len(nrow(md))) {
    pid      <- md$PlayerID[i]
    row_meta <- md[i, , drop = FALSE]

    if (verbose) message("PlayerID: ", pid, " (", row_meta$From, "–", row_meta$To, ")")

    # ── Check cache ──────────────────────────────────────────────────────────
    cached <- if (!overwrite_cache) bbgr_parquet_read_player(pid) else NULL
    if (!is.null(cached)) {
      if (verbose) message("  Using cached data for ", pid)
      out_list[[pid]] <- get(pid, envir = bbgr_mem_cache(), ifnotfound = cached)
      next
    }

    # ── Regular season ───────────────────────────────────────────────────────
    reg_years <- list()
    for (yr in seq(row_meta$From, row_meta$To)) {
      url <- .gl_url(pid, yr, postseason = FALSE)
      if (verbose) message("  Year ", yr, ": ", url)
      resp <- httr::GET(url)
      if (httr::status_code(resp) != 200L) {
        Sys.sleep(sleep_sec + runif(1, 0, jitter_sec))
        next
      }
      df <- .parse_table(resp, yr, pid, row_meta, postseason = FALSE, verbose = verbose)
      if (!is.null(df) && nrow(df)) reg_years[[as.character(yr)]] <- df
      Sys.sleep(sleep_sec + runif(1, 0, jitter_sec))
    }
    reg <- if (length(reg_years)) dplyr::bind_rows(reg_years) else NULL

    # ── Postseason ───────────────────────────────────────────────────────────
    post <- NULL
    if (isTRUE(include_postseason)) {
      url <- .gl_url(pid, yr = 0, postseason = TRUE)
      if (verbose) message("  Postseason: ", url)
      resp <- httr::GET(url)
      if (httr::status_code(resp) == 200L) {
        post <- .parse_table(resp, yr = 0, pid, row_meta, postseason = TRUE, verbose = verbose)
        if (!is.null(post) && nrow(post) == 0L) post <- NULL
      }
      Sys.sleep(sleep_sec + runif(1, 0, jitter_sec))
    }

    # ── Append to Parquet (only if data exists) ───────────────────────────────
    if (!is.null(reg)  && nrow(reg))  bbgr_parquet_append(reg,  keep_cols = names(reg),  compression = compression)
    if (!is.null(post) && nrow(post)) bbgr_parquet_append(post, keep_cols = names(post), compression = compression)

    # ── Store in memory ───────────────────────────────────────────────────────
    assign(pid, list(regular = reg, postseason = post), envir = bbgr_mem_cache())

    # ── Prepare output ───────────────────────────────────────────────────────
    if (isTRUE(split_postseason_result)) {
      out_list[[pid]] <- list(regular = reg, postseason = post)
    } else {
      out_list[[pid]] <- reg
    }
  }

  # ── Final return ───────────────────────────────────────────────────────────
  if (isTRUE(split_postseason_result)) {
    return(out_list)
  } else {
    out <- Filter(Negate(is.null), out_list)
    if (length(out) == 0L) return(tibble::tibble())
    return(dplyr::bind_rows(out))
  }
}
