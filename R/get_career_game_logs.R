#' Retrieve career batting game logs for players
#'
#' Downloads and caches batting game logs from Baseball Reference for all players
#' in a given metadata data frame. Logs are saved to Parquet cache and also returned
#' as R objects. Supports both regular-season and postseason logs.
#'
#' @param metadata_df A data frame with at least the columns:
#'   `Name`, `From`, `To`, `PlayerID`, and `Country`.
#' @param include_postseason Logical, default `TRUE`. If `TRUE`, also scrape postseason logs (one extra request per player)
#' @param split_postseason_result Logical, default `TRUE`. If TRUE, return per-player list with separate
#'   `regular` and `postseason` tibbles; if `FALSE`, return a single tibble
#'   that *combines* regular+postseason using an Arrow-safe union with aligned schemas.
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
                                 split_postseason_result = TRUE,
                                 sleep_sec = 3,
                                 jitter_sec = 0.5,
                                 overwrite_cache = FALSE,
                                 compression = c("zstd","snappy"),
                                 verbose = interactive()) {

  compression <- match.arg(compression)

  md <- metadata_df |>
    dplyr::distinct(PlayerID, .keep_all = TRUE)

#### Estimate maximum number of seasons/pages to scrape ───────────────────── ####
  seasons_per_player <- md$To - md$From + 1L
  total_seasons_est  <- sum(seasons_per_player)
  total_pages_est    <- total_seasons_est + if (isTRUE(include_postseason)) nrow(md) else 0L

  SEASON_CAP_WARN <- getOption("bbgraphsR.season_cap_warn", 100L)

  if (interactive() && total_seasons_est >= SEASON_CAP_WARN) {
    msg <- sprintf(
      paste0("This job may fetch up to %d regular-season pages",
             if (isTRUE(include_postseason)) " + %d postseason pages" else "",
             " (≈ %d total). Continue?"),
      total_seasons_est,
      if (isTRUE(include_postseason)) nrow(md) else NULL,
      total_pages_est
    )
    ans <- utils::askYesNo(msg)
    if (is.na(ans) || !ans) {
      message("Aborted by user before scraping.")
      return(invisible(tibble::tibble()))
    }
  }

  out_list <- vector("list", length = nrow(md))
  names(out_list) <- md$PlayerID

  for (i in seq_len(nrow(md))) {
    pid      <- md$PlayerID[i]
    row_meta <- md[i, , drop = FALSE]

    if (verbose) message("PlayerID: ", pid, " (", row_meta$From, "–", row_meta$To, ")")

    # ── If not overwriting and data exists in parquet, short‑circuit ─────────
    if (!overwrite_cache) {
      # When the caller wants a single tibble, read the combined (both) now.
      if (!split_postseason_result) {
        combined <- bbgr_parquet_read_player(pid, season_type = "both", return = "tibble")
        if (!is.null(combined)) {
          out_list[[pid]] <- combined
          next
        }
      } else {
        # For split mode, try to load each part
        reg_ds  <- bbgr_parquet_read_player(pid, season_type = "regular",   return = "tibble")
        post_ds <- bbgr_parquet_read_player(pid, season_type = "postseason", return = "tibble")
        if (!is.null(reg_ds) || !is.null(post_ds)) {
          assign(pid, list(regular = reg_ds, postseason = post_ds), envir = bbgr_mem_cache())
          out_list[[pid]] <- list(regular = reg_ds, postseason = post_ds)
          next
        }
      }
    }

    #### Regular season: fetch per-year ─────────────────────────────────────── ####
    years <- seq(row_meta$From, row_meta$To)
    reg_years <- vector("list", length(years))
    names(reg_years) <- as.character(years)

    if (verbose && interactive()) {
      message("Scraping ", row_meta$Name, " (", length(years),
              " seasons", if (include_postseason) " + postseason" else "", ")...")
      pb <- utils::txtProgressBar(min = 0, max = length(years) + as.integer(include_postseason), style = 3)
    } else {
      pb <- NULL
    }

    step <- 0L
    for (yr in years) {
      resp <- httr::GET(.gl_url(pid, yr, postseason = FALSE))
      if (httr::status_code(resp) == 200L) {
        df <- .parse_table(resp, yr, pid, row_meta, postseason = FALSE, verbose = FALSE)
        if (!is.null(df) && nrow(df)) reg_years[[as.character(yr)]] <- df
      }
      step <- step + 1L
      if (!is.null(pb)) utils::setTxtProgressBar(pb, step)
      Sys.sleep(sleep_sec + stats::runif(1, 0, jitter_sec))
    }

    reg <- if (length(Filter(Negate(is.null), reg_years))) {
      dplyr::bind_rows(Filter(Negate(is.null), reg_years))
    } else NULL

    #### ── Postseason (optional) ────────────────────────────────────────────────####
    post <- NULL
    if (isTRUE(include_postseason)) {
      resp <- httr::GET(.gl_url(pid, yr = 0, postseason = TRUE))
      if (httr::status_code(resp) == 200L) {
        post <- .parse_table(resp, yr = 0, pid, row_meta, postseason = TRUE, verbose = FALSE)
        if (!is.null(post) && nrow(post) == 0L) post <- NULL
      }
      step <- step + 1L
      if (!is.null(pb)) utils::setTxtProgressBar(pb, step)
      Sys.sleep(sleep_sec + stats::runif(1, 0, jitter_sec))
    }

    # ✅ Close the progress bar and print a done line (after regular + optional postseason)
    if (!is.null(pb)) close(pb)
    if (verbose) message(" ✅ Done: ", row_meta$Name, " (", pid, ")")

    #### Persist what we got to Parquet ─────────────────────────────────────── ####
    if (!is.null(reg)  && nrow(reg))  bbgr_parquet_append(reg,  keep_cols = names(reg),  compression = compression)
    if (!is.null(post) && nrow(post)) bbgr_parquet_append(post, keep_cols = names(post), compression = compression)

    #### ── In‑memory cache for split mode ─────────────────────────────────────── ####
    assign(pid, list(regular = reg, postseason = post), envir = bbgr_mem_cache())

    #### ── Return shape ──────────────────────────────────────────────────────── ####
    if (isTRUE(split_postseason_result)) {
      out_list[[pid]] <- list(regular = reg, postseason = post)
    } else {
      # 🔹 NEW: read back the Arrow‑safe combined (both) with aligned schemas
      combined <- bbgr_parquet_read_player(pid, season_type = "both", return = "tibble")
      out_list[[pid]] <- combined
    }
  }

  #### Final return ───────────────────────────────────────────────────────────####
  if (isTRUE(split_postseason_result)) {
    return(out_list)
  } else {
    out <- Filter(Negate(is.null), out_list)
    if (length(out) == 0L) return(tibble::tibble())
    combined <- dplyr::bind_rows(out)

    # 🆕 only add Gcar_real when we asked for reg+post combined
    if (isTRUE(include_postseason)) {
      combined <- .add_career_gcar(combined)
    }

    return(combined)
  }
}
