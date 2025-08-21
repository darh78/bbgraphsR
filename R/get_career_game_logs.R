#' Retrieve career batting game logs for players
#'
#' Downloads and caches batting game logs from Baseball Reference for all players
#' in a given metadata data frame. Logs are saved to Parquet cache and also returned
#' as tidy tibbles. Supports both regular-season and postseason logs.
#'
#' @param metadata_df A data frame with at least the columns:
#'   `Name`, `From`, `To`, `PlayerID`, and `Country`.
#' @param include_postseason Logical, default `FALSE`. If `TRUE`, also scrape postseason logs
#'   (one extra request per player).
#' @param split_postseason_result Logical, default `TRUE`. If `TRUE`, return **two**
#'   separate tibbles: `regular` and `postseason`. If `FALSE`, return a **single tibble**
#'   combining whatever is included (regular ± postseason) and add `Gcar_real`
#'   reindexed across all rows in chronological order.
#' @param sleep_sec Seconds to wait between requests (politeness).
#' @param jitter_sec Random jitter (uniform 0–`jitter_sec`) added to `sleep_sec`.
#' @param overwrite_cache If `TRUE`, force re-download even if cache exists.
#' @param compression Parquet codec: `"zstd"` (default) or `"snappy"`.
#' @param verbose If `TRUE`, print status/progress.
#'
#' @return
#' If `split_postseason_result = TRUE` (default), a list with two tibbles:
#' \describe{
#'   \item{regular}{All regular-season logs combined.}
#'   \item{postseason}{All postseason logs combined.}
#' }
#'
#' If `split_postseason_result = FALSE`, a single tibble combining all logs
#' that were requested (regular ± postseason) with an extra `Gcar_real` column
#' that reindexes games across the whole career chronologically.
#'
#' @examples
#' \dontrun{
#' # One tibble: combined reg+post with Gcar_real
#' logs <- get_career_game_logs(players, split_postseason_result = FALSE)
#'
#' # Two tibbles: reg and post separately
#' logs_split <- get_career_game_logs(players, split_postseason_result = TRUE)
#' logs_split$regular
#' logs_split$postseason
#' }
#'
#' @export
get_career_game_logs <- function(metadata_df,
                                 include_postseason = FALSE,
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
    # Build a message whose number of %d matches the arguments (no sprintf pitfalls)
    msg_parts <- c(
      sprintf("This job may fetch up to %d regular-season pages (if not in cache)", total_seasons_est),
      if (isTRUE(include_postseason)) sprintf(" + %d postseason pages", nrow(md)) else NULL,
      sprintf(" (≈ %d total).", total_pages_est),
      "\nDo you want to continue?"
    )
    msg <- paste0(msg_parts, collapse = "")

    # Print the message cleanly, then ask Yes/No
    message(msg)
    ans <- utils::askYesNo("Continue?")
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
    # ---------- cache short-circuit -----------------------------------------
    if (!overwrite_cache) {
      if (isTRUE(split_postseason_result) && isTRUE(include_postseason)) {
        # split AND postseason requested → return list only here
        reg_ds  <- bbgr_parquet_read_player(pid, season_type = "regular",   return = "tibble")
        post_ds <- bbgr_parquet_read_player(pid, season_type = "postseason", return = "tibble")
        if (!is.null(reg_ds) || !is.null(post_ds)) {
          out_list[[pid]] <- list(regular = reg_ds, postseason = post_ds)
          next
        }
      } else if (isTRUE(split_postseason_result) && !isTRUE(include_postseason)) {
        # split requested but postseason = FALSE → return a SINGLE tibble (regular only)
        reg_ds <- bbgr_parquet_read_player(pid, season_type = "regular", return = "tibble")
        if (!is.null(reg_ds)) {
          out_list[[pid]] <- reg_ds
          next
        }
      } else {
        # non-split mode → single tibble (both or regular)
        if (isTRUE(include_postseason)) {
          combined <- bbgr_parquet_read_player(pid, season_type = "both", return = "tibble")
          if (!is.null(combined)) {
            out_list[[pid]] <- .add_career_gcar(combined)
            next
          }
        } else {
          reg_only <- bbgr_parquet_read_player(pid, season_type = "regular", return = "tibble")
          if (!is.null(reg_only)) {
            out_list[[pid]] <- reg_only
            next
          }
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
    # ---- choose the per-player result shape (STILL INSIDE THE LOOP) --------
    if (isTRUE(split_postseason_result) && isTRUE(include_postseason)) {
      # list entry with $regular / $postseason
      out_list[[pid]] <- list(regular = reg, postseason = post)
    } else {
      # single tibble per player
      if (isTRUE(include_postseason)) {
        # combine reg + post and add Gcar_real only when postseason included
        combined <- dplyr::bind_rows(Filter(Negate(is.null), list(reg, post)))
        out_list[[pid]] <- if (nrow(combined)) .add_career_gcar(combined) else tibble::tibble()
      } else {
        out_list[[pid]] <- if (!is.null(reg)) reg else tibble::tibble()
      }
    }
  } # <--- CLOSE the for-loop RIGHT HERE

  # ---- Final return (NOW OUTSIDE THE LOOP) ---------------------------------
  if (isTRUE(include_postseason) && isTRUE(split_postseason_result)) {
    # per-player list of {regular, postseason}
    return(out_list)
  } else {
    # flatten to a single tibble
    out <- Filter(is.data.frame, out_list)  # only keep tibbles
    if (!length(out)) {
      return(tibble::tibble())
    } else {
      return(dplyr::bind_rows(out))
    }
  }
}
