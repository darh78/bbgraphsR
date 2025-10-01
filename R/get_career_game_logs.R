#' Retrieve career batting game logs for players
#'
#' Downloads and caches batting game logs from Baseball Reference for all players
#' in a given metadata data frame. Logs are saved to Parquet cache and also returned
#' as tidy tibbles. Supports both regular-season and postseason logs.
#' @importFrom dplyr distinct
#'
#' @param metadata_df A data frame with at least the columns:
#'   `Name`, `From`, `To`, `PlayerID`, and `Country`.
#' @param include_postseason Logical, default `FALSE`. If `TRUE`, also scrape postseason logs
#'   (one extra request per player).
#' @param split_postseason_result Logical, default `TRUE`. If `TRUE`, return **two**
#'   separate tibbles: `regular` and `postseason`. If `FALSE`, return a **single tibble**
#'   combining whatever is included (regular +/- postseason) and add `Gcar_real`
#'   reindexed across all rows in chronological order.
#' @param sleep_sec Seconds to wait between requests (politeness).
#' @param jitter_sec Random jitter (uniform 0$`jitter_sec`) added to `sleep_sec`.
#' @param overwrite_cache If `TRUE`, force re-download even if cache exists.
#' @param overwrite_scope When `overwrite_cache = TRUE`, choose between
#'   `"smart"` (default) to only scrape seasons missing from cache (and refresh
#'   the most recent season for active players) or `"force"` to re-download every
#'   season regardless of cache state.
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
#' that were requested (regular +/- postseason) with an extra `Gcar_real` column
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
                                 overwrite_scope = c("smart","force"),
                                 compression = c("zstd","snappy"),
                                 verbose = interactive()) {

  compression <- match.arg(compression)
  overwrite_scope <- match.arg(overwrite_scope)

  md <- metadata_df |>
    dplyr::distinct(PlayerID, .keep_all = TRUE)

  current_year <- suppressWarnings(as.integer(format(Sys.Date(), "%Y")))
  active_buffer <- getOption("bbgraphsR.active_season_buffer", 1L)
  retired_cutoff <- getOption("bbgraphsR.retired_cutoff_years", 5L)

  if (!is.numeric(active_buffer) || is.na(active_buffer)) active_buffer <- 1L
  if (!is.numeric(retired_cutoff) || is.na(retired_cutoff)) retired_cutoff <- 5L

  active_buffer  <- max(0L, as.integer(active_buffer))
  retired_cutoff <- max(0L, as.integer(retired_cutoff))

  active_threshold  <- if (is.na(current_year)) Inf  else current_year - active_buffer
  retired_threshold <- if (is.na(current_year)) -Inf else current_year - retired_cutoff

#### Estimate maximum number of seasons/pages to scrape --------------------- ####
  seasons_per_player <- md$To - md$From + 1L
  total_seasons_est  <- sum(seasons_per_player)
  total_pages_est    <- total_seasons_est + if (isTRUE(include_postseason)) nrow(md) else 0L

  SEASON_CAP_WARN <- getOption("bbgraphsR.season_cap_warn", 100L)

  if (interactive() && total_seasons_est >= SEASON_CAP_WARN) {
    # Build a message whose number of %d matches the arguments (no sprintf pitfalls)
    msg_parts <- c(
      sprintf("This job may fetch up to %d regular-season pages (if not in cache)", total_seasons_est),
      if (isTRUE(include_postseason)) sprintf(" + %d postseason pages", nrow(md)) else NULL,
      sprintf(" ( %d total).", total_pages_est),
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

    if (verbose) message("PlayerID: ", pid, " (", row_meta$From, "$", row_meta$To, ")")

    years <- seq(row_meta$From, row_meta$To)
    cached_regular_years <- .bbgr_cached_years(pid, "regular")
    cached_post_years <- .bbgr_cached_years(pid, "postseason")
    is_active_player <- isTRUE(!is.na(row_meta$To) && row_meta$To >= active_threshold)
    is_retired_player <- isTRUE(!is.na(row_meta$To) && row_meta$To <= retired_threshold)

    expected_reg_seasons <- NA_integer_
    if ("Yrs" %in% names(row_meta)) {
      val <- suppressWarnings(as.integer(row_meta$Yrs))
      if (length(val)) expected_reg_seasons <- val[1]
    }
    if (is.na(expected_reg_seasons) || expected_reg_seasons < 0L) expected_reg_seasons <- NA_integer_
    cached_reg_seasons <- length(cached_regular_years)
    has_full_regular_cache <- !is.na(expected_reg_seasons) && cached_reg_seasons >= expected_reg_seasons

    missing_regular_years <- setdiff(years, cached_regular_years)
    regular_years_to_fetch <- integer()

    if (overwrite_cache && identical(overwrite_scope, "force")) {
      regular_years_to_fetch <- years
    } else {
      if (!has_full_regular_cache || is.na(expected_reg_seasons)) {
        regular_years_to_fetch <- missing_regular_years
      }

      if (overwrite_cache && !identical(overwrite_scope, "force") && is_active_player) {
        recent_year <- suppressWarnings(as.integer(row_meta$To))
        regular_years_to_fetch <- union(regular_years_to_fetch, recent_year)
      }

      if (!overwrite_cache && has_full_regular_cache) {
        regular_years_to_fetch <- integer()
      }

      if (overwrite_cache && !identical(overwrite_scope, "force") && has_full_regular_cache) {
        regular_years_to_fetch <- setdiff(regular_years_to_fetch, missing_regular_years)
      }
    }

    regular_years_to_fetch <- sort(unique(regular_years_to_fetch))

    need_regular_scrape <- length(regular_years_to_fetch) > 0L

    need_postseason_scrape <- FALSE
    if (isTRUE(include_postseason)) {
      postseason_cached <- length(cached_post_years) > 0L
      if (overwrite_cache) {
        if (identical(overwrite_scope, "force")) {
          need_postseason_scrape <- TRUE
        } else {
          need_postseason_scrape <- !postseason_cached || is_active_player
          if (is_retired_player && postseason_cached) need_postseason_scrape <- FALSE
        }
      } else {
        need_postseason_scrape <- !postseason_cached
      }
    }

    reg_cached <- NULL
    post_cached <- NULL
    cache_read_error <- FALSE

    if (!need_regular_scrape) {
      reg_cached <- tryCatch(
        bbgr_parquet_read_player(pid, season_type = "regular", return = "tibble"),
        error = function(e) {
          cache_read_error <<- TRUE
          if (isTRUE(verbose)) {
            message("  Cache for ", pid, " could not be read (", conditionMessage(e), "). Forcing refresh…")
          }
          NULL
        }
      )
      if (cache_read_error) {
        regular_years_to_fetch <- years
        need_regular_scrape <- TRUE
      }
    }

    if (isTRUE(include_postseason) && !need_postseason_scrape) {
      post_cached <- tryCatch(
        bbgr_parquet_read_player(pid, season_type = "postseason", return = "tibble"),
        error = function(e) {
          cache_read_error <<- TRUE
          if (isTRUE(verbose)) {
            message("  Postseason cache for ", pid, " could not be read (", conditionMessage(e), "). Forcing refresh…")
          }
          NULL
        }
      )
      if (cache_read_error) {
        need_postseason_scrape <- TRUE
      }
    }

    if (!need_regular_scrape && !need_postseason_scrape && !cache_read_error) {
      if (is.null(reg_cached)) reg_cached <- tibble::tibble()
      if (isTRUE(include_postseason) && is.null(post_cached)) post_cached <- tibble::tibble()

      assign(pid, list(regular = reg_cached, postseason = post_cached), envir = bbgr_mem_cache())

      if (isTRUE(split_postseason_result) && isTRUE(include_postseason)) {
        out_list[[pid]] <- list(regular = reg_cached, postseason = post_cached)
      } else if (isTRUE(split_postseason_result) && !isTRUE(include_postseason)) {
        out_list[[pid]] <- reg_cached
      } else {
        if (isTRUE(include_postseason)) {
          parts <- Filter(function(x) !is.null(x), list(reg_cached, post_cached))
          combined_cached <- if (length(parts)) dplyr::bind_rows(parts) else tibble::tibble()
          out_list[[pid]] <- if (nrow(combined_cached)) .add_career_gcar(combined_cached) else combined_cached
        } else {
          out_list[[pid]] <- reg_cached
        }
      }
      next
    }

    #### Regular season: fetch per-year --------------------------------------- ####
    years_to_scrape <- regular_years_to_fetch
    reg_years <- vector("list", length(years_to_scrape))
    names(reg_years) <- as.character(years_to_scrape)

    total_steps <- length(years_to_scrape) + as.integer(need_postseason_scrape)
    if (verbose && interactive()) {
      message(
        "Scraping ", row_meta$Name, " (",
        length(years_to_scrape), " season", if (length(years_to_scrape) == 1) "" else "s",
        if (need_postseason_scrape) " + postseason" else "", ")..."
      )
      pb <- utils::txtProgressBar(min = 0, max = if (total_steps > 0) total_steps else 1, style = 3)
    } else {
      pb <- NULL
    }

    step <- 0L
    if (length(years_to_scrape)) {
      for (yr in years_to_scrape) {
        resp <- httr::GET(.gl_url(pid, yr, postseason = FALSE))
        if (httr::status_code(resp) == 200L) {
          df <- .parse_table(resp, yr, pid, row_meta, postseason = FALSE, verbose = FALSE)
          if (!is.null(df) && nrow(df)) reg_years[[as.character(yr)]] <- df
        }
        step <- step + 1L
        if (!is.null(pb)) utils::setTxtProgressBar(pb, step)
        Sys.sleep(sleep_sec + stats::runif(1, 0, jitter_sec))
      }
    }

    reg_new <- if (length(Filter(Negate(is.null), reg_years))) {
      dplyr::bind_rows(Filter(Negate(is.null), reg_years))
    } else NULL

    #### -- Postseason (optional) ------------------------------------------------####
    post_new <- NULL
    if (need_postseason_scrape) {
      resp <- httr::GET(.gl_url(pid, yr = 0, postseason = TRUE))
      if (httr::status_code(resp) == 200L) {
        post_new <- .parse_table(resp, yr = 0, pid, row_meta, postseason = TRUE, verbose = FALSE)
        if (!is.null(post_new) && nrow(post_new) == 0L) post_new <- NULL
      }
      step <- step + 1L
      if (!is.null(pb)) utils::setTxtProgressBar(pb, step)
      Sys.sleep(sleep_sec + stats::runif(1, 0, jitter_sec))
    }

    if (!is.null(pb)) close(pb)
    if (verbose) message("  Done: ", row_meta$Name, " (", pid, ")")

    if (!is.null(reg_new)  && nrow(reg_new))  bbgr_parquet_append(reg_new,  keep_cols = names(reg_new),  compression = compression)
    if (!is.null(post_new) && nrow(post_new)) bbgr_parquet_append(post_new, keep_cols = names(post_new), compression = compression)

    reg_final <- bbgr_parquet_read_player(pid, season_type = "regular", return = "tibble")
    if (is.null(reg_final)) reg_final <- tibble::tibble()
    post_final <- NULL
    if (isTRUE(include_postseason)) {
      post_final <- bbgr_parquet_read_player(pid, season_type = "postseason", return = "tibble")
      if (is.null(post_final)) post_final <- tibble::tibble()
    }

    assign(pid, list(regular = reg_final, postseason = post_final), envir = bbgr_mem_cache())

    if (isTRUE(split_postseason_result) && isTRUE(include_postseason)) {
      out_list[[pid]] <- list(regular = reg_final, postseason = post_final)
    } else if (isTRUE(split_postseason_result) && !isTRUE(include_postseason)) {
      out_list[[pid]] <- reg_final
    } else {
      if (isTRUE(include_postseason)) {
        parts <- Filter(function(x) !is.null(x), list(reg_final, post_final))
        combined <- if (length(parts)) dplyr::bind_rows(parts) else tibble::tibble()
        out_list[[pid]] <- if (nrow(combined)) .add_career_gcar(combined) else combined
      } else {
        out_list[[pid]] <- reg_final
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
