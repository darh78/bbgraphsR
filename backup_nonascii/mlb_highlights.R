# utils ----
`%||%` <- function(a, b) if (!is.null(a)) a else b
safe_filename <- function(x) gsub("[^A-Za-z0-9._-]+", "_", x)

have_ffmpeg <- function(ffmpeg = "ffmpeg") {
  res <- try(system2(ffmpeg, "-version", stdout = TRUE, stderr = TRUE), silent = TRUE)
  !inherits(res, "try-error")
}

#' @keywords internal
#' Remux HLS (.m3u8) to MP4 using ffmpeg (no re-encode).
hls_to_mp4 <- function(hls_url, outfile, ffmpeg = "ffmpeg") {
  if (!have_ffmpeg(ffmpeg)) stop("ffmpeg not found on PATH. Please install ffmpeg.")
  code <- system2(ffmpeg, c("-y", "-i", shQuote(hls_url), "-c", "copy", shQuote(outfile)))
  if (!file.exists(outfile) || code != 0) stop("ffmpeg failed to create MP4 from HLS.")
  normalizePath(outfile, winslash = "/")
}

# Helper function for safe API calls
safe_api_call <- function(url, description = "API call", timeout = 30) {
  tryCatch({
    r <- httr::GET(url,
                   httr::user_agent("R-mlb-highlights/1.2"),
                   httr::timeout(timeout))

    if (httr::http_error(r)) {
      status <- httr::status_code(r)
      if (status == 404) return(NULL)  # Missing content is common, not an error
      if (status >= 500) {
        warning("Server error (", status, ") for ", description, ": ", url)
        return(NULL)
      }
      warning("HTTP error (", status, ") for ", description)
      return(NULL)
    }

    content_text <- httr::content(r, "text", encoding = "UTF-8")
    if (nchar(content_text) == 0) return(NULL)

    jsonlite::fromJSON(content_text, simplifyVector = FALSE)
  }, error = function(e) {
    if (grepl("timeout", e$message, ignore.case = TRUE)) {
      warning(description, " timed out after ", timeout, " seconds")
    } else if (grepl("resolve host|network", e$message, ignore.case = TRUE)) {
      warning(description, " failed - check internet connection")
    } else {
      warning(description, " error: ", e$message)
    }
    return(NULL)
  })
}

# Team validation helper
validate_team_name <- function(team_input) {
  mlb_teams <- c(
    "Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", "Boston Red Sox",
    "Chicago Cubs", "Chicago White Sox", "Cincinnati Reds", "Cleveland Guardians",
    "Colorado Rockies", "Detroit Tigers", "Houston Astros", "Kansas City Royals",
    "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins", "Milwaukee Brewers",
    "Minnesota Twins", "New York Mets", "New York Yankees", "Oakland Athletics",
    "Philadelphia Phillies", "Pittsburgh Pirates", "San Diego Padres", "San Francisco Giants",
    "Seattle Mariners", "St. Louis Cardinals", "Tampa Bay Rays", "Texas Rangers",
    "Toronto Blue Jays", "Washington Nationals"
  )

  team_lower <- tolower(team_input)
  partial_matches <- mlb_teams[grepl(team_lower, tolower(mlb_teams))]

  if (length(partial_matches) == 0) {
    distances <- adist(team_lower, tolower(mlb_teams))
    closest <- mlb_teams[which.min(distances)]
    message("Team '", team_input, "' not found. Did you mean '", closest, "'?")
  } else if (length(partial_matches) > 1) {
    message("Found ", length(partial_matches), " teams matching '", team_input, "': ",
            paste(partial_matches[1:min(3, length(partial_matches))], collapse = ", "),
            if (length(partial_matches) > 3) "..." else "")
  }
  invisible(TRUE)
}

# fetching ----

#' Fetch MLB highlight video URLs for a team, filtered by player and (optionally) play description
#'
#' @description
#' Queries MLB Stats API for games in a date range (inclusive), filters to a team,
#' extracts highlight items, and returns a tidy data.frame of **playback URLs**.
#' Duplicates are removed and MP4s are preferred; HLS can be kept optionally.
#'
#' @param start_date Character or Date, "YYYY-MM-DD" (inclusive).
#' @param end_date Character or Date, "YYYY-MM-DD" (inclusive).
#' @param team Character. Team name (partial match, case-insensitive).
#' @param player Character or NULL. Player name (partial, case-insensitive). If NULL, returns all team highlights.
#' @param play_description Character, optional. Extra keyword(s) to match in title/description.
#' @param include_hls Logical. Keep HLS (`.m3u8`) rows too? Default `FALSE` (MP4 only).
#' @param keep_ids Logical. Keep `game_pk`/`item_id` columns? Default `FALSE`.
#' @param search_type Character. One of "contains" (default), "exact", or "regex" for player/description matching.
#' @param parallel Logical. Use parallel processing for multiple games? Default `NULL` (auto-detect).
#' @param progress Logical. Show progress indicators? Default `TRUE`.
#' @param timeout Numeric. Timeout in seconds for API calls. Default `30`.
#' @return `data.frame` with columns:
#'   `game_date`, `home_team`, `away_team`, `title`, `description`, `published`,
#'   `playback_name`, `url` (plus ids if `keep_ids = TRUE`).
#'   Includes attribute `query_info` with search parameters and results summary.
#' @export
mlb_fetch_highlights <- function(start_date, end_date,
                                 team, player = NULL,
                                 play_description = NULL,
                                 include_hls = FALSE,
                                 keep_ids = FALSE,
                                 search_type = c("contains", "exact", "regex"),
                                 parallel = NULL,
                                 progress = TRUE,
                                 timeout = 30) {

  # Input validation
  search_type <- match.arg(search_type)

  # Validate packages
  required_packages <- c("httr", "jsonlite")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop("Required packages not available: ", paste(missing_packages, collapse = ", "),
         "\nInstall with: install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))")
  }

  # Date validation
  to_date <- function(x) {
    if (inherits(x, "Date")) return(x)
    tryCatch({
      parsed <- as.Date(as.character(x), "%Y-%m-%d")
      if (is.na(parsed)) stop("Invalid date format")
      parsed
    }, error = function(e) {
      stop("Invalid date '", x, "'. Use format 'YYYY-MM-DD' or Date object.")
    })
  }

  sd <- to_date(start_date)
  ed <- to_date(end_date)

  if (sd > ed) stop("start_date must be <= end_date")
  if (sd > Sys.Date()) warning("start_date is in the future - no games may be available yet")

  # Validate team
  validate_team_name(team)

  # 1) Fetch schedule with better error handling
  if (progress) cat("Fetching game schedule for", team, "...\n")

  sched_url <- sprintf(
    "https://statsapi.mlb.com/api/v1/schedule?sportId=1&startDate=%s&endDate=%s",
    format(sd, "%Y-%m-%d"), format(ed, "%Y-%m-%d")
  )

  sch <- safe_api_call(sched_url, "Schedule fetch", timeout)
  if (is.null(sch)) {
    stop("Failed to fetch MLB schedule. Please check your internet connection and try again.")
  }

  # Extract games more efficiently
  rows <- list()
  if (length(sch$dates)) {
    for (d in sch$dates) {
      if (length(d$games)) {
        for (g in d$games) {
          rows[[length(rows) + 1L]] <- data.frame(
            game_pk = as.integer(g$gamePk),
            game_date = as.character(g$officialDate %||% d$date),
            home_team = as.character(g$teams$home$team$name),
            away_team = as.character(g$teams$away$team$name),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (!length(rows)) {
    stop("No MLB games found in the date range ", sd, " to ", ed)
  }

  sched_df <- do.call(rbind, rows)

  # 2) Filter by team with improved matching
  team_pattern <- switch(search_type,
                         "exact" = paste0("^", team, "$"),
                         "contains" = team,
                         "regex" = team
  )

  keep <- grepl(team_pattern, sched_df$home_team, ignore.case = TRUE) |
    grepl(team_pattern, sched_df$away_team, ignore.case = TRUE)
  sched_df <- sched_df[keep, , drop = FALSE]

  if (!nrow(sched_df)) {
    stop("No games matched team '", team, "' in the date range ", sd, " to ", ed,
         "\nTry using a partial team name like 'Red Sox' instead of 'Boston Red Sox'")
  }

  if (progress) {
    cat("Found", nrow(sched_df), "game(s) for", team, "\n")
  }

  # Determine parallel processing
  if (is.null(parallel)) {
    parallel <- nrow(sched_df) > 5 && requireNamespace("parallel", quietly = TRUE)
  }

  if (parallel && !requireNamespace("parallel", quietly = TRUE)) {
    warning("Parallel processing requested but 'parallel' package not available. Using sequential processing.")
    parallel <- FALSE
  }

  # Enhanced playback extraction
  pull_playbacks <- function(cnt, g) {
    if (is.null(cnt)) return(data.frame())

    containers <- list(
      cnt$highlights$highlights$items,
      cnt$highlights$live$items,
      cnt$editorial$highlight$items,
      cnt$editorial$recap$items,
      cnt$media$epg,
      cnt$highlights$gameDay$items
    )
    items <- Filter(Negate(is.null), containers)
    if (length(items)) items <- do.call(c, items) else items <- list()

    if (length(items) == 0) return(data.frame())

    extract_item <- function(it) {
      pbs <- it$playbacks %||% (it$media$playbacks %||% NULL)
      if (is.null(pbs)) return(NULL)

      # Enhanced metadata extraction with multiple fallbacks
      ttl <- it$title %||% it$headline %||% it$blurb %||% it$name %||% "Unknown"
      dt <- it$date %||% it$updated %||% it$pubDate %||% ""
      id <- it$id %||% it$guid %||% it$uid %||% ""
      desc <- it$blurb %||% it$seoTitle %||% it$headline %||% it$description %||% ""

      do.call(rbind, lapply(pbs, function(p) {
        data.frame(
          game_pk = g$game_pk,
          game_date = g$game_date,
          home_team = g$home_team,
          away_team = g$away_team,
          item_id = as.character(id),
          title = as.character(ttl),
          description = as.character(desc),
          published = as.character(dt),
          playback_name = as.character(p$name %||% ""),
          url = as.character(p$url %||% ""),
          stringsAsFactors = FALSE
        )
      }))
    }

    all_playbacks <- lapply(items, extract_item)
    valid_playbacks <- Filter(function(x) !is.null(x) && nrow(x) > 0, all_playbacks)

    if (length(valid_playbacks) == 0) return(data.frame())
    do.call(rbind, valid_playbacks)
  }

  # 3) Fetch content per game with progress and parallel support
  fetch_game_content <- function(game_row) {
    gp <- game_row$game_pk
    api_url <- sprintf("https://statsapi.mlb.com/api/v1/game/%s/content", gp)
    cnt <- safe_api_call(api_url, paste("Game", gp, "content"), timeout)
    pull_playbacks(cnt, game_row)
  }

  if (progress && nrow(sched_df) > 1) {
    pb <- txtProgressBar(min = 0, max = nrow(sched_df), style = 3)
    on.exit(if (exists("pb")) close(pb), add = TRUE)
  }

  if (parallel) {
    if (progress) cat("Using parallel processing for", nrow(sched_df), "games...\n")

    game_list <- lapply(seq_len(nrow(sched_df)), function(i) sched_df[i, , drop = FALSE])
    out <- parallel::mclapply(game_list, fetch_game_content,
                              mc.cores = min(parallel::detectCores() - 1, nrow(sched_df)))

  } else {
    out <- list()
    for (i in seq_len(nrow(sched_df))) {
      if (progress && exists("pb")) setTxtProgressBar(pb, i)

      game_row <- sched_df[i, , drop = FALSE]
      result <- fetch_game_content(game_row)

      if (nrow(result) > 0) {
        out[[length(out) + 1L]] <- result
      }
    }
  }

  # Process results
  valid_results <- Filter(function(x) !is.null(x) && nrow(x) > 0, out)

  if (!length(valid_results)) {
    if (progress) cat("\nNo highlights found for the specified criteria.\n")

    base_df <- data.frame(
      game_date = character(), home_team = character(), away_team = character(),
      title = character(), description = character(), published = character(),
      playback_name = character(), url = character(), stringsAsFactors = FALSE
    )

    attr(base_df, "query_info") <- list(
      start_date = as.character(sd), end_date = as.character(ed),
      team = team, player = player, play_description = play_description,
      games_searched = nrow(sched_df), highlights_found = 0
    )

    return(base_df)
  }

  df <- unique(do.call(rbind, valid_results))

  if (progress) cat("\nFound", nrow(df), "highlight playbacks before filtering\n")

  # 4) Apply filters with improved pattern matching
  create_search_pattern <- function(search_term, search_type) {
    if (is.null(search_term) || !nzchar(trimws(search_term))) return(NULL)

    switch(search_type,
           "exact" = paste0("\\b", gsub("([.*+?^${}()|[\\]\\\\])", "\\\\\\1", search_term), "\\b"),
           "contains" = gsub("([.*+?^${}()|[\\]\\\\])", "\\\\\\1", search_term),
           "regex" = search_term
    )
  }

  # Apply player filter
  if (!is.null(player) && nzchar(player)) {
    player_pattern <- create_search_pattern(player, search_type)
    if (!is.null(player_pattern)) {
      m <- grepl(player_pattern, df$title, ignore.case = TRUE) |
        grepl(player_pattern, df$description, ignore.case = TRUE) |
        grepl(player_pattern, df$playback_name, ignore.case = TRUE)
      df <- df[m, , drop = FALSE]

      if (nrow(df) == 0) {
        warning("No highlights found matching player '", player, "' with search type '", search_type, "'")
      }
    }
  }

  # Apply description filter
  if (!is.null(play_description) && nzchar(play_description)) {
    desc_pattern <- create_search_pattern(play_description, search_type)
    if (!is.null(desc_pattern)) {
      m <- grepl(desc_pattern, df$title, ignore.case = TRUE) |
        grepl(desc_pattern, df$description, ignore.case = TRUE)
      df <- df[m, , drop = FALSE]

      if (nrow(df) == 0) {
        warning("No highlights found matching description '", play_description, "' with search type '", search_type, "'")
      }
    }
  }

  if (nrow(df) == 0) {
    if (progress) cat("No highlights match the specified filters.\n")

    base_df <- data.frame(
      game_date = character(), home_team = character(), away_team = character(),
      title = character(), description = character(), published = character(),
      playback_name = character(), url = character(), stringsAsFactors = FALSE
    )

    attr(base_df, "query_info") <- list(
      start_date = as.character(sd), end_date = as.character(ed),
      team = team, player = player, play_description = play_description,
      games_searched = nrow(sched_df), highlights_found = 0
    )

    return(base_df)
  }

  # 5) Enhanced deduplication and preference logic
  is_mp4 <- grepl("\\.mp4($|\\?)", df$url, ignore.case = TRUE)

  # Improved playback ranking (lower = better)
  pref <- c("mp4Avc-1280x720", "mp4Avc-960x540", "mp4Avc-640x360", "mp4Avc-432x240", "mp4Avc",
            "HTTP_CLOUD_WIRED_60", "HTTP_CLOUD_WIRED", "hlsCloud")
  rank <- match(df$playback_name, pref)
  rank[is.na(rank)] <- 999

  # Enhanced URL normalization for better deduplication
  url_base <- sub("\\?.*$", "", df$url)
  url_path <- sub("^https?://[^/]+/", "", url_base)

  # Remove bitrate suffixes more comprehensively
  url_path_nobitrate <- gsub("-(\\d+K?|\\d+x\\d+)\\.(mp4|m3u8)$", ".\\2", url_path, perl = TRUE)

  # Remove common MLB asset suffixes
  clip_key <- gsub("-(csvm-diamondgcp-asset|filmroom-vr-alt)", "", url_path_nobitrate, perl = TRUE)

  # Sort by preference and deduplicate
  ord <- order(!is_mp4, rank, df$title, df$playback_name, df$url)
  df <- df[ord, , drop = FALSE]

  # Keep best rendition per clip
  keep_idx <- !duplicated(clip_key[ord])
  df <- df[keep_idx, , drop = FALSE]
  rownames(df) <- NULL

  # Filter HLS if requested
  if (!isTRUE(include_hls)) {
    before_count <- nrow(df)
    df <- df[grepl("\\.mp4($|\\?)", df$url, ignore.case = TRUE), , drop = FALSE]
    if (progress && before_count > nrow(df)) {
      cat("Filtered to MP4 only:", nrow(df), "of", before_count, "playbacks\n")
    }
  }

  # Select final columns
  keep_cols <- c("game_date", "home_team", "away_team", "title", "description", "published", "playback_name", "url")

  if (isTRUE(keep_ids)) {
    keep_cols <- c("game_pk", keep_cols)
  }

  df <- df[, intersect(keep_cols, names(df)), drop = FALSE]

  # Add metadata
  attr(df, "query_info") <- list(
    start_date = as.character(sd),
    end_date = as.character(ed),
    team = team,
    player = player,
    play_description = play_description,
    search_type = search_type,
    games_searched = nrow(sched_df),
    highlights_found = nrow(df),
    include_hls = include_hls,
    query_time = Sys.time()
  )

  if (progress) {
    cat("Final result:", nrow(df), "highlights\n")
  }

  df
}

# browsing/downloading ----

#' Interactively view or download highlight videos from a fetched table
#'
#' @description
#' Prints a clean, indexed list (no URLs), then lets the user select rows by index
#' to either open in a browser (MP4 works; HLS may require Safari/VLC) or download
#' to `./videos` by default. HLS rows are automatically converted to MP4 using
#' `ffmpeg` if available.
#'
#' @param highlights_df Data frame from `mlb_fetch_highlights()`.
#' @param action "view" or "download". If missing, a menu is shown.
#' @param rows Integer indices to act on. If `NULL`, a prompt is shown.
#' @param destdir Save directory. Defaults to `./videos` (created if needed).
#' @param max_display Maximum number of rows to display. Default 20.
#' @return Invisibly, the subset acted upon (with the same columns as input).
#' @export
mlb_browse_highlights <- function(highlights_df,
                                  action = c("view", "download"),
                                  rows = NULL,
                                  destdir = NULL,
                                  max_display = 20) {
  stopifnot(is.data.frame(highlights_df))

  if (!nrow(highlights_df)) {
    message("No highlights to browse.")
    return(invisible(highlights_df))
  }

  if (is.null(destdir)) destdir <- file.path(getwd(), "videos")
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)

  # Enhanced preview display
  view_cols <- intersect(c("game_date", "home_team", "away_team", "title", "playback_name"),
                         names(highlights_df))
  preview <- highlights_df[, view_cols, drop = FALSE]
  preview <- cbind(idx = seq_len(nrow(preview)), preview)

  # Truncate long titles for better display
  if ("title" %in% names(preview)) {
    preview$title <- ifelse(nchar(preview$title) > 50,
                            paste0(substr(preview$title, 1, 47), "..."),
                            preview$title)
  }

  # Display with pagination if needed
  total_rows <- nrow(preview)
  if (total_rows > max_display) {
    cat("Showing first", max_display, "of", total_rows, "highlights:\n\n")
    print(preview[1:max_display, ], row.names = FALSE)
    cat("\n... and", total_rows - max_display, "more rows\n")
  } else {
    print(preview, row.names = FALSE)
  }

  # Show summary info if available
  query_info <- attr(highlights_df, "query_info")
  if (!is.null(query_info)) {
    cat("\nQuery summary:")
    cat("\n  Date range:", query_info$start_date, "to", query_info$end_date)
    cat("\n  Team:", query_info$team)
    if (!is.null(query_info$player)) cat("\n  Player:", query_info$player)
    if (!is.null(query_info$play_description)) cat("\n  Description:", query_info$play_description)
    cat("\n  Games searched:", query_info$games_searched)
    cat("\n")
  }

  # Action selection with improved menu
  if (missing(action) || length(action) == 0L) {
    ch <- utils::menu(c("View in browser", "Download to disk"),
                      title = "Choose action:")
    action <- if (ch == 1) "view" else if (ch == 2) "download" else return(invisible(NULL))
  } else {
    action <- match.arg(action)
  }

  # Row selection with validation
  if (is.null(rows)) {
    if (total_rows == 1) {
      cat("\nPress Enter to select the only available row, or type 'q' to quit: ")
      ans <- readline()
      if (tolower(trimws(ans)) == "q") return(invisible(NULL))
      rows <- 1
    } else {
      cat("\nEnter row numbers (1-", total_rows, ") separated by commas (e.g. 1,3,5) or 'all' for all rows: ")
      ans <- readline()
      if (!nzchar(ans) || tolower(trimws(ans)) == "q") return(invisible(NULL))

      if (tolower(trimws(ans)) == "all") {
        rows <- seq_len(total_rows)
      } else {
        rows <- as.integer(strsplit(ans, "\\s*,\\s*")[[1]])
        rows <- rows[!is.na(rows)]
      }
    }
  }

  rows <- rows[rows >= 1 & rows <= nrow(highlights_df)]
  rows <- unique(rows)

  if (!length(rows)) {
    message("No valid rows selected.")
    return(invisible(NULL))
  }

  # Enhanced processing with better feedback
  if (length(rows) > 1) {
    cat("\nProcessing", length(rows), "highlights...\n")
  }

  for (i in rows) {
    ttl <- highlights_df$title[i]
    pb <- highlights_df$playback_name[i]
    u <- highlights_df$url[i]

    is_hls <- grepl("\\.m3u8($|\\?)", u, ignore.case = TRUE)
    is_mp4 <- grepl("\\.mp4($|\\?)", u, ignore.case = TRUE)

    if (identical(action, "view")) {
      cat("Opening:", substr(ttl, 1, 60), if (nchar(ttl) > 60) "..." else "", "\n")

      if (is_mp4) {
        utils::browseURL(u)
      } else if (is_hls) {
        message("Note: HLS (.m3u8) may not play in Chrome. Try Safari/VLC, or use Download to convert to MP4.")
        utils::browseURL(u)
      } else {
        utils::browseURL(u)
      }

    } else { # download
      fn <- paste0(safe_filename(ttl), "_", safe_filename(pb), ".mp4")
      path <- file.path(destdir, fn)

      cat("Downloading:", substr(ttl, 1, 50), if (nchar(ttl) > 50) "..." else "", "\n")

      if (is_mp4) {
        tryCatch({
          utils::download.file(u, destfile = path, mode = "wb", quiet = TRUE)
          cat("  Saved:", normalizePath(path, winslash = "/"), "\n")
        }, error = function(e) {
          warning("Download failed: ", e$message)
        })

      } else if (is_hls) {
        tryCatch({
          out <- hls_to_mp4(u, path)
          cat("  Saved (HLS→MP4):", out, "\n")
        }, error = function(e) {
          message("  Could not convert HLS. Install ffmpeg or open URL in Safari/VLC. Error: ", e$message)
        })

      } else {
        tryCatch({
          utils::download.file(u, destfile = path, mode = "wb", quiet = TRUE)
          cat("  Saved:", normalizePath(path, winslash = "/"), "\n")
        }, error = function(e) {
          warning("Download failed: ", e$message)
        })
      }
    }
  }

  if (identical(action, "download")) {
    cat("\nFiles saved to:", normalizePath(destdir, winslash = "/"), "\n")
  }

  invisible(highlights_df[rows, , drop = FALSE])
}
