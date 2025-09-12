#' Fetch MLB highlight video URLs for a team, filtered by player and (optionally) play description
#'
#' @description
#' Queries MLB Stats API for all games in a date range (inclusive), filters to games
#' involving a given team, extracts *highlight* items for each game, and returns a tidy
#' data.frame with available **playback URLs** (MP4 and HLS) for each highlight.
#'
#' @param start_date Character or Date. Start date in `"YYYY-MM-DD"` (inclusive).
#' @param end_date Character or Date. End date in `"YYYY-MM-DD"` (inclusive).
#' @param team Character. Team name to match (e.g. `"Boston Red Sox"`). Partial matches allowed.
#' @param player Character. Player name to filter highlights (e.g. `"Duran"`). Partial matches allowed.
#' @param play_description Character, optional. Additional keyword(s) to match in the highlight
#'   title/description (e.g. `"home run"`). Partial matches allowed.
#' @param include_hls Logical. If `FALSE` (default), prefer MP4s and drop HLS-only rows.
#' @return A `data.frame` with columns:
#'   `game_pk`, `game_date`, `home_team`, `away_team`, `item_id`, `title`,
#'   `published`, `playback_name`, `url`. One row per (highlight × playback rendition).
#'
#' @examples
#' \dontrun{
#' df <- mlb_fetch_highlights(
#'   start_date = "2025-09-10",
#'   end_date   = "2025-09-10",
#'   team       = "Boston Red Sox",
#'   player     = "Devers",
#'   play_description = "home run"
#' )
#' head(df)
#' }
#'
#' @export
mlb_fetch_highlights <- function(start_date,
                                 end_date,
                                 team,
                                 player,
                                 play_description = NULL,
                                 include_hls = FALSE) {
  # --- deps we use (keep package Imports lean) ---
  # @importFrom httr GET user_agent content stop_for_status
  # @importFrom jsonlite fromJSON
  # NAMESPACE: import these or call with :: in your package.
  requireNamespace("httr", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  # --- normalize dates ---
  to_date <- function(x) {
    if (inherits(x, "Date")) return(x)
    as.Date(as.character(x), format = "%Y-%m-%d")
  }
  sd <- to_date(start_date); ed <- to_date(end_date)
  stopifnot(!is.na(sd), !is.na(ed), sd <= ed)

  # --- 1) schedule for date range (all MLB, filter team by name) ---
  sched_url <- sprintf(
    "https://statsapi.mlb.com/api/v1/schedule?sportId=1&startDate=%s&endDate=%s",
    format(sd, "%Y-%m-%d"), format(ed, "%Y-%m-%d")
  )
  r <- httr::GET(sched_url, httr::user_agent("R-mlb-highlights/1.0"))
  httr::stop_for_status(r)
  sch <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  # Flatten schedule -> rows with gamePk, date, teams
  sch_rows <- list()
  if (!is.null(sch$dates) && length(sch$dates)) {
    for (d in sch$dates) {
      gms <- d$games
      if (length(gms)) {
        for (g in gms) {
          sch_rows[[length(sch_rows) + 1L]] <- data.frame(
            game_pk   = as.integer(g$gamePk),
            game_date = as.character(g$officialDate %||% d$date),
            home_team = as.character(g$teams$home$team$name),
            away_team = as.character(g$teams$away$team$name),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  sched_df <- if (length(sch_rows)) do.call(rbind, sch_rows) else {
    stop("No games found in the given date range.")
  }

  # --- 2) filter games by team (partial, case-insensitive) ---
  team_rx <- tolower(team)
  keep <- grepl(team_rx, tolower(sched_df$home_team)) | grepl(team_rx, tolower(sched_df$away_team))
  sched_df <- sched_df[keep, , drop = FALSE]
  if (!nrow(sched_df)) stop("No games matched the team '", team, "' in that date range.")

  # --- 3) for each game, fetch content & extract highlight playbacks ---
  extract_playbacks <- function(cnt, game_row) {
    # possible containers where items can live
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

    pull_rows <- function(it) {
      pbs <- it$playbacks %||% (it$media$playbacks %||% NULL)
      if (is.null(pbs)) return(NULL)
      ttl <- it$title %||% it$headline %||% it$blurb %||% it$name %||% NA_character_
      dt  <- it$date  %||% it$updated  %||% it$pubDate %||% NA_character_
      id  <- it$id    %||% it$guid     %||% it$uid    %||% NA_character_
      desc <- it$blurb %||% it$seoTitle %||% it$headline %||% NA_character_

      do.call(rbind, lapply(pbs, function(p) {
        data.frame(
          game_pk      = game_row$game_pk,
          game_date    = game_row$game_date,
          home_team    = game_row$home_team,
          away_team    = game_row$away_team,
          item_id      = as.character(id),
          title        = as.character(ttl),
          description  = as.character(desc),
          published    = as.character(dt),
          playback_name= as.character(p$name %||% NA),
          url          = as.character(p$url  %||% NA),
          stringsAsFactors = FALSE
        )
      }))
    }

    out <- do.call(rbind, Filter(Negate(is.null), lapply(items, pull_rows)))
    if (is.null(out)) out <- data.frame() else out
  }

  all_rows <- list()
  for (i in seq_len(nrow(sched_df))) {
    gp <- sched_df$game_pk[i]
    api_url <- sprintf("https://statsapi.mlb.com/api/v1/game/%s/content", gp)
    res <- httr::GET(api_url, httr::user_agent("R-mlb-highlights/1.0"))
    # some games may be missing content; skip cleanly
    if (httr::http_error(res)) next
    cnt <- try(jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE), silent = TRUE)
    if (inherits(cnt, "try-error")) next
    rows <- extract_playbacks(cnt, sched_df[i, , drop = FALSE])
    if (nrow(rows)) all_rows[[length(all_rows) + 1L]] <- rows
  }

  if (!length(all_rows)) {
    return(data.frame(
      game_pk=integer(), game_date=character(), home_team=character(), away_team=character(),
      item_id=character(), title=character(), description=character(), published=character(),
      playback_name=character(), url=character(), stringsAsFactors = FALSE
    ))
  }

  df <- unique(do.call(rbind, all_rows))

  # --- 4) filter by player and (optional) play description ---
  rx <- function(x) {
    if (is.null(x) || !nzchar(x)) return(NULL)
    paste0("(", x, ")")
  }
  rx_player <- rx(player)
  rx_desc   <- if (!is.null(play_description) && nzchar(play_description)) rx(play_description) else NULL

  if (!is.null(rx_player)) {
    keep <- grepl(rx_player, df$title, ignore.case = TRUE) |
      grepl(rx_player, df$description, ignore.case = TRUE) |
      grepl(rx_player, df$playback_name, ignore.case = TRUE)
    df <- df[keep, , drop = FALSE]
  }
  if (!is.null(rx_desc)) {
    keep <- grepl(rx_desc, df$title, ignore.case = TRUE) |
      grepl(rx_desc, df$description, ignore.case = TRUE)
    df <- df[keep, , drop = FALSE]
  }

  # --- 5) prefer MP4s (optionally drop HLS) & nice ordering ---
  is_mp4 <- grepl("\\.mp4($|\\?)", df$url, ignore.case = TRUE)
  pref <- c("mp4Avc-1280x720", "mp4Avc-640x360", "mp4Avc", "mp4Avc-432x240")
  rank <- match(df$playback_name, pref); rank[is.na(rank)] <- 999
  df <- df[order(!is_mp4, rank, df$title, df$published, df$playback_name), ]
  rownames(df) <- NULL
  if (!isTRUE(include_hls)) df <- df[is_mp4, , drop = FALSE]

  df
}


#' Interactively view or download highlight videos from a fetched table
#'
#' @description
#' Given the `data.frame` from `mlb_fetch_highlights()`, lets the user select rows and then:
#' - `action = "view"`: open the selected URLs in the default browser
#' - `action = "download"`: download the selected clips to `destdir`
#'
#' @param highlights_df A `data.frame` as returned by `mlb_fetch_highlights()`.
#' @param action Character. One of `"view"` or `"download"`. If missing, the user is prompted.
#' @param rows Integer vector of row indices to act on. If `NULL`, the user is prompted (comma-separated).
#' @param destdir Directory where files are saved when `action = "download"`. Defaults to current working dir.
#'
#' @return Invisibly, the subset `data.frame` of acted-upon rows.
#'
#' @examples
#' \dontrun{
#' df <- mlb_fetch_highlights("2025-09-10","2025-09-10","Boston Red Sox","Devers")
#' mlb_browse_highlights(df)                # prompt for action and rows
#' mlb_browse_highlights(df, "view", 1:3)   # open first 3 in browser
#' mlb_browse_highlights(df, "download", 1) # download first clip
#' }
#'
#' @export
mlb_browse_highlights <- function(highlights_df,
                                  action = c("view", "download"),
                                  rows = NULL,
                                  destdir = NULL) {
  stopifnot(is.data.frame(highlights_df))
  if (!nrow(highlights_df)) {
    message("No highlights to browse.")
    return(invisible(highlights_df))
  }

  # default save dir -> ./videos
  if (is.null(destdir)) destdir <- file.path(getwd(), "videos")
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)

  preview_cols <- intersect(c("game_date","home_team","away_team","title","playback_name","url"),
                            names(highlights_df))
  print(utils::head(highlights_df[preview_cols], 12))

  if (missing(action) || length(action) == 0L) {
    choice <- utils::menu(c("View in browser", "Download to disk"), title = "Choose action")
    action <- if (choice == 1) "view" else if (choice == 2) "download" else return(invisible(NULL))
  } else {
    action <- match.arg(action)
  }

  if (is.null(rows)) {
    cat("\nEnter row numbers to act on (comma-separated, e.g. 1,3,5): ")
    ans <- readline()
    if (!nzchar(ans)) return(invisible(NULL))
    rows <- as.integer(strsplit(ans, "\\s*,\\s*")[[1]])
    rows <- rows[rows >= 1 & rows <= nrow(highlights_df)]
  }
  sel <- unique(rows)
  if (!length(sel)) {
    message("No valid rows selected."); return(invisible(NULL))
  }

  # helper for safe filenames
  safe <- function(x) gsub("[^A-Za-z0-9._-]+", "_", x)

  for (i in sel) {
    ttl <- highlights_df$title[i]
    pb  <- highlights_df$playback_name[i]
    u   <- highlights_df$url[i]

    is_hls <- grepl("\\.m3u8($|\\?)", u, ignore.case = TRUE)
    is_mp4 <- grepl("\\.mp4($|\\?)",  u, ignore.case = TRUE)

    if (identical(action, "view")) {
      if (is_mp4) {
        utils::browseURL(u)                        # MP4 opens fine in browser
      } else if (is_hls) {
        message("Selected row is HLS (.m3u8). Try Safari/VLC, or choose Download to save as MP4.")
        utils::browseURL(u)                        # may work in Safari, not in Chrome
      } else {
        utils::browseURL(u)
      }
    } else { # download
      # choose extension by URL type
      ext <- if (is_mp4) ".mp4" else if (is_hls) ".mp4" else ".mp4"
      fn  <- paste0(safe(ttl), "_", safe(pb), ext)
      path <- file.path(destdir, fn)

      if (is_mp4) {
        utils::download.file(u, destfile = path, mode = "wb", quiet = FALSE)
        cat("Saved:", normalizePath(path, winslash = "/"), "\n")
      } else if (is_hls) {
        # HLS -> MP4 using ffmpeg
        tryCatch({
          out <- hls_to_mp4(u, path)
          cat("Saved (HLS→MP4):", out, "\n")
        }, error = function(e) {
          message("HLS download needs ffmpeg. Install it, or open the URL in Safari/VLC. Error: ", e$message)
        })
      } else {
        # unknown; try a straight download
        utils::download.file(u, destfile = path, mode = "wb", quiet = FALSE)
        cat("Saved:", normalizePath(path, winslash = "/"), "\n")
      }
    }
  }

  invisible(highlights_df[sel, , drop = FALSE])
}

#' @keywords internal
have_ffmpeg <- function(ffmpeg = "ffmpeg") {
  res <- try(system2(ffmpeg, "-version", stdout = TRUE, stderr = TRUE), silent = TRUE)
  !inherits(res, "try-error")
}

#' @keywords internal
#' Remux HLS (.m3u8) to MP4 using ffmpeg (no re-encode).
hls_to_mp4 <- function(hls_url, outfile, ffmpeg = "ffmpeg") {
  if (!have_ffmpeg(ffmpeg)) stop("ffmpeg not found on PATH. Please install ffmpeg.")
  # -y overwrite, -i input, -c copy = just remux segments into mp4 (fast)
  code <- system2(ffmpeg, c("-y", "-i", shQuote(hls_url), "-c", "copy", shQuote(outfile)))
  if (!file.exists(outfile) || code != 0) stop("ffmpeg failed to create MP4 from HLS.")
  normalizePath(outfile, winslash = "/")
}


#' Convenience wrapper: fetch highlights and then interactively view/download
#'
#' @description
#' Calls `mlb_fetch_highlights()` and then `mlb_browse_highlights()` to let the user
#' choose rows to view/download.
#'
#' @inheritParams mlb_fetch_highlights
#' @param action Passed to `mlb_browse_highlights()`. If missing, user is prompted.
#' @param include_hls Logical. From `mlb_fetch_highlights()`. If `FALSE`, keeps MP4s only.
#' @param destdir Directory for downloads (when `action = "download"`).
#'
#' @return Invisibly, the subset acted upon (see `mlb_browse_highlights()`).
#'
#' @examples
#' \dontrun{
#' mlb_fetch_highlights_interactive(
#'   start_date = "2025-09-10",
#'   end_date   = "2025-09-10",
#'   team       = "Boston Red Sox",
#'   player     = "Butler",
#'   play_description = NULL
#' )
#' }
#'
#' @export
mlb_fetch_highlights_interactive <- function(start_date,
                                             end_date,
                                             team,
                                             player,
                                             play_description = NULL,
                                             action,
                                             include_hls = FALSE,
                                             destdir = getwd()) {
  df <- mlb_fetch_highlights(start_date, end_date, team, player, play_description, include_hls)
  if (!nrow(df)) {
    message("No matching highlights found.")
    return(invisible(df))
  }
  mlb_browse_highlights(df, action = action, destdir = destdir)
}
