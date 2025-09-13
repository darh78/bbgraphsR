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
#' @param player Character. Player name (partial, case-insensitive).
#' @param play_description Character, optional. Extra keyword(s) to match in title/description.
#' @param include_hls Logical. Keep HLS (`.m3u8`) rows too? Default `FALSE` (MP4 only).
#' @param keep_ids Logical. Keep `game_pk`/`item_id` columns? Default `FALSE`.
#' @return `data.frame` with columns:
#'   `game_date`, `home_team`, `away_team`, `title`, `description`, `published`,
#'   `playback_name`, `url` (plus ids if `keep_ids = TRUE`).
#' @export
mlb_fetch_highlights <- function(start_date, end_date,
                                 team, player,
                                 play_description = NULL,
                                 include_hls = FALSE,
                                 keep_ids = FALSE) {
  requireNamespace("httr", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)

  to_date <- function(x) if (inherits(x, "Date")) x else as.Date(as.character(x), "%Y-%m-%d")
  sd <- to_date(start_date); ed <- to_date(end_date)
  stopifnot(!is.na(sd), !is.na(ed), sd <= ed)

  # 1) schedule
  sched_url <- sprintf(
    "https://statsapi.mlb.com/api/v1/schedule?sportId=1&startDate=%s&endDate=%s",
    format(sd, "%Y-%m-%d"), format(ed, "%Y-%m-%d")
  )
  r <- httr::GET(sched_url, httr::user_agent("R-mlb-highlights/1.1"))
  httr::stop_for_status(r)
  sch <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  rows <- list()
  if (length(sch$dates)) {
    for (d in sch$dates) {
      if (length(d$games)) {
        for (g in d$games) rows[[length(rows)+1L]] <- data.frame(
          game_pk   = as.integer(g$gamePk),
          game_date = as.character(g$officialDate %||% d$date),
          home_team = as.character(g$teams$home$team$name),
          away_team = as.character(g$teams$away$team$name),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  sched_df <- if (length(rows)) do.call(rbind, rows) else stop("No games in range.")

  # 2) team filter
  keep <- grepl(team, sched_df$home_team, TRUE) | grepl(team, sched_df$away_team, TRUE)
  sched_df <- sched_df[keep, , drop = FALSE]
  if (!nrow(sched_df)) stop("No games matched team '", team, "'.")

  # helpers to extract highlight playbacks
  pull_playbacks <- function(cnt, g) {
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

    one <- function(it) {
      pbs <- it$playbacks %||% (it$media$playbacks %||% NULL)
      if (is.null(pbs)) return(NULL)
      ttl <- it$title %||% it$headline %||% it$blurb %||% it$name %||% NA_character_
      dt  <- it$date  %||% it$updated  %||% it$pubDate %||% NA_character_
      id  <- it$id    %||% it$guid     %||% it$uid    %||% NA_character_
      desc<- it$blurb %||% it$seoTitle %||% it$headline %||% NA_character_

      do.call(rbind, lapply(pbs, function(p)
        data.frame(
          game_pk      = g$game_pk,
          game_date    = g$game_date,
          home_team    = g$home_team,
          away_team    = g$away_team,
          title        = as.character(ttl),
          description  = as.character(desc),
          playback_name= as.character(p$name %||% NA),
          url          = as.character(p$url  %||% NA),
          stringsAsFactors = FALSE
        )))
    }
    do.call(rbind, Filter(Negate(is.null), lapply(items, one)))
  }

  # 3) fetch content per game
  out <- list()
  for (i in seq_len(nrow(sched_df))) {
    gp <- sched_df$game_pk[i]
    api <- sprintf("https://statsapi.mlb.com/api/v1/game/%s/content", gp)
    rs  <- httr::GET(api, httr::user_agent("R-mlb-highlights/1.1"))
    if (httr::http_error(rs)) next
    cnt <- try(jsonlite::fromJSON(httr::content(rs, "text", encoding="UTF-8"), simplifyVector = FALSE), TRUE)
    if (inherits(cnt, "try-error")) next
    tbl <- pull_playbacks(cnt, sched_df[i, , drop = FALSE])
    if (nrow(tbl)) out[[length(out)+1L]] <- tbl
  }
  if (!length(out)) {
    base <- data.frame(game_date=character(), home_team=character(), away_team=character(),
                       title=character(), description=character(), playback_name=character(),
                       url=character(), stringsAsFactors = FALSE)
    return(base)
  }
  df <- unique(do.call(rbind, out))

  # 4) filters by player/description
  if (nzchar(player)) {
    m <- grepl(player, df$title, TRUE) | grepl(player, df$description, TRUE) | grepl(player, df$playback_name, TRUE)
    df <- df[m, , drop = FALSE]
  }
  if (!is.null(play_description) && nzchar(play_description)) {
    m <- grepl(play_description, df$title, TRUE) | grepl(play_description, df$description, TRUE)
    df <- df[m, , drop = FALSE]
  }

  # 5) dedupe & prefer MP4 — keep ONE rendition per logical clip
  is_mp4 <- grepl("\\.mp4($|\\?)", df$url, TRUE)

  # rank playback profiles (lower = better)
  pref <- c("mp4Avc-1280x720","mp4Avc-640x360","mp4Avc","mp4Avc-432x240",
            "HTTP_CLOUD_WIRED_60","HTTP_CLOUD_WIRED","hlsCloud")  # typical HLS names last
  rank <- match(df$playback_name, pref); rank[is.na(rank)] <- 999

  # normalize URL to a stable clip key (strip query, bitrate suffixes, and the common tail)
  url_base <- sub("\\?.*$", "", df$url)
  url_path <- sub("^https?://[^/]+/", "", url_base)

  # remove bitrate like "-4000K.mp4" or "-2000K.mp4", keep ".mp4" (or strip extension)
  url_path_nobitrate <- sub("-\\d+K\\.(mp4|m3u8)$", ".\\1", url_path, perl = TRUE)

  # drop the standard filmroom tail so mp4/hls map to the same key
  url_path_notail <- sub("-csvm-diamondgcp-asset(\\.mp4|\\.m3u8)$", "", url_path_nobitrate, perl = TRUE)

  clip_key <- url_path_notail

  # order by our preference, then pick the first row per clip_key
  ord <- order(!is_mp4,           # TRUE (not mp4) sorted after mp4
               rank,              # better profiles first
               df$title,          # stable tie-breakers
               df$playback_name,
               df$url)
  df <- df[ord, , drop = FALSE]

  # keep the best single rendition per clip
  keep_idx <- !duplicated(clip_key[ord])
  df <- df[keep_idx, , drop = FALSE]

  rownames(df) <- NULL

  # if user doesn't want HLS, drop remaining .m3u8 rows
  if (!isTRUE(include_hls)) df <- df[grepl("\\.mp4($|\\?)", df$url, TRUE), , drop = FALSE]

  keep_cols <- c("game_date","home_team","away_team","title","description","published","playback_name","url")

  if (isTRUE(keep_ids)) {
    # only keep game_pk, never item_id
    keep_cols <- c("game_pk", keep_cols)
  }

  df <- df[, intersect(keep_cols, names(df)), drop = FALSE]
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
#' @return Invisibly, the subset acted upon (with the same columns as input).
#' @export
mlb_browse_highlights <- function(highlights_df,
                                  action = c("view","download"),
                                  rows = NULL,
                                  destdir = NULL) {
  stopifnot(is.data.frame(highlights_df))

  if (!nrow(highlights_df)) {
    message("No highlights to browse."); return(invisible(highlights_df))
  }

  if (is.null(destdir)) destdir <- file.path(getwd(), "videos")
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)

  # Make a printed view WITHOUT URLs and with stable 1..N index
  view_cols <- intersect(c("game_date","home_team","away_team","title","playback_name"),
                         names(highlights_df))
  preview <- highlights_df[, view_cols, drop = FALSE]
  preview <- cbind(idx = seq_len(nrow(preview)), preview)

  # print all rows (or cap at, say, 30 if you prefer)
  print(preview, row.names = FALSE)

  # decide action
  if (missing(action) || length(action) == 0L) {
    ch <- utils::menu(c("View in browser", "Download to disk"), title = "Choose action")
    action <- if (ch == 1) "view" else if (ch == 2) "download" else return(invisible(NULL))
  } else action <- match.arg(action)

  # choose rows
  if (is.null(rows)) {
    cat("\nEnter row numbers to act on (comma-separated, e.g. 1,3,5): ")
    ans <- readline()
    if (!nzchar(ans)) return(invisible(NULL))
    rows <- as.integer(strsplit(ans, "\\s*,\\s*")[[1]])
  }
  rows <- rows[rows >= 1 & rows <= nrow(highlights_df)]
  rows <- unique(rows)
  if (!length(rows)) { message("No valid rows."); return(invisible(NULL)) }

  # act
  for (i in rows) {
    ttl <- highlights_df$title[i]
    pb  <- highlights_df$playback_name[i]
    u   <- highlights_df$url[i]

    is_hls <- grepl("\\.m3u8($|\\?)", u, TRUE)
    is_mp4 <- grepl("\\.mp4($|\\?)",  u, TRUE)

    if (identical(action, "view")) {
      if (is_mp4) {
        utils::browseURL(u)
      } else if (is_hls) {
        message("HLS (.m3u8): Chrome may not play it. Try Safari/VLC, or choose Download to save as MP4.")
        utils::browseURL(u)  # works in Safari/VLC; Chrome usually not
      } else {
        utils::browseURL(u)
      }
    } else { # download
      fn  <- paste0(safe_filename(ttl), "_", safe_filename(pb), ".mp4")
      path <- file.path(destdir, fn)

      if (is_mp4) {
        utils::download.file(u, destfile = path, mode = "wb", quiet = FALSE)
        cat("Saved:", normalizePath(path, winslash = "/"), "\n")
      } else if (is_hls) {
        tryCatch({
          out <- hls_to_mp4(u, path)
          cat("Saved (HLS→MP4):", out, "\n")
        }, error = function(e) {
          message("Could not convert HLS. Install ffmpeg or open the URL in Safari/VLC. Error: ", e$message)
        })
      } else {
        utils::download.file(u, destfile = path, mode = "wb", quiet = FALSE)
        cat("Saved:", normalizePath(path, winslash = "/"), "\n")
      }
    }
  }

  invisible(highlights_df[rows, , drop = FALSE])
}
