#' Canonical (stable) column order used when returning data
#' @keywords internal
.bbgr_canonical_cols <- c(
  # ids / meta
  "PlayerID","SeasonType","Year","Name","Country","From","To",
  # game identity
  "Date","Rk","Gcar","Gtm","Tm","Team","game_location","Opp","Result","Inngs",
  # counting stats
  "PA","AB","R","H","2B","3B","HR","RBI","BB","SO","TB","SB","CS","GIDP","HBP","SH","SF","ROE","IBB",
  # rates / advanced
  "BA","OBP","SLG","OPS","BAbip","aLI","WPA","acLI","cWPA","RE24","BOP","Pos"
)

#' @keywords internal
.bbgr_rate_cols <- c("BA","OBP","SLG","OPS","BAbip","aLI","WPA","acLI","cWPA","RE24")

#' @keywords internal
#' @noRd
bbgr_slim <- function(df, keep_cols) {
  if (!is.data.frame(df) || length(keep_cols) == 0) return(df)
  keep_cols <- intersect(keep_cols, names(df))
  df[keep_cols]
}

#' @keywords internal
#' @noRd
bbgr_mem_cache <- function() .bbg_graphs_cache

#' Normalize column types before writing to Parquet
#'
#' Arrow infers column schemas from the incoming data frame. Columns that are
#' entirely `NA` can accidentally be written as logical or string even though
#' the canonical schema expects numeric data. This helper coerces known columns
#' into stable types to prevent schema drift across yearly partitions.
#'
#' @keywords internal
#' @noRd
.bbgr_normalize_for_parquet <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) return(df)

  numeric_cols <- c(
    "Rk","Gcar","Gtm",
    "PA","AB","R","H","2B","3B","HR","RBI","SB","CS","BB","SO","TB","GIDP","HBP","SH","SF","ROE","IBB",
    "BAbip","aLI","WPA","acLI","cWPA","RE24","BOP"
  )
  integer_cols <- c("Year","From","To")
  character_cols <- c(
    "PlayerID","Name","Country","Team","Tm","game_location","Opp","Result","Inngs","Pos","SeasonType"
  )

  df <- df |>
    dplyr::mutate(dplyr::across(dplyr::any_of(numeric_cols), as.numeric)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(integer_cols), as.integer)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(character_cols), as.character))

  if ("Date" %in% names(df) && !inherits(df$Date, "Date")) {
    suppressWarnings(df$Date <- as.Date(df$Date))
  }

  df
}

#' Cached seasons available for a player/season type
#'
#' Small helper to inspect which "Year=..." partitions already exist on disk for
#' a given `PlayerID`. This allows higher-level functions to decide whether a
#' fresh scrape is actually needed.
#'
#' @keywords internal
#' @noRd
.bbgr_cached_years <- function(player_id, season_type = c("regular", "postseason")) {
  season_type <- match.arg(season_type)

  player_root <- file.path(bbgr_parquet_dir(), season_type, paste0("PlayerID=", player_id))
  if (!dir.exists(player_root)) return(integer())

  year_dirs <- dir(player_root, pattern = "^Year=", full.names = FALSE, recursive = FALSE)
  if (!length(year_dirs)) return(integer())

  years <- suppressWarnings(as.integer(sub("^Year=", "", year_dirs)))
  years <- years[!is.na(years)]

  sort(unique(years))
}

#' #' Cache directory used by bbgraphsR
#'
#' @description
#' Returns the path to the package cache directory (platform-specific),
#' typically under `rappdirs::user_cache_dir("bbgraphsR")`.
#'
#' @return A length-1 character vector with the cache directory path.
#' @seealso [bbgr_open_cache()], [bbgr_parquet_clear()]
#' @keywords internal
bbgr_parquet_dir <- local({
  dir <- NULL
  function(path = NULL) {
    if (!is.null(path)) {
      dir <<- normalizePath(path, mustWork = FALSE)
      if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    }
    if (is.null(dir)) {
      dir <<- file.path(rappdirs::user_cache_dir("bbgraphsR"), "players_parquet")
      if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    }
    dir
  }
}
)

#' Open the bbgraphsR cache folder in the system file browser
#'
#' Opens the folder returned by [bbgr_parquet_dir()] in your OS file manager.
#' @return Invisibly returns the cache directory path.
#' @export
bbgr_open_cache <- function() {
  path <- bbgr_parquet_dir()
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  utils::browseURL(path)
  invisible(path)
}

# Append player data to Parquet cache, in a subfolder for season type
# (partition by PlayerID and Year so reads are fast & simple)
#' @keywords internal
#' @noRd
bbgr_parquet_append <- function(df, keep_cols, compression = "zstd") {
  if (is.null(df) || !nrow(df)) return(invisible(NULL))

  # slim safely
  keep_cols <- intersect(keep_cols, names(df))
  df <- df[keep_cols]
  df <- .bbgr_normalize_for_parquet(df)

  if (!"SeasonType" %in% names(df)) {
    stop("Data frame must include a SeasonType column ('Regular' or 'Postseason').")
  }
  stype <- unique(df$SeasonType)
  if (length(stype) != 1) stop("Parquet append only supports one SeasonType per write.")
  subdir <- tolower(stype)  # "regular" or "postseason"

  cache_dir <- file.path(bbgr_parquet_dir(), subdir)
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  arrow::write_dataset(
    df,
    path = cache_dir,
    format = "parquet",
    partitioning = c("PlayerID", "Year"),   # <<< important
    existing_data_behavior = "overwrite",   # safe per (stype, player, year) partition
    compression = compression
  )

  invisible(NULL)
}

# Read cached Parquet for a player (supports regular, postseason, or both)
bbgr_parquet_read_player <- function(player_id,
                                     season_type = c("both", "regular", "postseason"),
                                     return = c("tibble", "dataset"),
                                     cols = NULL,
                                     enforce_canonical = TRUE) {
  season_type <- match.arg(season_type)
  return      <- match.arg(return)

  root <- bbgr_parquet_dir()

  # open one season type ("regular" or "postseason")
  read_one <- function(stype) {
    path <- file.path(root, tolower(stype), paste0("PlayerID=", player_id))
    if (!dir.exists(path)) return(NULL)

    # Let Arrow auto-detect hive-style partitions (PlayerID, Year)
    ds <- arrow::open_dataset(path)   # <- removed partitioning="hive"

    # Drop fantasy columns that vary by season/era (prevents schema conflicts)
    ds <- dplyr::select(ds, -dplyr::any_of(c("DFS(DK)", "DFS(FD)", "DFS_DK", "DFS_FD")))

    # Ensure PlayerID exists as a column (since we opened at PlayerID=... level)
    if (!("PlayerID" %in% names(ds))) {
      ds <- dplyr::mutate(ds, PlayerID = player_id)
    }

    # Optional column projection
    if (!is.null(cols)) {
      want <- intersect(cols, names(ds))
      if (length(want)) ds <- dplyr::select(ds, dplyr::all_of(want))
    }

    ds
  }

  reg_ds  <- if (season_type %in% c("both","regular"))     read_one("regular")    else NULL
  post_ds <- if (season_type %in% c("both","postseason"))  read_one("postseason") else NULL

  parts <- Filter(Negate(is.null), list(reg_ds, post_ds))
  if (!length(parts)) return(NULL)

  # Align schemas: restrict to common cols, then cast known rate cols to numeric
  .bbgr_rate_cols <- c("BA","OBP","SLG","OPS","BAbip","aLI","WPA","acLI","cWPA","RE24")
  if (length(parts) == 2L) {
    common_cols <- intersect(names(parts[[1]]), names(parts[[2]]))
    cast_numeric <- intersect(.bbgr_rate_cols, common_cols)
    parts <- lapply(parts, function(ds) {
      ds <- dplyr::select(ds, dplyr::all_of(common_cols))
      if (length(cast_numeric)) {
        ds <- dplyr::mutate(ds, dplyr::across(dplyr::all_of(cast_numeric), as.numeric))
      }
      ds
    })
  } else {
    # single dataset present: still cast rate cols if necessary
    only <- parts[[1]]
    cast_numeric <- intersect(.bbgr_rate_cols, names(only))
    if (length(cast_numeric)) {
      parts[[1]] <- dplyr::mutate(only, dplyr::across(dplyr::all_of(cast_numeric), as.numeric))
    }
  }

  # Union Arrow datasets safely
  ds_combined <- Reduce(dplyr::union_all, parts)

  # Optional canonical ordering
  if (isTRUE(enforce_canonical)) {
    .bbgr_canonical_cols <- c(
      "PlayerID","SeasonType","Year","Name","Country","From","To",
      "Date","Rk","Gcar","Gtm","Tm","Team","game_location","Opp","Result","Inngs",
      "PA","AB","R","H","2B","3B","HR","RBI","BB","SO","TB","SB","CS","GIDP","HBP","SH","SF","ROE","IBB",
      "BA","OBP","SLG","OPS","BAbip","aLI","WPA","acLI","cWPA","RE24","BOP","Pos"
    )
    keep <- intersect(.bbgr_canonical_cols, names(ds_combined))
    if (length(keep)) ds_combined <- dplyr::select(ds_combined, dplyr::all_of(keep))
  }

  if (return == "dataset") {
    ds_combined
  } else {
    out <- dplyr::collect(ds_combined)

  # (Optional) also drop fantasy columns in the materialized tibble
    out <- dplyr::select(out, -dplyr::any_of(c("DFS(DK)", "DFS(FD)", "DFS_DK", "DFS_FD")))

    # canonicalize order again post-collect (harmless if already done)
    if (isTRUE(enforce_canonical)) {
      .bbgr_canonicalize <- function(x) {
        keep <- intersect(.bbgr_canonical_cols, names(x))
        if (length(keep)) dplyr::select(x, dplyr::all_of(keep)) else x
      }
      out <- .bbgr_canonicalize(out)
    }

    # >>> add career-wide Gcar when combining both
    if (season_type == "both") {
      .add_career_gcar <- function(df) {
        if (!is.data.frame(df) || !nrow(df)) return(df)
        if (!all(c("PlayerID","Date") %in% names(df))) return(df)
        if (!inherits(df$Date, "Date")) suppressWarnings(df$Date <- as.Date(df$Date))

        if ("SeasonType" %in% names(df)) {
          df$SeasonType <- factor(df$SeasonType, levels = c("Regular","Postseason"), ordered = TRUE)
        }

        df |>
          dplyr::arrange(
            .data$PlayerID, .data$Date,
            dplyr::across(dplyr::any_of("SeasonType")),
            dplyr::coalesce(.data$Gcar, Inf),
            dplyr::coalesce(.data$Rk,  Inf)
          ) |>
          dplyr::group_by(.data$PlayerID) |>
          dplyr::mutate(Gcar_real = dplyr::row_number()) |>
          dplyr::ungroup() |>
          dplyr::relocate(Gcar_real, .after = Gcar)
      }

      out <- .add_career_gcar(out)
    }

    out
  }
}

# List players in the Parquet cache (robust)
bbgr_parquet_list <- function() {
  root <- bbgr_parquet_dir()
  if (!dir.exists(root)) return(tibble::tibble())

  # Find all parquet part files under regular/ and postseason/
  parts <- list.files(
    root,
    pattern   = "\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (!length(parts)) {
    return(tibble::tibble(PlayerID=character(), SeasonType=character(),
                          min_year=integer(), max_year=integer(), n_rows=integer()))
  }

  # helper to extract partition values from hive dirs like PlayerID=xxx/Year=yyyy
  parse_part <- function(p) {
    # split path, get SeasonType from first-level dir (regular/postseason)
    rel <- sub(paste0("^", normalizePath(root, mustWork = FALSE), "/?"), "", normalizePath(p))
    bits <- strsplit(rel, .Platform$file.sep, fixed = TRUE)[[1]]
    stype <- bits[1]                       # "regular" or "postseason"
    pid   <- sub("^PlayerID=", "", grep("^PlayerID=", bits, value = TRUE)[1])
    yr    <- sub("^Year=", "",     grep("^Year=",     bits, value = TRUE)[1])
    list(SeasonType = stype, PlayerID = pid, Year = suppressWarnings(as.integer(yr)))
  }

  # row count from parquet metadata (no data scan)
  row_count <- function(p) {
    pf <- arrow::ParquetFileReader$create(p)
    pf$metadata$num_rows
  }

  info <- lapply(parts, function(p) {
    key <- parse_part(p)
    c(key, list(rows = row_count(p)))
  })

  df <- tibble::tibble(
    SeasonType = vapply(info, `[[`, "", "SeasonType"),
    PlayerID   = vapply(info, `[[`, "", "PlayerID"),
    Year       = vapply(info, function(x) x[["Year"]], integer(1)),
    rows       = vapply(info, function(x) x[["rows"]], numeric(1))
  )

  df |>
    dplyr::group_by(SeasonType, PlayerID) |>
    dplyr::summarise(
      min_year = suppressWarnings(min(Year, na.rm = TRUE)),
      max_year = suppressWarnings(max(Year, na.rm = TRUE)),
      n_rows   = sum(rows, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::arrange(SeasonType, PlayerID)
}

#' Clear Parquet cache on disk
#'
#' @description
#' Deletes Parquet cache files under the package cache directory returned by
#' [bbgr_parquet_dir()]. If `player_id` is provided, only that player's folder
#' is removed; otherwise the entire Parquet cache is reset.
#'
#' @param player_id Character scalar or `NULL`. Player ID to clear. If `NULL`,
#'   clears the whole Parquet cache.
#' @param ask Logical. If `TRUE`, prompt for confirmation when `player_id` is `NULL`.
#'
#' @return Invisibly returns `TRUE` when the cache was cleared, `FALSE` if aborted.
#' @seealso [bbgr_parquet_dir()], [bbgr_open_cache()], [bbgr_clear_all()]
#' @export
bbgr_parquet_clear <- function(player_id = NULL, ask = TRUE) {
  root <- bbgr_parquet_dir()

  if (is.null(player_id) && isTRUE(ask)) {
    ans <- utils::askYesNo("This will DELETE ALL Parquet cache files. Continue?")
    if (is.na(ans) || !ans) {
      message("Aborted: Parquet cache not cleared.")
      return(invisible(FALSE))
    }
  }

  if (is.null(player_id)) {
    unlink(root, recursive = TRUE)
    dir.create(root, recursive = TRUE)
    return(invisible(TRUE))
  }

  path <- file.path(root, player_id)
  unlink(path, recursive = TRUE)
  invisible(TRUE)
}

#' Clear in-memory and Parquet caches
#'
#' @description
#' Clears the in-memory cache and the on-disk Parquet cache. If `player_ids`
#' is `NULL`, both caches are fully reset (with confirmation unless `ask = FALSE`).
#' If a vector of `player_ids` is provided, only those entries are cleared.
#'
#' @param player_ids Character vector of Player IDs to clear, or `NULL` for all.
#' @param ask Logical; ask for confirmation when clearing everything.
#'
#' @return Invisibly returns `TRUE` on success.
#' @seealso [bbgr_parquet_dir()], [bbgr_parquet_clear()], [bbgr_open_cache()]
#' @export
bbgr_clear_all <- function(player_ids = NULL, ask = TRUE) {
  if (is.null(player_ids) && isTRUE(ask)) {
    ans <- utils::askYesNo("This will DELETE the entire cache (memory + disk). Continue?")
    if (is.na(ans) || !ans) {
      message("Aborted: cache not cleared.")
      return(invisible(FALSE))
    }
  }

  if (is.null(player_ids)) {
    rm(list = ls(bbgr_mem_cache()), envir = bbgr_mem_cache())
    bbgr_parquet_clear(ask = FALSE) # already confirmed above
  } else {
    rm(list = intersect(player_ids, ls(bbgr_mem_cache())), envir = bbgr_mem_cache())
    for (pid in player_ids) bbgr_parquet_clear(pid, ask = FALSE)
  }

  invisible(TRUE)
}
