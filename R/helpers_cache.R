#' Slim a data frame to desired columns (safe drop)
#'
#' @param df A data frame or tibble.
#' @param keep_cols Character vector of column names to keep.
#' @return A data frame with only the specified columns (in order).
#' @export

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

# Default parquet cache dir
#' @keywords internal
#' @noRd
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
})

# Append player data to Parquet cache, in a subfolder for season type
# (partition by PlayerID and Year so reads are fast & simple)
#' @keywords internal
#' @noRd
bbgr_parquet_append <- function(df, keep_cols, compression = "zstd") {
  if (is.null(df) || !nrow(df)) return(invisible(NULL))

  # slim safely
  keep_cols <- intersect(keep_cols, names(df))
  df <- df[keep_cols]

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
                                     return = c("tibble", "dataset")) {
  season_type <- match.arg(season_type)
  return      <- match.arg(return)

  root <- bbgr_parquet_dir()

  read_one <- function(stype) {
    subdir <- file.path(root, tolower(stype))
    if (!dir.exists(subdir)) return(NULL)
    path <- file.path(subdir, paste0("PlayerID=", player_id))
    if (!dir.exists(path)) return(NULL)
    if (return == "dataset") {
      arrow::open_dataset(path)
    } else {
      arrow::read_parquet(path)
    }
  }

  if (season_type == "both") {
    reg  <- read_one("regular")
    post <- read_one("postseason")
    if (is.null(reg) && is.null(post)) return(NULL)
    if (return == "dataset") {
      # Datasets can be combined directly
      dplyr::bind_rows(list(reg, post))
    } else {
      # tibbles: bind only non-null
      dplyr::bind_rows(Filter(Negate(is.null), list(reg, post)))
    }
  } else {
    read_one(season_type)
  }
}

# List players in parquet cache
# List players in the Parquet cache (robust)
bbgr_parquet_list <- function() {
  root <- bbgr_parquet_dir()
  if (!dir.exists(root)) return(tibble::tibble())
  # NEW: if empty, return empty tibble instead of opening a dataset
  has_files <- length(list.files(root, pattern = "\\.parquet$", recursive = TRUE)) > 0L
  if (!has_files) return(tibble::tibble(PlayerID=character(), min_year=integer(),
                                        max_year=integer(), n_rows=integer()))

  ds <- arrow::open_dataset(root, partitioning = c("PlayerID","Year","SeasonType"), unify_schemas = TRUE)
  ds |>
    dplyr::group_by(PlayerID) |>
    dplyr::summarise(min_year = min(Year, na.rm = TRUE),
                     max_year = max(Year, na.rm = TRUE),
                     n_rows   = dplyr::n(),
                     .groups  = "drop") |>
    dplyr::arrange(PlayerID) |>
    dplyr::collect()
}

# Clear parquet cache
#' @export
bbgr_parquet_clear <- function(player_id = NULL) {
  root <- bbgr_parquet_dir()
  reg_dir  <- file.path(root, "regular")
  post_dir <- file.path(root, "postseason")

  if (is.null(player_id)) {
    unlink(root, recursive = TRUE, force = TRUE)
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    return(invisible(TRUE))
  }

  # Delete partition folders for this PlayerID in both season types
  paths <- c(
    file.path(reg_dir,  paste0("PlayerID=", player_id)),
    file.path(post_dir, paste0("PlayerID=", player_id))
  )
  for (p in paths) if (dir.exists(p)) unlink(p, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

# Clear both caches for player(s)
bbgr_clear_all <- function(player_ids = NULL) {
  if (is.null(player_ids)) {
    rm(list = ls(bbgr_mem_cache()), envir = bbgr_mem_cache())
    bbgr_parquet_clear()
  } else {
    rm(list = intersect(player_ids, ls(bbgr_mem_cache())), envir = bbgr_mem_cache())
    for (pid in player_ids) bbgr_parquet_clear(pid)
  }
}

# Open cache folder
bbgr_open_cache_dir <- function() {
  path <- bbgr_parquet_dir()
  utils::browseURL(path)
}
