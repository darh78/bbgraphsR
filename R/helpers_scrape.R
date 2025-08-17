# Internal: build game log URL
#' @keywords internal
#' @noRd
.gl_url <- function(pid, yr, postseason = FALSE) {
  if (postseason) {
    sprintf("https://www.baseball-reference.com/players/gl.fcgi?id=%s&t=b&year=0&post=1", pid)
  } else {
    sprintf("https://www.baseball-reference.com/players/gl.fcgi?id=%s&t=b&year=%d", pid, as.integer(yr))
  }
}

# Internal: parse game log page to tibble
#' @keywords internal
#' @noRd
.parse_table <- function(resp, yr, pid, row_meta, postseason = FALSE, verbose = FALSE) {
  doc  <- rvest::read_html(resp)
  node <- rvest::html_element(doc, css = "#div_players_standard_batting table#players_standard_batting")
  if (is.na(node)) return(NULL)

  # Extract and normalize
  df <- rvest::html_table(node, fill = TRUE)
  df <- .fix_colnames_from_datastat(node, df)
  df <- .drop_repeated_headers(df)
  df <- bbgr_normalize_gamelog(df)
  df <- .align_postseason_headers(df)

  # keep only real games (don’t rely on Date being present)
  df <- .keep_only_games(df)
  if (!nrow(df)) return(NULL)

  # Fill meta fields
  df$PlayerID   <- pid
  df$SeasonType <- if (postseason) "Postseason" else "Regular"

  # Year handling…
  if (postseason) {
    if ("Date" %in% names(df) && !all(is.na(df$Date))) {
      if (!inherits(df$Date, "Date")) {
        suppressWarnings(df$Date <- as.Date(df$Date, format = "%Y-%m-%d"))
      }
      df$Year <- as.integer(format(df$Date, "%Y"))
    } else {
      df$Year <- 0L
    }
  } else {
    df$Year <- as.integer(yr)
  }

  df$Name    <- row_meta$Name[1]
  df$Country <- row_meta$Country[1]
  df$From    <- as.integer(row_meta$From[1])
  df$To      <- as.integer(row_meta$To[1])

  df
}

# Internal: parse HTML table node -> clean df
#' @keywords internal
#' @noRd
.fix_colnames_from_datastat <- function(tbl_node, df) {
  th <- rvest::html_elements(tbl_node, xpath = ".//thead//tr[last()]//th")
  ds <- rvest::html_attr(th, "data-stat")
  if (length(ds) != ncol(df)) ds <- make.unique(rep("col", ncol(df)))
  old <- names(df)
  bad <- is.na(old) | old == ""
  old[bad] <- ds[bad]
  names(df) <- make.unique(old, sep = "_")
  df
}

# remove header rows BR repeats in <tbody>
#' @keywords internal
#' @noRd
.drop_repeated_headers <- function(df) {
  if ("Rk" %in% names(df))       df <- df[df$Rk != "Rk", , drop = FALSE]
  if ("date_game" %in% names(df)) df <- df[df$date_game != "Date", , drop = FALSE]
  df
}

# Helper: Make postseason table columns match regular-season columns
#' @keywords internal
#' @noRd
.align_postseason_headers <- function(df) {
  # Define canonical order (match your regular-season data)
  standard_cols <- c(
    "Rk","Gcar","Gtm","Date","Team","game_location","Opp","Result","Inngs","PA","AB","R","H",
    "2B","3B","HR","RBI","SB","CS","BB","SO","BA","OBP","SLG","OPS","TB","GIDP","HBP","SH","SF",
    "ROE","IBB","BAbip","aLI","WPA","acLI","cWPA","RE24","BOP","Pos"
  )

  # Ensure all expected columns exist
  for (mc in setdiff(standard_cols, names(df))) {
    df[[mc]] <- NA
  }

  # Reorder columns to match standard order first, then extras
  extra_cols <- setdiff(names(df), standard_cols)
  df <- df[, c(standard_cols, extra_cols), drop = FALSE]

  df
}

# Normalize game log numeric columns
#' @keywords internal
#' @noRd
bbgr_normalize_gamelog <- function(df) {
  # Trim whitespace from names
  names(df) <- trimws(names(df))

  # Known numeric/stat columns
  numeric_cols <- c(
    "Rk","Gcar","Gtm","PA","AB","R","H","2B","3B","HR","RBI","SB","CS","BB","SO",
    "BA","OBP","SLG","OPS","TB","GIDP","HBP","SH","SF","ROE","IBB","BAbip","aLI",
    "WPA","acLI","cWPA","RE24","BOP"
  )

  # Convert if column exists
  for (col in intersect(numeric_cols, names(df))) {
    suppressWarnings({
      df[[col]] <- as.numeric(df[[col]])
    })
  }

  # Ensure Date is Date class if it looks like a date
  if ("Date" %in% names(df) && !inherits(df$Date, "Date")) {
    # Try multiple formats just in case
    suppressWarnings({
      df$Date <- as.Date(df$Date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))
    })
  }

  df
}

#' Keep only actual game rows (drop headers/totals), robust to Date issues
#' @keywords internal
.keep_only_games <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) return(df)

  # Keep rows where Rk is numeric (game rows). Totals/headers usually have NA / non-numeric
  rk_num <- suppressWarnings(as.integer(as.character(df$Rk)))
  keep   <- !is.na(rk_num)

  # Extra sanity: if a table somehow lacks Rk, fall back to Opp looking like a team code
  if (!("Rk" %in% names(df))) {
    if ("Opp" %in% names(df)) {
      keep <- grepl("^[A-Z]{2,4}$", as.character(df$Opp))
    } else {
      keep <- rep(TRUE, nrow(df))  # last resort: keep all
    }
  }

  df[keep, , drop = FALSE]
}
