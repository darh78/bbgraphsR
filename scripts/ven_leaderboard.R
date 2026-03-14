# =============================================================================
# Venezuelan MLB Players — Season Leaderboard (Batting + Pitching)
#
# Pulls active Venezuelan players from Baseball Reference, fetches their
# game logs for the current season (using local Parquet cache for batting),
# and produces separate leaderboards for hitters and pitchers.
#
# Season timing logic:
#   - Players are identified as "active for SEASON" if their last recorded
#     season is >= SEASON - 1 (covers the gap before BB-Ref updates for
#     the new season).
#   - The minimum PA / IP filter is suppressed during the first two weeks
#     after Opening Day, so the leaderboard is still useful early in the
#     season (or before it starts).
#
# Run periodically — the "smart" cache strategy only re-downloads the
# current season for active players, so repeat runs are fast.
#
# Usage (from project root):
#   Rscript scripts/ven_leaderboard.R
# =============================================================================

# ── Configuration ─────────────────────────────────────────────────────────────
# Each parameter is only set if it hasn't already been defined by the caller.
# This allows overriding via:  SEASON <- 2025; source("scripts/ven_leaderboard.R")

if (!exists("COUNTRY"))     COUNTRY     <- "Venezuela"

# Target season. NULL = auto-detect from today's date.
if (!exists("SEASON"))      SEASON      <- NULL

# Cut-off date for game logs. NULL = full season to date.
# Set to a past date to produce a historical snapshot, e.g. "2025-09-15".
# When set, only games on or before this date are counted, and the timing
# logic (grace period, apply_filter) is evaluated against this date instead
# of today.
if (!exists("CUTOFF_DATE")) CUTOFF_DATE <- NULL

# Approximate Opening Day for SEASON. NULL = auto-derive as March 26 of SEASON.
# Override if the real Opening Day differs (e.g. international series).
if (!exists("OPENING_DAY")) OPENING_DAY <- NULL

# Filters — only applied after the first two weeks of the season
if (!exists("MIN_PA"))      MIN_PA      <- 30L
if (!exists("MIN_IP"))      MIN_IP      <- 5.0
if (!exists("GRACE_DAYS"))  GRACE_DAYS  <- 14L

if (!exists("SLEEP_SEC"))   SLEEP_SEC   <- 2
if (!exists("JITTER_SEC"))  JITTER_SEC  <- 0.5

if (!exists("SAVE_CSV"))    SAVE_CSV    <- TRUE
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE) && file.exists("DESCRIPTION")) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(bbgraphsR)
  }
  library(dplyr)
  library(rvest)
})

# ── Resolve config defaults ───────────────────────────────────────────────────
today        <- Sys.Date()
if (is.null(SEASON))     SEASON      <- as.integer(format(today, "%Y"))
if (is.null(OPENING_DAY)) OPENING_DAY <- as.Date(sprintf("%d-03-26", SEASON))
if (!is.null(CUTOFF_DATE)) CUTOFF_DATE <- as.Date(CUTOFF_DATE)

# Reference date drives timing logic: use CUTOFF_DATE when doing a
# historical snapshot, otherwise use today.
ref_date     <- if (!is.null(CUTOFF_DATE)) CUTOFF_DATE else today

# ── Season timing helpers ─────────────────────────────────────────────────────
season_live  <- ref_date >= OPENING_DAY
grace_over   <- ref_date >= (OPENING_DAY + GRACE_DAYS)
apply_filter <- season_live && grace_over

cutoff_label <- if (!is.null(CUTOFF_DATE)) sprintf(" through %s", CUTOFF_DATE) else ""

cat(sprintf(
  "\n=== %s MLB Players — %d Season Leaderboard%s ===\n",
  COUNTRY, SEASON, cutoff_label
))
cat(sprintf("Today: %s  |  Opening Day: %s\n", today, OPENING_DAY))
if (!is.null(CUTOFF_DATE)) cat(sprintf("Cutoff date: %s\n", CUTOFF_DATE))
if (!season_live) {
  cat("Season has not started yet — showing full rosters, no PA/IP filter.\n\n")
} else if (!grace_over) {
  cat(sprintf(
    "Within first %d days of season — PA/IP filter suspended.\n\n", GRACE_DAYS
  ))
} else {
  cat(sprintf("Filters active: min %d PA (batting) | min %.1f IP (pitching).\n\n",
              MIN_PA, MIN_IP))
}

# ── Step 1: Country roster ────────────────────────────────────────────────────
cat("Fetching player roster from Baseball Reference...\n")
all_players <- get_players_by_country(COUNTRY)
cat(sprintf("  Total %s players in BR: %d\n", COUNTRY, nrow(all_players)))

# Active = last recorded season >= SEASON - 1 (BB-Ref may not yet list SEASON)
active <- all_players |>
  filter(To >= SEASON - 1L)

cat(sprintf("  Active players for %d: %d\n\n", SEASON, nrow(active)))

if (nrow(active) == 0) stop("No active players found.")

# ── Step 2: Split into hitters vs pitchers ────────────────────────────────────
# BR bio page Pos column uses field-position codes: 1=P, 2=C, 3=1B, 4=2B, etc.
# Pitchers have "1" as their *primary* (first) position — anchored to string start.
#   "1", "*1", "1/H", "*1/3"  → primary pitcher (excluded from batting leaderboard)
#   "/1"                      → occasionally pitches; included in pitching scrape,
#                               kept in batting leaderboard if PA qualifies
is_primary_pitcher <- function(pos) {
  !is.na(pos) & grepl("^[*]?1(/|$)", trimws(pos))
}
is_any_pitcher <- function(pos) {
  !is.na(pos) & (grepl("^[*]?1(/|$)", trimws(pos)) |
                   grepl("(/|^)[*]?1(/|$)", trimws(pos)))
}

if ("Pos" %in% names(active)) {
  pitchers_meta  <- active |> filter(is_any_pitcher(Pos))   # scraped for pitching logs
  hitters_meta   <- active |> filter(!is_primary_pitcher(Pos))  # batting leaderboard
} else {
  # Fallback: treat everyone as a hitter; pitching section will be empty
  pitchers_meta  <- active[0, ]
  hitters_meta   <- active
}

cat(sprintf("  Batters (incl. two-way): %d | Pitchers to scrape: %d\n\n",
            nrow(hitters_meta), nrow(pitchers_meta)))

# ── Helper: safe sleep between requests ───────────────────────────────────────
polite_sleep <- function() Sys.sleep(SLEEP_SEC + stats::runif(1, 0, JITTER_SEC))

# ═══════════════════════════════════════════════════════════════════════════════
# BATTING LEADERBOARD
# ═══════════════════════════════════════════════════════════════════════════════
cat("──────────────────────────────────────────────────\n")
cat("BATTING — fetching game logs...\n")
cat("──────────────────────────────────────────────────\n")

bat_meta <- active |>         # include pitchers who also bat (NL / universal DH edge cases)
  mutate(From = SEASON, To = SEASON)

bat_logs_raw <- get_career_game_logs(
  metadata_df             = bat_meta,
  include_postseason      = FALSE,
  split_postseason_result = FALSE,
  sleep_sec               = SLEEP_SEC,
  jitter_sec              = JITTER_SEC,
  overwrite_cache         = TRUE,
  overwrite_scope         = "smart",
  verbose                 = FALSE
)

bat_logs <- bat_logs_raw |>
  filter(
    Year == SEASON,
    SeasonType == "Regular",
    is.null(CUTOFF_DATE) | as.Date(Date) <= CUTOFF_DATE
  ) |>
  mutate(across(
    c(PA, AB, R, H, `2B`, `3B`, HR, RBI, BB, SO,
      TB, SB, CS, HBP, SF, GIDP, IBB),
    as.numeric
  )) |>
  filter(!is.na(AB))

bat_board <- bat_logs |>
  group_by(Name, PlayerID) |>
  summarise(
    G    = n_distinct(Date),
    PA   = sum(PA,    na.rm = TRUE),
    AB   = sum(AB,    na.rm = TRUE),
    R    = sum(R,     na.rm = TRUE),
    H    = sum(H,     na.rm = TRUE),
    `2B` = sum(`2B`,  na.rm = TRUE),
    `3B` = sum(`3B`,  na.rm = TRUE),
    HR   = sum(HR,    na.rm = TRUE),
    RBI  = sum(RBI,   na.rm = TRUE),
    BB   = sum(BB,    na.rm = TRUE),
    SO   = sum(SO,    na.rm = TRUE),
    SB   = sum(SB,    na.rm = TRUE),
    CS   = sum(CS,    na.rm = TRUE),
    TB   = sum(TB,    na.rm = TRUE),
    HBP  = sum(HBP,   na.rm = TRUE),
    SF   = sum(SF,    na.rm = TRUE),
    IBB  = sum(IBB,   na.rm = TRUE),
    GIDP = sum(GIDP,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    AVG = ifelse(AB > 0, round(H / AB, 3), NA_real_),
    OBP = ifelse(
      (AB + BB + HBP + SF) > 0,
      round((H + BB + HBP) / (AB + BB + HBP + SF), 3),
      NA_real_
    ),
    SLG = ifelse(AB > 0, round(TB / AB, 3), NA_real_),
    OPS = round(OBP + SLG, 3)
  )

# Apply PA filter only after grace period
if (apply_filter) bat_board <- bat_board |> filter(PA >= MIN_PA)

# Remove pure pitchers with negligible plate appearances
bat_board <- bat_board |>
  filter(!(Name %in% pitchers_meta$Name & PA < 10)) |>
  arrange(desc(OPS)) |>
  mutate(Rank = row_number()) |>
  select(Rank, Name, G, PA, AB, R, H, `2B`, `3B`, HR, RBI,
         BB, SO, SB, CS, TB, HBP, IBB, GIDP, AVG, OBP, SLG, OPS)

# ═══════════════════════════════════════════════════════════════════════════════
# PITCHING LEADERBOARD
# ═══════════════════════════════════════════════════════════════════════════════
cat("\nPITCHING — fetching game logs...\n")
cat("──────────────────────────────────────────────────\n")

# Internal helper: parse one pitching game log page from BB-Ref (t=p)
.parse_pitching_page <- function(pid, yr, player_name) {
  url  <- sprintf(
    "https://www.baseball-reference.com/players/gl.fcgi?id=%s&t=p&year=%d",
    pid, as.integer(yr)
  )
  resp <- tryCatch(httr::GET(url), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200L) return(NULL)

  doc  <- tryCatch(rvest::read_html(resp), error = function(e) NULL)
  if (is.null(doc)) return(NULL)

  node <- rvest::html_element(
    doc, css = "#div_players_standard_pitching table#players_standard_pitching"
  )
  if (is.na(node) || length(node) == 0) return(NULL)

  df <- tryCatch(
    suppressWarnings(suppressMessages(rvest::html_table(node, fill = TRUE))),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Drop repeated header rows (BB-Ref inserts them every ~20 rows)
  if ("Rk" %in% names(df)) df <- df[suppressWarnings(!is.na(as.integer(df$Rk))), ]
  if (nrow(df) == 0) return(NULL)

  # Normalise every column to character so bind_rows() never hits type conflicts
  # across players. Numeric conversions are done explicitly in the aggregation step.
  df[] <- lapply(df, as.character)

  df$PlayerID <- pid
  df$Name     <- player_name
  df$Year     <- as.integer(yr)

  # Parse Date column so callers can apply a cutoff filter
  if ("Date" %in% names(df)) {
    suppressWarnings(
      df$Date <- as.Date(df$Date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))
    )
  }
  df
}

# Fetch pitching logs for all pitchers
pitch_rows <- list()
if (nrow(pitchers_meta) > 0) {
  for (i in seq_len(nrow(pitchers_meta))) {
    pid  <- pitchers_meta$PlayerID[i]
    nm   <- pitchers_meta$Name[i]
    cat(sprintf("  Scraping pitching logs: %s (%s)...\n", nm, pid))
    pg <- .parse_pitching_page(pid, SEASON, nm)
    if (!is.null(pg) && nrow(pg) > 0) pitch_rows[[pid]] <- pg
    polite_sleep()
  }
}

if (length(pitch_rows) > 0) {
  pitch_logs <- bind_rows(pitch_rows)

  # Apply cutoff date if set
  if ("Date" %in% names(pitch_logs)) {
    pitch_logs <- pitch_logs |>
      filter(is.null(CUTOFF_DATE) | as.Date(Date) <= CUTOFF_DATE)
  }

  # Convert numeric stat columns (everything was normalised to character)
  for (col in c("ER", "R", "H", "HR", "BB", "SO")) {
    if (col %in% names(pitch_logs))
      pitch_logs[[col]] <- suppressWarnings(as.numeric(pitch_logs[[col]]))
  }

  # Baseball IP uses X.1 / X.2 to mean 1 or 2 outs — NOT decimal fractions.
  # Convert to total outs for correct summation, then back to IP notation.
  ip_to_outs <- function(ip_chr) {
    x <- suppressWarnings(as.numeric(ip_chr))
    whole <- floor(x)
    outs  <- round((x - whole) * 10)   # e.g. 9.2 → 2 outs
    whole * 3L + outs
  }
  outs_to_ip <- function(total_outs) {
    floor(total_outs / 3) + (total_outs %% 3) * 0.1
  }
  if ("IP" %in% names(pitch_logs))
    pitch_logs$IP_outs <- ip_to_outs(pitch_logs$IP)

  # Derive W / L / SV / HLD from the Dec column.
  # BB-Ref formats decisions as "W(3-1)", "L(2-4)", "S(5)", "HD" etc.
  if ("Dec" %in% names(pitch_logs)) {
    dec <- trimws(as.character(pitch_logs$Dec))
    pitch_logs$W   <- as.integer(grepl("^W",  dec))
    pitch_logs$L   <- as.integer(grepl("^L",  dec))
    pitch_logs$SV  <- as.integer(grepl("^S",  dec))   # "S(n)" or "W-S"
    pitch_logs$HLD <- as.integer(grepl("^H",  dec))   # "HD" or "H(n)"
  } else {
    pitch_logs$W <- pitch_logs$L <- pitch_logs$SV <- pitch_logs$HLD <- 0L
  }

  # GS: 1 if pitcher started — Inngs is "GS-N" for starts, "PR" or numeric for relief
  if ("Inngs" %in% names(pitch_logs)) {
    pitch_logs$GS <- as.integer(grepl("^GS", trimws(pitch_logs$Inngs)))
  } else {
    pitch_logs$GS <- 0L
  }

  pitch_board <- pitch_logs |>
    group_by(Name, PlayerID) |>
    summarise(
      G        = n(),
      GS       = sum(GS,      na.rm = TRUE),
      W        = sum(W,       na.rm = TRUE),
      L        = sum(L,       na.rm = TRUE),
      SV       = sum(SV,      na.rm = TRUE),
      HLD      = sum(HLD,     na.rm = TRUE),
      IP_outs  = sum(IP_outs, na.rm = TRUE),
      H        = sum(H,       na.rm = TRUE),
      R        = sum(R,       na.rm = TRUE),
      ER       = sum(ER,      na.rm = TRUE),
      HR       = sum(HR,      na.rm = TRUE),
      BB       = sum(BB,      na.rm = TRUE),
      SO       = sum(SO,      na.rm = TRUE),
      .groups  = "drop"
    ) |>
    mutate(
      IP   = outs_to_ip(IP_outs),
      ERA  = ifelse(IP_outs > 0, round(ER  / IP_outs * 27, 2), NA_real_),
      WHIP = ifelse(IP_outs > 0, round((BB + H) / (IP_outs / 3), 3), NA_real_),
      K9   = ifelse(IP_outs > 0, round(SO  / IP_outs * 27, 2), NA_real_),
      BB9  = ifelse(IP_outs > 0, round(BB  / IP_outs * 27, 2), NA_real_),
      KBB  = ifelse(BB > 0, round(SO / BB, 2), NA_real_)
    ) |>
    select(-IP_outs)

  if (apply_filter) pitch_board <- pitch_board |> filter(IP >= MIN_IP)

  pitch_board <- pitch_board |>
    arrange(ERA) |>
    mutate(Rank = row_number()) |>
    select(Rank, Name, G, GS, W, L, SV, IP, H, R, ER, HR, BB, SO,
           ERA, WHIP, K9, BB9, KBB)
} else {
  pitch_board <- tibble(
    Rank = integer(), Name = character(), G = integer(), GS = integer(),
    W = integer(), L = integer(), SV = integer(), IP = numeric(),
    H = integer(), R = integer(), ER = integer(), HR = integer(),
    BB = integer(), SO = integer(), ERA = numeric(), WHIP = numeric(),
    K9 = numeric(), BB9 = numeric(), KBB = numeric()
  )
}

# ═══════════════════════════════════════════════════════════════════════════════
# PRINT RESULTS
# ═══════════════════════════════════════════════════════════════════════════════
options(width = 220)

filter_note <- if (apply_filter) {
  sprintf("min %d PA", MIN_PA)
} else {
  "no PA filter (early season)"
}

cat(sprintf(
  "\n\n╔══════════════════════════════════════════════════════════════╗\n"
))
cat(sprintf(
  "  %s MLB PLAYERS — %d  |  BATTING LEADERBOARD  (%s)\n",
  toupper(COUNTRY), SEASON, filter_note
))
cat(sprintf(
  "╚══════════════════════════════════════════════════════════════╝\n\n"
))
cat(sprintf("Qualifying batters: %d\n\n", nrow(bat_board)))
print(as.data.frame(bat_board), row.names = FALSE)

# Batting category leaders
show_leaders <- function(data, col, label, n = 5, fmt = "auto") {
  if (!col %in% names(data) || nrow(data) == 0) return(invisible(NULL))
  top <- data |> arrange(desc(.data[[col]])) |> head(n)
  cat(sprintf("\n%s:\n", label))
  for (i in seq_len(nrow(top))) {
    val <- top[[col]][i]
    val_str <- if (fmt == "auto") {
      if (!is.na(val) && val != floor(val)) sprintf("%.3f", val) else sprintf("%d", as.integer(val))
    } else {
      sprintf(fmt, val)
    }
    cat(sprintf("  %d. %-28s %s\n", i, top$Name[i], val_str))
  }
}

cat("\n--- BATTING CATEGORY LEADERS ---\n")
show_leaders(bat_board, "HR",  "Home Runs")
show_leaders(bat_board, "RBI", "RBI")
show_leaders(bat_board, "R",   "Runs Scored")
show_leaders(bat_board, "H",   "Hits")
show_leaders(bat_board, "SB",  "Stolen Bases")
show_leaders(bat_board, "BB",  "Walks")
show_leaders(bat_board, "AVG", "Batting Average")
show_leaders(bat_board, "OBP", "On-Base Percentage")
show_leaders(bat_board, "SLG", "Slugging Percentage")
show_leaders(bat_board, "OPS", "OPS")

# Pitching leaderboard
pitch_filter_note <- if (apply_filter) {
  sprintf("min %.1f IP", MIN_IP)
} else {
  "no IP filter (early season)"
}

cat(sprintf(
  "\n\n╔══════════════════════════════════════════════════════════════╗\n"
))
cat(sprintf(
  "  %s MLB PLAYERS — %d  |  PITCHING LEADERBOARD  (%s)\n",
  toupper(COUNTRY), SEASON, pitch_filter_note
))
cat(sprintf(
  "╚══════════════════════════════════════════════════════════════╝\n\n"
))
cat(sprintf("Qualifying pitchers: %d\n\n", nrow(pitch_board)))
print(as.data.frame(pitch_board), row.names = FALSE)

# Pitching category leaders (ERA: ascending; rest: descending)
show_leaders_asc <- function(data, col, label, n = 5) {
  if (!col %in% names(data) || nrow(data) == 0) return(invisible(NULL))
  top <- data |> arrange(.data[[col]]) |> head(n)
  cat(sprintf("\n%s:\n", label))
  for (i in seq_len(nrow(top))) {
    val <- top[[col]][i]
    val_str <- if (!is.na(val) && val != floor(val)) sprintf("%.2f", val) else sprintf("%d", as.integer(val))
    cat(sprintf("  %d. %-28s %s\n", i, top$Name[i], val_str))
  }
}

cat("\n--- PITCHING CATEGORY LEADERS ---\n")
show_leaders(pitch_board,     "W",   "Wins")
show_leaders(pitch_board,     "SO",  "Strikeouts")
show_leaders(pitch_board,     "SV",  "Saves")
show_leaders(pitch_board,     "IP",  "Innings Pitched", fmt = "%.1f")
show_leaders_asc(pitch_board, "ERA", "ERA (lowest)")
show_leaders_asc(pitch_board, "WHIP","WHIP (lowest)")
show_leaders(pitch_board,     "K9",  "K/9")
show_leaders(pitch_board,     "KBB", "K/BB ratio")

# ── Save CSVs ─────────────────────────────────────────────────────────────────
if (SAVE_CSV) {
  out_dir      <- "output"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  stamp        <- format(if (!is.null(CUTOFF_DATE)) CUTOFF_DATE else today, "%Y%m%d")
  slug         <- gsub(" ", "_", tolower(COUNTRY))

  bat_file   <- file.path(out_dir, sprintf("%s_batting_%d_%s.csv",  slug, SEASON, stamp))
  pitch_file <- file.path(out_dir, sprintf("%s_pitching_%d_%s.csv", slug, SEASON, stamp))

  write.csv(bat_board,   bat_file,   row.names = FALSE)
  write.csv(pitch_board, pitch_file, row.names = FALSE)
  cat(sprintf("\n\nBatting leaderboard  → %s\n", bat_file))
  cat(sprintf("Pitching leaderboard → %s\n",    pitch_file))
}

cat(sprintf(
  "\nRetrieved on: %s\n",
  format(lubridate::with_tz(Sys.time(), "US/Eastern"), "%Y-%m-%d %H:%M %Z")
))
