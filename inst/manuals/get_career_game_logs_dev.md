------------------------------------------------------------------------

## **`get_career_game_logs_dev.md`**

\`\``markdown # Developer Guide —`get_career_game_logs\`

This document explains the internal workings of `get_career_game_logs()` and related helpers in **bbgraphsR**.

------------------------------------------------------------------------

## 1. Overview

`get_career_game_logs()` orchestrates: 1. Reading player metadata 2. Downloading game logs (regular + optional postseason) 3. Parsing HTML tables into tidy tibbles 4. Storing data in Parquet cache 5. Returning results to the user

------------------------------------------------------------------------

## 2. Function Flow

### `get_career_game_logs()`

-   **Inputs:**
    -   `metadata_df`: data frame with Name, From, To, PlayerID, Country
    -   `include_postseason`: fetch postseason logs if TRUE
    -   `split_postseason_result`: return as nested list if TRUE, else bind regular seasons
    -   `sleep_sec`, `jitter_sec`: polite scraping delays
    -   `overwrite_cache`: skip reading cache and force re-download
    -   `compression`: Parquet compression codec
    -   `verbose`: print progress
-   **Loop per player:**
    1.  Fetch **regular season** logs (loop over each year from `From` to `To`)
    2.  Optionally fetch **postseason** logs (one page)
    3.  Append each part separately to Parquet (`bbgr_parquet_append()`)
    4.  Store in in-memory cache via `bbgr_mem_cache()`
    5.  Add to return list

------------------------------------------------------------------------

## 3. Key Helpers

### HTML Table Processing

-   `.gl_url(pid, year, postseason)` — build Baseball Reference URL
-   `.parse_table()` — parse HTML into tibble, adds meta columns
-   `.fix_colnames_from_datastat()` — normalize column names
-   `.drop_repeated_headers()` — remove extra header rows
-   `bbgr_normalize_gamelog()` — standardizes types (numeric, character)

------------------------------------------------------------------------

### Postseason Handling

-   Postseason tables have slightly different headers → normalized via `.align_postseason_headers()`
-   Regular and postseason kept in **separate cache subfolders** to avoid type conflicts

------------------------------------------------------------------------

### Caching

-   `bbgr_parquet_append(df, keep_cols, compression)`
    -   Writes dataset to cache in `bbgr_parquet_dir()/[season_type]`
    -   Uses `arrow::write_dataset()`
    -   Restricts write to one `SeasonType` at a time
-   `bbgr_parquet_read_player(pid)` — reads player’s cached data
-   `bbgr_parquet_list()` — lists all cached players

------------------------------------------------------------------------

### Helpers for Output

-   `logs_list_to_tibble(list, season_type)`
    -   Takes the `get_career_game_logs()` list output and binds all `regular` or `postseason` logs into one tibble.

------------------------------------------------------------------------

## 4. Suggested File Organization

For clarity, helpers can be split logically:

-   **`R/get_career_game_logs.R`**
    -   Main function
-   **`R/helpers_scrape.R`**
    -   `.gl_url()`
    -   `.parse_table()`
    -   `.fix_colnames_from_datastat()`
    -   `.drop_repeated_headers()`
    -   `bbgr_normalize_gamelog()`
-   **`R/helpers_cache.R`**
    -   `bbgr_parquet_append()`
    -   `bbgr_parquet_read_player()`
    -   `bbgr_parquet_list()`
    -   `bbgr_mem_cache()`
-   **`R/helpers_output.R`**
    -   `logs_list_to_tibble()`

------------------------------------------------------------------------

## 5. Developer Notes

-   **Data typing:** Baseball Reference sometimes changes column types (e.g., numeric BA in regular season vs. character in postseason). Keeping separate datasets avoids `bind_rows()` conflicts.
-   **Meta columns:** Always appended in `.parse_table()`:
    -   `PlayerID`, `Year`, `SeasonType`, `Name`, `Country`, `From`, `To`
-   **Politeness:** Respect scraping delays to prevent blocking.
