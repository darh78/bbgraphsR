# bbgraphsR Caching System

This document describes how `bbgraphsR` caches Baseball-Reference game log data to **avoid unnecessary re-scraping** and to respect BR's polite usage guidelines.

## Overview

The package implements **two layers** of caching:

1.  **In-memory cache**
    -   Fast, session-local storage for data already fetched in the current R session.

    -   Lives in the `.bbg_graphs_cache` environment.

    -   Access via:

        ``` r
        bbgr_mem_cache()         # returns the environment
        ls(bbgr_mem_cache())     # see cached PlayerIDs
        ```

    -   Cleared automatically when the R session ends.
2.  **On-disk Parquet cache**
    -   Persistent cache between R sessions.

    -   Uses [Apache Parquet](https://parquet.apache.org/) for compact columnar storage.

    -   Partitioned by `PlayerID` and `Year` for fast lookups.

    -   Default location:

        ```         
        <user-cache-dir>/bbgraphsR/players_parquet/
        ```

        where `<user-cache-dir>` is platform-specific (see [`rappdirs::user_cache_dir()`](https://CRAN.R-project.org/package=rappdirs)).

## Why Parquet?

-   Smaller size than CSV or RDS for large tabular data.
-   Fast partial reads — load only the needed `PlayerID`/`Year` partitions.
-   Compatible with many tools beyond R (Python, Spark, etc.).

## Cache Control Functions

| Function | Purpose |
|------------------------------------------------------|------------------|
| `bbgr_mem_cache()` | Return in-memory cache environment. |
| `bbgr_parquet_dir()` | Get/set on-disk cache root folder. |
| `bbgr_parquet_list()` | List all cached players & years on disk. |
| `bbgr_parquet_read_player()` | Read cached data for a given PlayerID. |
| `bbgr_parquet_append()` | Append new data to disk cache. |
| `bbgr_parquet_clear()` | Remove cached data for a given PlayerID or all players. |

Example: \`\`\`r \# list cached players bbgr_parquet_list()

# clear a single player's cache

bbgr_parquet_clear("troutmi01")

# clear everything (careful!)

bbgr_parquet_clear()
