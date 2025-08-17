# User Guide — `get_career_game_logs`

This guide explains how to use the `get_career_game_logs()` function from the **bbgraphsR** package to retrieve MLB player career batting game logs.

------------------------------------------------------------------------

## 1. Purpose

`get_career_game_logs()` fetches game logs for one or more players from [Baseball-Reference.com](https://www.baseball-reference.com/), optionally including postseason games, and stores them in a Parquet cache for faster repeated use.

------------------------------------------------------------------------

## 2. Basic Usage

### Input data

The function requires a **metadata data frame** with at least these columns:

-   `Name` — player’s full name\
-   `From` — first season year\
-   `To` — last season year\
-   `PlayerID` — Baseball Reference player ID\
-   `Country` — player’s country

Example:

``` r
latin_500 <- data.frame( Name = "Miguel Cabrera", From = 2003,
To = 2023, PlayerID = "cabremi01", Country = "Venezuela" )
```
