
# Ensure package functions are available
if (!exists("mlb_fetch_highlights")) {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all()
  } else {
    stop("Please install devtools or install the package first")
  }
}

# Mock data for testing
create_mock_schedule <- function() {
  list(
    dates = list(
      list(
        date = "2025-09-10",
        games = list(
          list(
            gamePk = 12345,
            officialDate = "2025-09-10",
            teams = list(
              home = list(team = list(name = "Boston Red Sox")),
              away = list(team = list(name = "New York Yankees"))
            )
          )
        )
      )
    )
  )
}

create_mock_content <- function() {
  list(
    highlights = list(
      highlights = list(
        items = list(
          list(
            id = "highlight_1",
            title = "Rafael Devers home run",
            blurb = "Devers crushes a 2-run homer to left field",
            date = "2025-09-10T20:30:00Z",
            playbacks = list(
              list(
                name = "mp4Avc-1280x720",
                url = "https://example.com/video1.mp4"
              ),
              list(
                name = "hlsCloud", 
                url = "https://example.com/video1.m3u8"
              )
            )
          )
        )
      )
    )
  )
}

# Test helpers
expect_valid_highlights_df <- function(df) {
  expected_cols <- c("game_date", "home_team", "away_team", "title", 
                     "description", "published", "playback_name", "url")
  
  expect_s3_class(df, "data.frame")
  expect_true(all(expected_cols %in% names(df)))
  expect_true(nrow(df) >= 0)
  
  if (nrow(df) > 0) {
    expect_type(df$game_date, "character")
    expect_type(df$home_team, "character") 
    expect_type(df$away_team, "character")
    expect_type(df$title, "character")
    expect_type(df$url, "character")
  }
}
