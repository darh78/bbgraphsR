
test_that("package setup works correctly", {
  # Test that main function exists and is callable
  expect_true(exists("mlb_fetch_highlights"))
  expect_true(is.function(mlb_fetch_highlights))
  
  # Test that utility functions exist
  expect_true(exists("safe_filename"))
  expect_true(is.function(safe_filename))
  
  # Test basic functionality of utility function
  expect_equal(safe_filename("Test Title"), "Test_Title")
})

test_that("function has correct parameters", {
  # Check function signature
  args <- names(formals(mlb_fetch_highlights))
  expected_args <- c("start_date", "end_date", "team", "player")
  
  expect_true(all(expected_args %in% args))
})
