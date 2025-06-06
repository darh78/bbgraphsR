context("viz_rd")

library(bbgraphsR)

test_that("viz_rd errors with invalid team", {
  expect_error(viz_rd("XYZ", 2021))
})
