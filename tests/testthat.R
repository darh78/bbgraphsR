
library(testthat)

# Try to load the package, with fallback for development
tryCatch({
  library(bbgraphsR)
}, error = function(e) {
  # If package not installed, load all functions from source
  message("Package not installed, loading from source...")
  devtools::load_all()
})

test_check("bbgraphsR")
