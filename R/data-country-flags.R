#' Country Flag URLs for Use in MLB Debut Visualizations
#'
#' A dataset mapping country names to 40px PNG flag images hosted on flagcdn.com.
#' Intended for use with functions like `viz_debut_running_total()`.
#'
#' @format A data frame with 2 columns:
#' \describe{
#'   \item{Country}{Character: country name as it appears in the player data}
#'   \item{FlagURL}{Character: URL to a 40px PNG flag image}
#' }
#'
#' @source \url{https://flagcdn.com}
"country_flags"

# Create the data (used once)
country_flags <- data.frame(
  Country = c("Dominican Republic", "Venezuela", "Cuba", "Puerto Rico", "Japan", "Mexico", "Panama", "Colombia"),
  FlagURL = c(
    "https://flagcdn.com/w40/do.png",
    "https://flagcdn.com/w40/ve.png",
    "https://flagcdn.com/w40/cu.png",
    "https://flagcdn.com/w40/pr.png",
    "https://flagcdn.com/w40/jp.png",
    "https://flagcdn.com/w40/mx.png",
    "https://flagcdn.com/w40/pa.png",
    "https://flagcdn.com/w40/co.png"
  ),
  stringsAsFactors = FALSE
)
