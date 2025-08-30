# # data_statsapi <- sabRmetrics::download_statsapi(
# # start_date = "2024-09-01",
# # end_date = "2024-09-02"
# #  )
#
# # # You can also run this function with parallel computation if you want to download more games
# # cluster <- parallel::makeCluster(parallel::detectCores())
# # data_statsapi_cluster <- sabRmetrics::download_statsapi(
# #   start_date == "2024-09-01",
# #   end_date == "2024-09-02",
# #   cl == cluster
# # )
# # parallel::stopCluster(cluster)
#
#
# library(dplyr)
# library(tidyr)
# library(sabRmetrics)
#
# schedule_extrac <- sabRmetrics::extract_schedule("2024-05-01", "2024-05-30", level= "mlb", game_type = "R")
#
# schedule_long <- schedule_extrac %>%
#   pivot_longer(
#     cols        = c(team_id_away, team_name_away, score_away,
#                     team_id_home, team_name_home, score_home),
#     names_to    = c(".value","side"),
#     names_pattern = "(.+)_(away|home)"
#   ) %>%
#   rename(
#     team_id    = team_id,
#     team_name  = team_name,
#     runs_scored   = score,
#     runs_allowed  = score  # we'll fix this in Step 2
#   ) %>%
#   # Now runs_scored is correct; make runs_allowed by flipping depending on side:
#   mutate(
#     runs_allowed = if_else(side=="away", score_home, score_away)
#   ) %>%
#   select(game_id, date, team_id, team_name, runs_scored, runs_allowed)
#
# head(schedule_long)
#
#
#
