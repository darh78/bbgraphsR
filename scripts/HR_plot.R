# library(dplyr)
# library(ggplot2)
# library(mlbplotR)
# library(baseballr)
#
# # 0) Make sure HR is numeric and compute cumsum per player
# g_logs_hr500 <- latin_500hr_gamelogs |>
#   mutate(HR = as.integer(HR),
#          Gcar_real = as.integer(Gcar_real)) |>
#   group_by(PlayerID) |>
#   arrange(Gcar_real, .by_group = TRUE) |>
#   mutate(cumHR = cumsum(HR)) |>
#   ungroup()
#
# # 1) Endpoints (last game per player in your tibble)
# ends <- g_logs_hr500 |>
#   group_by(PlayerID) |>
#   slice_max(Gcar_real, n = 1, with_ties = FALSE) |>
#   ungroup() |>
#   mutate(
#     x_offset = Gcar_real + 50,   # push right
#     y_offset = cumHR + 10   # push upward
#   )
#
# # 2) Map BBRef -> MLBAM using Chadwick register
# cw <- baseballr::chadwick_player_lu() |>
#   select(key_bbref, key_mlbam, name_first, name_last)
#
# ends <- ends |>
#   left_join(cw, by = c("PlayerID" = "key_bbref")) |>
#   mutate(mlbam_id = as.integer(key_mlbam))
#
#
# # 3) Plot: lines + headshots at endpoints
# hr_comparison <-
#   ggplot() +
#   geom_line(
#     data = g_logs_hr500,
#     aes(x = Gcar_real, y = cumHR, group = PlayerID, color = PlayerID),
#     linewidth = 1.2, alpha = 0.7
#   ) +
#   mlbplotR::geom_mlb_headshots(
#     data = ends,
#     aes(x = x_offset, y = y_offset, player_id = mlbam_id),
#     width = 0.05,
#     alpha = 1,
#     show.legend = TRUE
#   ) +
#   labs(x = "Career game number", y = "Cumulative HR",
#        title = "Cumulative career HR for Latin players",
#        subtitle = ">= 500 HR") +
#   theme_minimal(base_size = 12) +
#   theme(legend.position = "none")
#
# print(hr_comparison)
