library(ggplot2)
library(mlbplotR)
library(plotly)
library(dplyr)
library(ggtext)


#### Getting the data and cleaning it ####

#### Get the players from Venezuelan and saves it as a dataframe in the environmernt, called `ven`
source("scripts/ven_players_fixed.R")

#### Get the game logs of the debut for each Venezuelan player
ven_debut_game <- ven |>
  mutate(To = From) |>
  get_career_game_logs() |>
  filter(Gcar == 1) |>
  arrange(desc(Date))

#### Create column with current teams so historical teams are compatible with {mlbplotr} package
ven_debut_game <- ven_debut_game |>
  mutate(Team_clean = case_when(
    # Current team mappings
    Team == "CHW" ~ "CWS",  # Chicago White Sox
    Team == "KCR" ~ "KC",   # Kansas City
    Team == "SDP" ~ "SD",   # San Diego
    Team == "SFG" ~ "SF",   # San Francisco
    Team == "TBR" ~ "TB",   # Tampa Bay
    Team == "WSN" ~ "WSH",  # Washington
    Team == "ARI" ~ "AZ",   # Arizona

    # Historical team mappings to current teams
    Team == "FLA" ~ "MIA",  # Florida Marlins → Miami Marlins
    Team == "MON" ~ "WSH",  # Montreal Expos → Washington Nationals
    Team == "CAL" ~ "LAA",  # California Angels → Los Angeles Angels
    Team == "TBD" ~ "TB",   # Tampa Bay Devil Rays → Tampa Bay Rays
    Team == "ANA" ~ "LAA",  # Anaheim Angels → Los Angeles Angels
    Team == "KCA" ~ "KC",   # Kansas City Athletics → Kansas City Royals
    Team == "NYG" ~ "SF",   # New York Giants → San Francisco Giants

    TRUE ~ Team
  ))

#### Summarize number of Venezuelan players that debuted by team, and list them ----
team_counts_complete <- ven_debut_game |>
  count(Team_clean, sort = TRUE) |>
  left_join(
    ven_debut_game |>
      group_by(Team_clean) |>
      summarise(Players = paste0(Name, " (", format(Date, "%Y-%m-%d"), ")", collapse = ", "), .groups = 'drop'),
    by = "Team_clean"
  )


#### Complete plot ----

# Create the plot
whole_plot <- team_counts_complete %>%
  ggplot(aes(x = reorder(Team_clean, n), y = n)) +
  geom_col(aes(fill = Team_clean), alpha = 0.95, width = 0.8) +
  scale_fill_mlb(type = "primary") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.15, size = 3.5, fontface = "bold",
            color = "grey20") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, x_axis_max),
                     breaks = seq(0, x_axis_max, 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0,
                              margin = margin(b = 5), color = "grey15"),
    plot.subtitle = element_text(size = 8, hjust = 0, color = "grey50",
                                 margin = margin(b = 15)),
    axis.text.y = element_mlb_logo(size = .5),  # Much larger team logos
    axis.text.x = element_text(size = 8, color = "grey30"),
    axis.title.x = element_text(size = 8, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 8, face = "bold", margin = margin(r = 15)),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.5),
    plot.background = element_rect(fill = "#fafafa", color = NA),
    panel.background = element_rect(fill = "#fafafa", color = NA),
    plot.margin = margin(25, 30, 20, 25)  # Reduced margins to give more space to chart
  ) +
  labs(
    title = "Debut de Venezolanos en MLB",
    subtitle = "Jugadores por franquicia",
    x = NULL,
    y = NULL,
    caption = sprintf("Equipos antiguos considerados en sus franquicias actuales.<br><b> Fuente:</b> Baseball-Reference. <b>Actualizado en:</b> %s", hoy)
  ) +
  theme(
    plot.caption = ggtext::element_markdown(
      size   = 7,
      color  = "grey60",
      hjust  = 1,
      margin = margin(t = 8)
    )
  )

# Function to save plots with square dimensions
save_reels_plots <- function(plot, width = 1800, height = 1920, dpi = 300) {
    filename <- paste0("venezuelan_mlb_debuts_all.png")

    ggsave(
      path = "output/",
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      units = "px",
      dpi = dpi,
      bg = "#fafafa",
      limitsize = FALSE  # Allow larger plots
    )

    cat("Saved:", filename, "\n")
}

print(whole_plot)

# Save all plots as square images
save_reels_plots(whole_plot)

#### Preparation for plotting in chunks ----

#### Calculate number of charts needed
total_teams <- nrow(team_counts_complete)
teams_per_chart <- 5
num_charts <- ceiling(total_teams / teams_per_chart)

# Find the maximum count for consistent x-axis
max_count <- max(team_counts_complete$n)
x_axis_max <- ceiling(max_count / 5) * 5  # Round up to nearest 5

#### Plotting ----

# Create list to store plots
plot_list <- list()

hoy <- format(Sys.Date(), "%d-%m-%Y")

# Generate each chart
for (i in 1:num_charts) {
  start_idx <- (i - 1) * teams_per_chart + 1
  end_idx <- min(i * teams_per_chart, total_teams)

  # Get data for current chart
  chart_data <- team_counts_complete %>%
    slice(start_idx:end_idx) %>%
    arrange(desc(n)) |>
    mutate(Team_clean = forcats::fct_reorder(Team_clean, n))

  # Create the plot
  current_plot <- chart_data %>%
    ggplot(aes(x = reorder(Team_clean, n), y = n)) +
    geom_col(aes(fill = Team_clean), alpha = 0.95, width = 0.8) +
    scale_fill_mlb(type = "primary") +
    coord_flip() +
    geom_text(aes(label = n), hjust = -0.15, size = 8, fontface = "bold",
              color = "grey20") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                       limits = c(0, x_axis_max),
                       breaks = seq(0, x_axis_max, 5)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0,
                                margin = margin(b = 5), color = "grey15"),
      plot.subtitle = element_text(size = 12, hjust = 0, color = "grey50",
                                   margin = margin(b = 15)),
      axis.text.y = element_mlb_logo(size = 1.2),  # Much larger team logos
      axis.text.x = element_text(size = 14, color = "grey30"),
      axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 15)),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.5),
      plot.background = element_rect(fill = "#fafafa", color = NA),
      panel.background = element_rect(fill = "#fafafa", color = NA),
      plot.margin = margin(25, 30, 20, 25)  # Reduced margins to give more space to chart
    ) +
    labs(
      title = "Debut de Venezolanos en MLB",
      subtitle = "Jugadores por franquicia",
      x = NULL,
      y = "Jugadores",
      caption = sprintf("Equipos antiguos considerados en sus franquicias actuales.<br><b> Fuente:</b> Baseball-Reference. <b>Actualizado en:</b> %s", hoy)
    ) +
    theme(
      plot.caption = ggtext::element_markdown(
        size   = 9,
        color  = "grey60",
        hjust  = 1,
        margin = margin(t = 8)
      )
    )

  plot_list[[i]] <- current_plot
}

# Function to save plots with square dimensions
save_square_plots <- function(plot_list, width = 1800, height = 1800, dpi = 300) {
  for (i in seq_along(plot_list)) {
    filename <- paste0("venezuelan_mlb_debuts_chart_", i, ".png")

    ggsave(
      path = "output/",
      filename = filename,
      plot = plot_list[[i]],
      width = width,
      height = height,
      units = "px",
      dpi = dpi,
      bg = "#fafafa",
      limitsize = FALSE  # Allow larger plots
    )

    cat("Saved:", filename, "\n")
  }
}

# Display all plots
for (i in seq_along(plot_list)) {
  print(plot_list[[i]])
  cat("\nChart", i, "- Teams:",
      paste(team_counts_complete$Team_clean[((i-1)*teams_per_chart + 1):min(i*teams_per_chart, total_teams)],
            collapse = ", "), "\n\n")
}

# Save all plots as square images
save_square_plots(plot_list)

# Print summary
cat("Summary:\n")
cat("- Total teams:", total_teams, "\n")
cat("- Charts created:", num_charts, "\n")
cat("- Teams per chart:", teams_per_chart, "\n")
cat("- X-axis range: 0 to", x_axis_max, "\n")
cat("- Image dimensions: 1800px x 1800px\n")



##### Accumulated debut over time ----

# Using the ven_debut_game dataframe already created, make a time series plot of cumulative debuts over time,
# grouped by Team and using the Years of the "Date" column as the x axis

ven_debut_time_series <- ven_debut_game %>%
  mutate(Year = as.numeric(format(Date, "%Y"))) %>%
  group_by(Year, Team) %>%
  summarise(Debuts = n(), .groups = 'drop') %>%
  arrange(Year) %>%
  group_by(Team) %>%
  mutate(Cumulative_Debuts = cumsum(Debuts)) %>%
  ungroup() %>%
  group_by(Year, Team_clean) %>%
  summarise(Debuts = sum(Debuts), .groups = 'drop') %>%
  arrange(Year) %>%
  group_by(Team_clean) %>%
  mutate(Cumulative_Debuts = cumsum(Debuts)) %>%
  ungroup()

# Plot the cummulative debut over time using a geom_line() using the Team as groups, and with colors from mlbplotr

debut_time_series_plot <- ven_debut_time_series_mapped  |>
  ggplot(aes(x = Year, y = Cumulative_Debuts, color = Team_clean)) +
  geom_line(linewidth = 1.2, alpha = 0.85) +
  #geom_point(size = 2, alpha = 0.85) +
  scale_color_mlb(type = "primary") +
  scale_x_continuous(breaks = seq(1950, 2025, by = 5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5,
                              margin = margin(b = 5), color = "grey15"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey50",
                                 margin = margin(b = 25)),
    axis.text.x = element_text(size = 11, color = "grey30"),
    axis.text.y = element_text(size = 11, color = "grey30"),
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 15)),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = "#fafafa", color = NA),
    panel.background = element_rect(fill = "#fafafa", color = NA),
    plot.margin = margin(25, 25, 20, 25)
  ) +
  labs(
    title = "Debut de Venezolanos en MLB a lo largo del tiempo",
    subtitle = "Acumulado de jugadores por franquicia desde el primer debut en MLB",
    x = "Año",
    y = "Jugadores Acumulados",
    color = "Franquicia",
    caption = "Equipos históricos incluídos en franquicias actuales | Logos & colores via mlbplotR"
  ) +
  theme(plot.caption = element_text(size = 10, color = "grey60", hjust = 1,
                                    margin = margin(t = 15)))
print(debut_time_series_plot)


# Add the mlbplotR logo at the end of each line
# Avoid the following error
# Error in geom_mlb_logo(data = ven_debut_time_series_mapped %>% group_by(Team_clean) %>%  :
#                          could not find function "geom_mlb_logo"

debut_time_series_plot_logos <- debut_time_series_plot +
  mlbplotR::geom_mlb_logos(
    data = ven_debut_time_series_mapped |>
      dplyr::group_by(Team_clean) |>
      dplyr::filter(Year == max(Year)) |>
      dplyr::ungroup(),
    aes(x = Year + 0.5,
        y = Cumulative_Debuts,
        team_abbr = Team_clean),   # <-- key aesthetic
    inherit.aes = FALSE,   # <-- do not inherit global aes
    width = 0.03,                  # <-- use width/height, not size
    nudge_y = 0.5,
    alpha = 0.75
  ) +
  coord_cartesian(xlim = c(1950, 2026))  # keep existing scale, just extend view


ends <- ven_debut_time_series_mapped %>%
  group_by(Team_clean) %>%
  filter(Year == max(Year)) %>%   # last year per team
  ungroup()

# 1) Compute repelled positions using a dummy plot
dummy <- ggplot(
  ends,
  aes(x = Year, y = Cumulative_Debuts, label = Team_clean)
) +
  geom_text_repel(
    # tune these to your liking
    box.padding   = 0.4,
    point.padding = 0.6,
    nudge_x       = 0.5,   # push labels a bit to the right
    nudge_y       = 0.3,
    max.overlaps  = Inf
  )

# Extract the placed label positions from ggrepel
pos <- ggplot_build(dummy)$data[[1]] |>
  as.data.frame() |>
  transmute(
    Team_clean = label,
    x_rep = x,
    y_rep = y
  )

# 2) Join repelled positions back to your endpoints
ends_rep <- ends |>
  left_join(pos, by = "Team_clean")


# 3) Your base plot of lines (whatever you already have)
p <- debut_time_series_plot

# 4) Add segments from original point -> repelled position (optional, like ggrepel does)
p <- p +
  geom_segment(
    data = ends_rep,
    aes(x = Year, y = Cumulative_Debuts, xend = x_rep, yend = y_rep),
    linewidth = 0.3,
    alpha = 0.5
  )

# 5) Draw *true-color* logos at the repelled coordinates
p <- p +
  mlbplotR::geom_mlb_logos(
    data = ends_rep,
    aes(x = x_rep, y = y_rep, team_abbr = Team_clean),
    width  = 0.03,   # size control (by width); adjust to taste
    alpha  = ifelse(Cumulative_Debuts > 20, 1, 0.75),
    inherit.aes = FALSE   # <-- do not inherit global aes
    # IMPORTANT: don’t map `color` here, or you’ll recolor the logos
  )

# (Optional) extend x-axis to give labels some breathing room
p <- p + expand_limits(x = max(ends$Year) + 1)

p
