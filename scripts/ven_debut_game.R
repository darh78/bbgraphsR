library(ggplot2)
library(mlbplotR)

# Get the dataframe with Venezuelan players, and saves it as a dataframe in the environmernt, called `ven`
source("scripts/ven_players_fixed.R")

# Get the logs of the debut game for each Venezuelan MLB player
ven_debut_game <- ven |>
  mutate(To = From) |>
  get_career_game_logs() |>
  filter(Gcar == 1) |>
  arrange(desc(Date))

# Examine original team counts
original_teams <- ven_debut_game |>
  count(Team, sort = TRUE)

# print(original_teams)
# cat("Total teams in original data:", nrow(original_teams))
# cat("\nTotal players:", sum(original_teams$n))

# Check valid team names in mlbplotR
valid_teams <- valid_team_names()
# print("Valid MLB team names in mlbplotR:")
# print(sort(valid_teams))

# Apply initial team mapping
teams_with_mapping <- ven_debut_game |>
  mutate(Team_clean = case_when(
    Team == "CHW" ~ "CWS",  # Chicago White Sox
    Team == "KCR" ~ "KC",   # Kansas City
    Team == "SDP" ~ "SD",   # San Diego
    Team == "SFG" ~ "SF",   # San Francisco
    Team == "TBR" ~ "TB",   # Tampa Bay
    Team == "WSN" ~ "WSH",  # Washington
    Team == "ARI" ~ "AZ",   # Arizona
    Team == "LAA" ~ "LAA",  # Keep LAA
    TRUE ~ Team
  )) |>
  count(Team, Team_clean, sort = TRUE)

# Identify teams that would be dropped
dropped_teams <- teams_with_mapping |>
  filter(!Team_clean %in% valid_team_names())

# print("Teams that would be dropped without historical mapping:")
# print(dropped_teams)

# cat("Players that would be lost:", sum(dropped_teams$n))

# Create comprehensive team mapping including historical teams
team_counts_complete <- ven_debut_game |>
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
  )) |>
  count(Team_clean, sort = TRUE)

# print("Complete team counts with historical mappings:")
# print(team_counts_complete)
# cat("Total players recovered:", sum(team_counts_complete$n))


# Show which teams gained players from historical mappings
historical_mapping <- ven_debut_game |>
  filter(Team %in% c("FLA", "MON", "CAL", "TBD", "ANA", "KCA", "NYG")) |>
  mutate(Team_clean = case_when(
    Team == "FLA" ~ "MIA",  # Florida → Miami
    Team == "MON" ~ "WSH",  # Montreal → Washington
    Team == "CAL" ~ "LAA",  # California → LAA
    Team == "TBD" ~ "TB",   # Tampa Bay Devil Rays → TB
    Team == "ANA" ~ "LAA",  # Anaheim → LAA
    Team == "KCA" ~ "KC",   # Kansas City Athletics → KC
    Team == "NYG" ~ "SF",   # NY Giants → SF
    TRUE ~ Team
  )) |>
  count(Team, Team_clean, sort = TRUE)

# print("Historical team mappings applied:")
# print(historical_mapping)

# Create the complete final plot with all players included
final_complete_plot <- team_counts_complete %>%
  ggplot(aes(x = reorder(Team_clean, n), y = n)) +
  geom_col(aes(fill = Team_clean), alpha = 0.95, width = 0.75) +
  scale_fill_mlb(type = "primary") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.1, size = 4.2, fontface = "bold",
            color = "grey20") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                     breaks = seq(0, 25, 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5,
                              margin = margin(b = 5), color = "grey15"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey50",
                                 margin = margin(b = 25)),
    axis.text.y = element_mlb_logo(size = 0.8),  # Team logos
    axis.text.x = element_text(size = 11, color = "grey30"),
    axis.title.x = element_text(size = 13, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 15)),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.5),
    plot.background = element_rect(fill = "#fafafa", color = NA),
    panel.background = element_rect(fill = "#fafafa", color = NA),
    plot.margin = margin(25, 25, 20, 25)
  ) +
  labs(
    title = "Debut de Venezolanos en MLB",
    subtitle = "Franquicias actuales dónde han debutado",
    x = "Franquicias",
    y = "Jugadores",
    caption = "Equipos históricos incluídos en franquicias actuales | Logos & colores via mlbplotR"
  ) +
  theme(plot.caption = element_text(size = 10, color = "grey60", hjust = 1,
                                    margin = margin(t = 15)))

print(final_complete_plot)

top_10_teams <- head(team_counts_complete, 10)
print("Top 10 teams with most Venezuelan player debuts:")
print(top_10_teams)

# Summary statistics
cat("Distribution Summary:")
cat("\n- Mean players per team:", round(mean(team_counts_complete$n), 1))
cat("\n- Median players per team:", median(team_counts_complete$n))
cat("\n- Range:", min(team_counts_complete$n), "to", max(team_counts_complete$n))
cat("\n- Teams with 20+ players:", sum(team_counts_complete$n >= 20))
cat("\n- Teams with 10+ players:", sum(team_counts_complete$n >= 10))
