library(dplyr)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggplot2)
library(ggmapinset)
library(sf)
library(cowplot)
library(ggrepel)

# Get the dataframe with Venezuelan players
source("scripts/ven_players_fixed.R")

# Make a summary of players by State

estados <- ven |>
  group_by(State) |>
  summarise(Jugadores = n(),
            Ultimo_debut = max(Debut, na.rm = TRUE)) |>
  left_join(
    ven  |>
      filter(!is.na(Debut))  |>                       # ignore missing dates
      group_by(State) |>
      slice_max(Debut, n = 1, with_ties = FALSE)  |>  # row with the most recent date
      summarise(Jugador = first(Name), .groups = "drop"),
    by = "State"
  )  |>
  mutate(Hace_dias = as.numeric(Sys.Date() - Ultimo_debut))  |>
  arrange(desc(Jugadores)
          )


# Venezuela's map
ven_map <- ne_states(country = "venezuela", returnclass = "sf") |>
  select(name, geometry) |>
  filter(!is.na(name))

# Update the name of Vargas for LA Guaira
ven_map[ven_map$name == "Vargas", "name"] <- "La Guaira"

# Join the data per States to the map
ven_players_map <- ven_map |>
  left_join(estados, by = c("name" = "State")) |>
  rename("Estado" = "name")

# Filter for the small states around Caracas
caracas_region <- ven_map  |>
  filter(name %in% c("Distrito Capital", "Miranda", "La Guaira", "Aragua", "Carabobo"))

## 1) Union of Caracas states
caracas_union   <- st_union(caracas_region)
caracas_centre  <- st_centroid(caracas_union)

# 2) Inset config — 100 km circle, zoom 4, small nudge right/down
inset_cfg <- configure_inset(
  shape = shape_circle(
    centre = caracas_centre,
    radius = 200000              # 100 km in meters
  ),
  scale = 2,                     # zoom factor
  translation = c(550000, 600000),  # adjust placement (in meters)
  crs_working = 3857             # do inset geometry in meters
)

# 3) Plot Venezuela map + inset
ggplot(ven_players_map) +
  geom_sf_inset(aes(fill = Jugadores),
                color = "gray30", size = 0.8,
                map_base  = "normal", map_inset = "normal") +
  geom_inset_frame() +
  # geom_inset_link(colour = "gray40", linewidth = 0.4) +
  scale_fill_stepsn(
    colours = RColorBrewer::brewer.pal(6, "YlOrRd"),
    breaks  = c(0, 2, 5, 10, 20, 40, Inf),
    na.value = "grey85",
    name = "Jugadores"
  ) +
  coord_sf_inset(inset_cfg) +
  theme_void() +
  labs(title = "Venezolanos en MLB")
