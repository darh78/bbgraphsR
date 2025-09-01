library(dplyr)

# Script to get the dataframe of all the Venezuelan MLB players and clean it

#### Retreive updated list ofplayers from Venezuela ------
ven <-  get_players_by_country("Venezuela")

#### Correcting some values of variables for some players ----
ven[ven$Name == "Eduardo Escobar", "State"] <- "Aragua"               # Considered as Maracay
ven[ven$Name == "Sandy León", "State"] <- "Zulia"                     # Typo correction from Zuila
ven[ven$Name == "Yoendrys Gómez", "State"] <- "Yaracuy"               # Missing State
ven[ven$Name == "Jesús Tinoco", "State"] <- "Monagas"                 # Missing State
ven[ven$Name == "Maikel Garcia", "State"] <- "La Guaira"              # Wrong State
ven[ven$Name == "Luis Curvelo", "Debut"] <- as.Date("2025-07-31")     # Debut date missing in the table (retrieve from game logs)
ven[ven$Name == "Carlos Ascanio", "Debut"] <- as.Date("1946-05-30")   # Debut date missing in the table (Estimated)

#### Fix the ortography of some States names to match the Natural Earth (rnaturalearth pck) names ----
ven <- ven  |>
  mutate(
    State = case_when(
      State == "Bolivar" ~ "Bolívar",
      State == "Tachira" ~ "Táchira",
      State == "Merida" ~ "Mérida",
      State == "Falcon" ~ "Falcón",
      State == "Anzoategui" ~ "Anzoátegui",
      State == "Guarico" ~ "Guárico",
      State == "Distrito Federal" ~ "Distrito Capital",
      TRUE ~ State
    )
  ) |>
  arrange(Debut)
