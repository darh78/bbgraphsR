library(dplyr)

# Script to get the dataframe of all the Venezuelan MLB players and clean it

#### Retreive updated list ofplayers from Venezuela ------
rd <-  get_players_by_country("Dominican Republic")

#### Correcting some values of variables for some players ----
rd[rd$Name == "Jesus de la Rosa", "PlayerID"] <- "dela_je01"              # Considered as je01
rd[rd$Name == "Francisco de la Rosa", "PlayerID"] <- "dela_fr01"          # Considered as fr01
rd[rd$Name == "D'Angelo Jimenez", "PlayerID"] <- "jimend'01"              # Considered as 01
rd[rd$Name == "Julio Baez", "PlayerID"] <- "baezju01"                     # Missing State
# rd[rd$Name == "Maikel Garcia", "State"] <- "La Guaira"              # Wrong State
rd[rd$Name == "Julio Baez", "Debut"] <- as.Date("1940-05-30")             # Debut date missing in the table (Estimated)
rd[rd$Name == "Sijo Gómez", "Debut"] <- as.Date("1929-05-30")             # Debut date missing in the table (Estimated)
rd[rd$Name == "Néstor Lambertus", "Debut"] <- as.Date("1929-05-30")       # Debut date missing in the table (Estimated)
rd[rd$Name == "Enrique Lantigua", "Debut"] <- as.Date("1935-05-30")       # Debut date missing in the table (Estimated)
rd[rd$Name == "Horacio Martínez", "Debut"] <- as.Date("1935-05-30")       # Debut date missing in the table (Estimated)
rd[rd$Name == "Pedro San", "Debut"] <- as.Date("1926-05-30")              # Debut date missing in the table (Estimated)
rd[rd$Name == "Tetelo Vargas", "Debut"] <- as.Date("1927-05-30")          # Debut date missing in the table (Estimated)

#### Fix the ortography of some States names to match the Natural Earth (rnaturalearth pck) names ----
# rd <- rd  |>
#   mutate(
#     State = case_when(
#       State == "Bolivar" ~ "Bolívar",
#       State == "Tachira" ~ "Táchira",
#       State == "Merida" ~ "Mérida",
#       State == "Falcon" ~ "Falcón",
#       State == "Anzoategui" ~ "Anzoátegui",
#       State == "Guarico" ~ "Guárico",
#       State == "Distrito Federal" ~ "Distrito Capital",
#       TRUE ~ State
#     )
#   ) |>
#   arrange(Debut)
