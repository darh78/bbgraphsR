

get_teams_meta <- function() {

  # Getting Lahman package tables into bbgraprhsR package
  Teams_Lahman <- read.csv("data/Teams.csv")[ , -1]
  Teams_Franchises <- read.csv("data/TeamsFranchises.csv")[ , -1]

  teams_meta <- Teams_Lahman  |>
    dplyr::left_join(Teams_Franchises, by = "franchID") |>
    dplyr::select(yearID, teamIDBR, name, franchID, franchName, active, lgID, divID, DivWin, WCWin, LgWin, WSWin, park, attendance) |>
    dplyr::filter(active == "Y",
                  lgID == "AL" | lgID == "NL")

  teams_meta$name[teams_meta$teamIDBR == "LAA"] <- "Los Angeles Angels"

  # Getting teamcolors package table into bbgraprhsR package
  teamcolors <- read.csv("data/teamcolors.csv")[ , -1] |>
    dplyr::filter(league == "mlb")

  # Swap "primary" and "secondary" colors of some teams
  swap_indices <- c(1, 2, 5, 11, 16, 17, 21, 25, 27, 28)
  # 1 ARI, 2 ATL, 5 CHC, 11 HOU, 16 MIL, 17 MIN, 21 PHI, 25 SEA, 27 TBR, 28 TEX

  temp <- teamcolors$primary[swap_indices]
  teamcolors$primary[swap_indices] <- teamcolors$secondary[swap_indices]
  teamcolors$secondary[swap_indices] <- temp

  ### Create df teams_meta with all the info about them and joining the teamscolors df
  teams_meta <- teams_meta |>
    dplyr::left_join(teamcolors, by = "name") |>
    dplyr::rename(Season = yearID,
                  Team = teamIDBR,
                  TeamName = name,
                  Franchise = franchName) |>
    dplyr::select(Season, Team, TeamName, franchID, Franchise, active, lgID, divID, DivWin, WCWin, LgWin, WSWin, primary, secondary, park, attendance)

  teams_meta$primary[teams_meta$TeamName == "Brooklyn Dodgers"] <- "#082984"
  teams_meta$secondary[teams_meta$TeamName == "Brooklyn Dodgers"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "Boston Braves"] <- "#C8102E"
  teams_meta$secondary[teams_meta$TeamName == "Boston Braves"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "New York Giants"] <- "#FF3E00"
  teams_meta$secondary[teams_meta$TeamName == "New York Giants"] <- "#000000"
  teams_meta$primary[teams_meta$TeamName == "Philadelphia Athletics"] <- "#150360"
  teams_meta$secondary[teams_meta$TeamName == "Philadelphia Athletics"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "Kansas City Athletics"] <- "#00843D"
  teams_meta$secondary[teams_meta$TeamName == "Kansas City Athletics"] <- "#FFFFFF"
  teams_meta$primary[teams_meta$TeamName == "California Angels"] <- "#001E40"
  teams_meta$secondary[teams_meta$TeamName == "California Angels"] <- "#C1033B"
  teams_meta$primary[teams_meta$TeamName == "Tampa Bay Devil Rays"] <- "#02B189"
  teams_meta$secondary[teams_meta$TeamName == "Tampa Bay Devil Rays"] <- "#D2BC50"
  teams_meta$primary[teams_meta$TeamName == "Los Angeles Angels of Anaheim" |
                       teams_meta$TeamName == "Anaheim Angels"] <- "#BA0021"
  teams_meta$secondary[teams_meta$TeamName == "Los Angeles Angels of Anaheim" |
                         teams_meta$TeamName == "Anaheim Angels"] <- "#003263"
  teams_meta$primary[teams_meta$TeamName == "Cleveland Guardians"] <- "#00385D"
  teams_meta$secondary[teams_meta$TeamName == "Cleveland Guardians"] <- "#E50022"
  teams_meta$primary[teams_meta$TeamName == "Florida Marlins"] <- "#00A3B3"
  teams_meta$secondary[teams_meta$TeamName == "Florida Marlins"] <- "#000000"
  teams_meta$primary[teams_meta$TeamName == "Montreal Expos"] <-"#67ABE5"
  teams_meta$secondary[teams_meta$TeamName == "Montreal Expos"] <-"#E4002B"
  teams_meta$primary[is.na(teams_meta$primary)] <- "#000000"
  teams_meta$secondary[is.na(teams_meta$secondary)] <- "#E6E3E3"

  # Giving the dataframe as output to be used in another functions
  return(teams_meta)
}
