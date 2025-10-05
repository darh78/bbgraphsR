## Internal globals and imports for R CMD check cleanliness

#' @keywords internal
#' @noRd
utils::globalVariables(c(
  # common aesthetics/columns used non-standardly in dplyr/ggplot
  "x","y","label","text_color","fontface","leader","linewidth","alpha","linetype","spacing",
  # standings / viz fields
  "Year","Team","W","L","Wpct","Rank","Date","Opp","Season","SeasonType","TeamName",
  "Rank_RD","Rank_Wpct","RunningTotal","cum_RD","Gcar","Gcar_real","Rk","Tm","Inngs",
  # palette / metadata
  "yearID","teamIDBR","name","franchID","franchName","active","lgID","divID",
  "DivWin","WCWin","LgWin","WSWin","league","Franchise","TeamName.y","Franchise.y",
  "primary","secondary","primary.y","secondary.y","last_season_info","Wp_global","WLpct",
  # misc labels used in plots/tidying
  "Label","x_bracket","x_flag","x_label","x_line_end","x_lineend",
  # stat columns referenced programmatically
  "PA","AB","R","H","`2B`","`3B`","HR","RBI","BB","SO","TB","SB","CS","GIDP","HBP","SH","SF","ROE","IBB",
  "BA","OBP","SLG","OPS","BAbip","aLI","WPA","acLI","cWPA","RE24","BOP","Pos",
  # other names from data sets/utilities
  "Players","Players_Unknown_debut","Game","Game_MaxStreak","Game_MinStreak","MaxStreak","MinStreak",
  "Record","RD","RA","R_cum","RA_cum","TeamName","game_location",
  # additional names flagged by R CMD check
  "PlayerID","rows","month_day","Gm","GB","H_A","Streak","Rank_Streak",
  "AVG.bat","AVG.pit","BABIP.bat","BABIP.pit","BB.bat","BB.pit","BB_9","BB_K","BB_pct.bat",
  "BK","BS","Birthdate","Birthplace","CG","Country","Debut","Debuts","ER","ERA","ERA_adjusted",
  "FB.pit","FB_pct.bat","FB_pct.pit","FIP","FlagURL","GB.bat","GB.pit","GB_FB.bat","GB_FB.pit",
  "GB_pct.bat","GB_pct.pit","GDP","GS","H.bat","H.pit","HBP.bat","HBP.pit","HR.bat","HR.pit",
  "HR_9","HR_FB.bat","HR_FB.pit","H_9","IBB.bat","IBB.pit","IP","ISO","K_9","K_BB","K_pct.bat",
  "LD.bat","LD.pit","LD_pct.bat","LD_pct.pit","R.bat","R.pit","SO.bat","SV","ShO","WAR.bat","WAR.pit",
  "WHIP","WP","active.y","alt","attendance","divID.y","franchID.y","from_season","label_text","lgID.y",
  "park","pythWpct","wOBA","wRAA","wRC","wRC_plus"
))

#' @importFrom utils read.csv adist setTxtProgressBar txtProgressBar
#' @importFrom stats complete.cases runif
NULL
