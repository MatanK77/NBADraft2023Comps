# Necessary Datasets (in CSV files)
historical_college_players
draft2023_college_players

# Scaling Data

historical_college_players[3:17] <- scale(historical_college_players[3:17])

# Adding Positions

draft_2023positions <- c("Forward", "Guard", "Guard", "Guard-Forward",
                         "Guard-Forward", "Center", "Forward", "Forward",
                         "Forward", "Forward-Center", "Forward", "Forward",
                         "Guard-Forward", "Forward", "Forward", "Guard",
                         "Guard", "Forward", "Forward", "Forward", "Guard-Forward",
                         "Forward-Center", "Guard-Forward", "Forward", "Guard",
                         "Guard", "Guard", "Forward", "Forward")

draft2023_college_players <- draft2023_college_players %>%
  mutate(POSITION = draft_2023positions, .after = DISPLAY_FIRST_LAST)

# Scaling 2023 Data

draft2023_college_players[3:17] <- scale(draft2023_college_players[3:17])


# Function to find euclidean distance between 2023 player and historical players

compute_distances <- function(input_player, historical_players) {
  # Compute Euclidean distance between input_player and all historical_players
  distances <- apply(historical_players[-c(1,2)], 1, function(player) {
    sqrt(sum((input_player[,-c(1,2)] - player)^2))
  })
  
  # Get the names of the top 3 players with the lowest distance
  top_3_indices <- order(distances)[1:3]
  top_3_players <- historical_players$DISPLAY_FIRST_LAST[top_3_indices]
  
  print(input_player$DISPLAY_FIRST_LAST)
  return(top_3_players)
}

# Example of using function

compute_distances(draft2023_college_players[6,], historical_college_players)


# All Comparisons

all_comps <- map_df(1:nrow(draft2023_college_players), function(i) {
  input_player <- draft2023_college_players[i, ]
  top_3_players <- compute_distances(input_player, historical_college_players)
  data.frame(Input_Player = input_player$DISPLAY_FIRST_LAST, 
             Top_3_Players = paste(top_3_players, collapse = ", "))
})




# Combined dataset

combined <- bind_rows(draft2023_college_players, historical_college_players)

# Removing position data

combined <- combined[, -2]  

# Rescaling from 0-1 

combined_radar <- combined %>%
  mutate(across(-DISPLAY_FIRST_LAST, ~ rescale(., to = c(0, 1), na.rm = TRUE)))

# Like the other stats, make TOV% and PF_100 where "more is better" in terms of scaling (higher from 0-1 to mean fewer TOV)

combined_radar$`TOV%_career` <- 1 - combined_radar$`TOV%_career`
combined_radar$PF100_career <- 1 - combined_radar$PF100_career


# Adding Max and min to the dataframe for use in fmsb radar charts

combined_radar_fmsb <- rbind(rep(1, 16), rep(0, 16), combined_radar)

combined_radar_fmsb <- combined_radar_fmsb %>%
  rename(FTr = FTr_career, `TS%` = `TS%_career`, `3PA_r` = `3PAr_career`, Height = height_in_inches,
         `FT%` = `FT%_career`, `2P%` = `2P%_career`,`3P%` = `3P%_career`, PF_100 = PF100_career,
         `ORB%` = `ORB%_career`,`DRB%` = `DRB%_career`, `AST%` = `AST%_career`, `STL%` = `STL%_career`,
         `BLK%` = `BLK%_career`, `TOV%` = `TOV%_career`, `USG%` = `USG%_career`)



# Iterate over each player and make radarchart
for (i in 3:31) {
  player <- combined_radar_fmsb[i, ]
  
  
  # Create a radar plot
  radarchart(combined_radar_fmsb[c(1,2,i),-1],
             caxislabels = colnames(combined_radar_fmsb),
             cglcol = "gray",
             vlcex = 0.8,
             pcol = "purple",
             plwd = 2,
             title = combined_radar_fmsb[i,]$DISPLAY_FIRST_LAST)
}

