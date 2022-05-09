##### SUMMARY #####
# In this script we create a new df counting total NPG and A stats
# for every unique player in our dataset across every season in the timeframe.
# We then prepare the data for visualisation in ggplot and in Tableau.


#--------------------------
# load packages
# -------------------------
library(dplyr)
library(readr)
library(readxl)

library(showtext)
showtext_auto()
library(extrafont)
font_import(path = '../../../../Fonts/Exo 2')
View(fonts())
loadfonts(device = "postscript")

library(ggplot2)



### DATA PROCESSING ###

# New dataset with relevant statistics summed
players_summed <- all_seasons %>%
  group_by(Player,Born) %>%
  summarise(
    minAge = min(Age, na.rm = TRUE),
    MP = sum(MP),
    Starts = sum(Starts),
    Min = sum(Min),
    Full90s = sum(Full90s),
    G = sum(G),
    A = sum(A),
    NPG = sum(NPG),
    PK = sum(PK)
    )

## Determining the league each player has played the most minutes in
# Initialising df to count minutes per league for each player
league_count <- setNames(data.frame(matrix(ncol = 7, 
                           nrow = dim(players_summed)[1])), # rows = no. of unique players
         c("Player","Born","eng Premier League","fr Ligue 1","it Serie A","es La Liga","de Bundesliga"))

league_count$Player <- players_summed$Player
league_count$Born <- players_summed$Born

# Counting minutes per league for each player
league_list <- c("eng Premier League","fr Ligue 1","it Serie A","es La Liga","de Bundesliga")

for (p in 1:dim(league_count)[1]) {
  for (l in 1:length(league_list)) {
    league_count[p,l+2] <- sum(all_seasons$Min[all_seasons$Player == league_count[p,1] & 
                                         all_seasons$Born == league_count[p,2] &
                                         all_seasons$Comp == league_list[l]])
  }
}

# Assigning most played league for each player
league_count$Comp <- NA

for (p in 1:dim(league_count)[1]) {
  league_count[p,8] <- colnames(league_count[3:7])[max.col(league_count[p,3:7],ties.method="random")]
}

# Joining league to players df
players_summed %>%
  left_join(league_count[c(1,2,8)], by = 'Player','Born')

# Some players have "Inf" minimum age as they only played one season where age wasn't recorded. 
# These players all have negligible minutes, so we just replace with "18" as placeholder
players_summed$minAge[players_summed$minAge == 'Inf'] <- 18

## Calculating per90 stats
players_summed$G90 <- (players_summed$G / players_summed$Min)*90
players_summed$NPG90 <- (players_summed$NPG / players_summed$Min)*90
players_summed$A90 <- (players_summed$A / players_summed$Min)*90
players_summed$`NPG+A90` <- players_summed$NPG90 + players_summed$A90

# New column indicating whether player's minimum age is 27 or over (for visualisation)
players_summed$Over27 <- NA
players_summed$Over27[players_summed$minAge >=27] <- '*'
players_summed$Over27[players_summed$minAge <27] <- ''


### VISUALISATION ###
# As there are over 16,000 players in the dataset, we cannot label all of them in the final ggplot viz
# However, we still want to a subset of players who are prominent and interesting
# To achieve this, we create a new column called DisplaySurname and manually add the surnames of the players we want to label here in Excel
# Moreover, player names still overlap making the graph less legible
# So, we create new columns for NPG90 and A90 where we can finely move player positions on the graph through Excel to minimise label overlap
# For the interactive Tableau version we don't need to do this, and simply import the unedited dataset with the true NPG90 and A90 stats

# Final dataset for Tableau
write_excel_csv(players_summed,'data/players summed final 2.csv') #Use this function so player names with non-standard characters are rendered correctly

# Final dataset for ggplot
players_summed_edit <- players_summed
players_summed_edit$Surname <- NA
players_summed_edit$NPG90Edit <- players_summed_edit$NPG90
players_summed_edit$A90Edit <- players_summed_edit$A90
write_excel_csv(players_summed_edit,'data/players summed final excel 2.csv')
### >> Manually add surnames and edit NPG90 and A90 on Excel for ggplot viz << ##
players_summed_ggplot <- read_excel('data/players summed final edit.xlsx')
players_summed_ggplot$DisplaySurname<- paste(players_summed$Surname,players_summed$Over27)
players_summed_ggplot$DisplaySurname[players_summedB$DisplaySurname == '*'] <- ''

## ggplot viz
final_plot <- ggplot(players_summed_ggplot %>% filter(Min > 7000), aes(x = A90edit, y = NPG90edit, size = Min, col = Comp)) +
  geom_point(alpha = 0.2) +
  scale_color_brewer(palette = 'Set1') +
  geom_text(aes(label=DisplaySurname),
            hjust=-.075,vjust=0,
            col = '#333333',
            show.legend = FALSE,
            family = 'Exo 2 Medium',
            alpha = 0.85) +
  scale_size(range = c(1, 4.25), name="Minutes played") +
  scale_x_continuous(name = "Assists per 90 minutes", breaks = c(-0.005,0.25,0.5), limits = c(-0.02,0.53), labels = c(0,0.25,0.5)) +
  scale_y_continuous(name = "Non-penalty goals per 90 minutes", breaks = c(-0.01,0.45,0.9), limits = c(-0.02,0.91), labels = c(0,0.45,0.9)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = '#f5f5f5', color = '#f5f5f5'),
    plot.background = element_rect(fill = '#f5f5f5', color = '#f5f5f5'),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_line(color = '#ebebeb'),
    text=element_text(family = 'Exo 2 SemiBold')
  )

# Export as png and edit in Photoshop
ggsave(
  'viz/players summed plot.png',
  plot = final_plot,
  width = 3219,
  height = 2965,
  units = "px"
)

