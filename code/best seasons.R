##### SUMMARY #####
# In this script we create a new df which stores total stats over the whole
# season for every player in our dataset (summing stats across multiple clubs played for in a season).
# We then prepare the data for visualisation in ggplot and Tableau.

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


## New df that sums seasons where a player played for two separate teams
best_seasons <- all_seasons %>%
  group_by(Player,Born,season) %>%
  summarise(
    MP = sum(MP),
    Starts = sum(Starts),
    Min = sum(Min),
    Full90s = sum(Full90s),
    G = sum(G),
    A = sum(A),
    NPG = sum(NPG),
    PK = sum(PK),
    xA = sum(xA)
  )

## Determining most played league for each player-season
most_played_season <- all_seasons %>%
  group_by(Player,Born,season) %>%
  top_n(1, Min) %>%
  ungroup()

best_seasons <- best_seasons %>%
  left_join(most_played_season %>% select(2,5,6,8,34), by = c('Player','Born','season'))

## Calculating per90 stats
best_seasons$G90 <- (best_seasons$G / best_seasons$Min)*90
best_seasons$NPG90 <- (best_seasons$NPG / best_seasons$Min)*90
best_seasons$A90 <- (best_seasons$A / best_seasons$Min)*90
best_seasons$`NPG+A90` <- best_seasons$NPG90 + best_seasons$A90
best_seasons$xA90 <- (best_seasons$xA / best_seasons$Min)*90
best_seasons$`NPG+xA90` <- best_seasons$NPG90 + best_seasons$xA90

## Adding a column with player + season
# Creating reference table of shorter season names (1998-1999 -> 98-99)
shorter_seasons  <- season_list %>%
  cbind(c("92-93",
                  "93-94",
                  "94-95",
                  "95-96",
                  "96-97",
                  "97-98",
                  "98-99",
                  "99-00",
                  "00-01",
                  "01-02",
                  "02-03",
                  "03-04",
                  "04-05",
                  "05-06",
                  "06-07",
                  "07-08",
                  "08-09",
                  "09-10",
                  "10-11",
                  "11-12",
                  "12-13",
                  "13-14",
                  "14-15",
                  "15-16",
                  "16-17",
                  "17-18",
                  "18-19",
                  "19-20",
                  "20-21",
                  "21-22*")) %>% #asterisk to indicate season is currently undergoing
  as.data.frame() %>%
  rename('season' = '.',
         'shorter_season' = `V2`)

# Join shorter_seasons column to best_seasons df
best_seasons <- best_seasons %>%
  left_join(shorter_seasons, by = 'season')


### VISUALISATION ###
# As there are over 63,000 individual seasons in the dataset, we cannot label all of them in the final ggplot viz
# However, we still want to a subset of player-seasons that are prominent and interesting
# To achieve this, we create a new column called DisplaySurname and manually add the surnames of the players we want to label here in Excel
# Moreover, player names still overlap making the graph less legible
# So, we create new columns for NPG90 and A90 where we can finely move player positions on the graph through Excel to minimise label overlap
# For the interactive Tableau version we don't need to do this, so we keep the unedited dataset with the true NPG90 and A90 stats

## Final dataset for Tableau
best_seasons$PlayerSeason <- NA
best_seasons$PlayerSeason <- paste(best_seasons$Player,best_seasons$shorter_season)
write_excel_csv(best_seasons,'data/best seasons final.csv') #Use this function so player names with non-standard characters are rendered correctly

## Final dataset for ggplot
best_seasons_edit <- best_seasons
best_seasons_edit$DisplaySurname <- NA
best_seasons_edit$NPG90edit <- best_seasons_edit$NPG90
best_seasons_edit$A90edit <- best_seasons_edit$A90
write_excel_csv(best_seasons_edit,'data/best seasons_excel.csv') #Use this function so player names with non-standard characters are rendered correctly
### >> Manually add surnames and edit NPG90 and A90 on Excel for ggplot viz << ##
best_seasons_ggplot <- read_excel('data/best seasons edit.xlsx')

# New string column for player and season
best_seasons_ggplot$shorter_season_edit <- NA
best_seasons_ggplot$shorter_season_edit <- best_seasons_ggplot$shorter_season
best_seasons_ggplot$shorter_season_edit[is.na(best_seasons_ggplot$DisplaySurname)] <- NA

best_seasons_ggplot$PlayerSeason <- paste(best_seasons_ggplot$DisplaySurname,best_seasons_ggplot$shorter_season_edit)
best_seasons_ggplot$PlayerSeason[best_seasons_ggplot$PlayerSeason == 'NA NA'] <- NA


## ggplot viz
best_seasons_plot <- ggplot(best_seasons_ggplot %>% filter(Min >= 1500), aes(x = A90edit, y = NPG90edit, size = Min, col = Comp)) +
  geom_point(alpha = 0.2) +
  scale_color_brewer(palette = 'Set1') +
  geom_text(aes(label=PlayerSeason),
            hjust=-.075,vjust=0,
            col = '#505050',
            show.legend = FALSE,
            family = 'Exo 2 Medium',
            alpha = 0.85) +
  scale_size(range = c(1, 4.25), name="Minutes played") +
  scale_x_continuous(name = "Assists per 90 minutes", breaks = c(-0.005,0.4,0.8), limits = c(-0.02,0.88), labels = c(0,0.4,0.8)) +
  scale_y_continuous(name = "Non-penalty goals per 90 minutes", breaks = c(-0.01,0.5,1), limits = c(-0.02,1.45), labels = c(0,0.5,1)) +
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
  'viz/best seasons plot.png',
  plot = best_seasons_plot,
  width = 3219,
  height = 2586,
  units = "px"
)


