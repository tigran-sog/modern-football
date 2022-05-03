##### SUMMARY #####
# In this script we prepare the dataframes to scrape data,
# we scrape data from FBref and clean the scraped data 
# to produce an exhaustive dataframe
# of every player-season in the top 5 leagues since the 90s.

#--------------------------
# load packages
# -------------------------
library(dplyr)
library(readr)
library(readxl)

### DATA SCRAPING ###
# Vector for every season between 92/93 and 21/22
season_list  <- c("1992-1993",
                "1993-1994",
                "1994-1995",
                "1995-1996",
                "1996-1997",
                "1997-1998",
                "1998-1999",
                "1999-2000",
                "2000-2001",
                "2001-2002",
                "2002-2003",
                "2003-2004",
                "2004-2005",
                "2005-2006",
                "2006-2007",
                "2007-2008",
                "2008-2009",
                "2009-2010",
                "2010-2011",
                "2011-2012",
                "2012-2013",
                "2013-2014",
                "2014-2015",
                "2015-2016",
                "2016-2017",
                "2017-2018",
                "2018-2019",
                "2019-2020",
                "2020-2021",
                "2021-2022")

# Initialise master df to hold scraped data from all seasons
fbref_extract <- setNames(data.frame(
  matrix(ncol = 35, nrow = 0)), 
  c("Rk", "Player", "Nation", "Pos", "Squad", "Comp", "Age", "Born", 
  "MP", "Starts", "Min", "90s", "Gls", "Ast", "G-PK", "PK", "PKatt",
  "CrdY", "CrdR", "Gls", "Ast", "G+A", "G-PK", "G+A-PK", "xG", "npxG",
  "xA", "npxG+xA", "xG", "xA", "xG+xA", "npxG", "npxG+xA", "season"))

# Creating vector for expected assists stats (this will be used to join pre-17-18 and post-17-18 season data together)
extra_columns <- c("xG", "npxG", "xA", "npxG+xA", "xG", "xA", "xG+xA", "npxG", "npxG+xA")

## Scraping data from FBRef
# This for loop cycles through every season, calling a new URL to scrape from each time
for (s in season_list) {
  # Master URL to scrape from
  url <- ("https://fbref.com/en/comps/Big5/season/stats/players/season-Big-5-European-Leagues-Stats")
  
  # Code to scrape table from FBref (borrowed from github.com/npranav10)
  season_data <- xml2::read_html(gsub("season",s,url)) %>% 
    rvest::html_nodes("#stats_standard") %>% 
    rvest::html_table()

  ## Seasons after 17-18 have extra columns pertaining to xG, so we handle them here with this if/else block
  # Seasons after 17-18
  if (s %in% c("2017-2018","2018-2019","2019-2020","2020-2021","2021-2022")) { 
  
    # Extract df with proper column names
    season_data <- season_data[[1]]
    colnames(season_data) <- season_data[1,]
    season_data <- season_data[c(-1),]
    
    season_data <- season_data[,-c(34)] # getting rid of "Matches" column
    season_data$season <- s # adding column indicating which season the data is from
    
    # Adding df to master df
    fbref_extract <- rbind(fbref_extract,season_data)
    
  }
  # Seasons before 17-18
  else { 
    # Extract df with proper column names
    season_data <- season_data[[1]]
    colnames(season_data) <- season_data[1,]
    season_data <- season_data[c(-1),]
    
    season_data <- season_data[,-c(25)] # getting rid of "Matches" column
    season_data <- cbind(season_data, setNames( lapply(extra_columns, function(x) x=NA), extra_columns)) # Adding extra xG columns so it can rbind with master df
    season_data$season <- s # adding column indicating which season the data is from
    
    # Adding df to master df
    fbref_extract <- rbind(fbref_extract,season_data)
    
  }
}

## Code to backup df if necessary
#all_data_backup <- all_data
#all_data <- all_data_backup
#all_data <- read_csv('all_data 280222.csv')

#### DATA CLEANING ####

# Remove commas from "minutes" string (so we can convert it to numeric later)
fbref_extract$Min <- gsub(',','',fbref_extract$Min)

# Renaming columns so all names are unique
colnames(fbref_extract)[12:34] <- c("Full90s","G","A","NPG","PK","PKatt","YC","RC",
                     "G90","A90","GA90","NPG90","NPGA90",
                     "xG","NPxG","xA","NPxGA",
                     "xG90","xA90","xGxA90","NPxG90","NPxGA90","season")

# Getting rid of all rows with column names there
fbref_extract <- fbref_extract %>% 
  filter(Rk != 'Rk') #3052 observations removed

# Resetting row index
row.names(fbref_extract) <- NULL

# Fix Evens Joseph's negative NPG stat
fbref_extract[which(fbref_extract$Player == "Evens Joseph" & fbref_extract$season == "2018-2019"),c(13,15,16)] <- c(0,0,0)

#-------------------------------------
# Further cleaning to perform in Excel
#-------------------------------------
# For some players in the 90s (usually those who transferred mid-season)
# data for Assists and Non-penalty Goals is missing is for some seasons
# We need to manually add these stats by checking match records for such seasons on FBref

# Saving all_data to csv (to manually add missing NPG data)
write_excel_csv(fbref_extract,'data/fbref_extract 020322.csv') #Use this function so player names with non-standard characters are rendered correctly

# Loading manually cleaned data back into R
all_seasons <- read_excel('data/fbref_extract 020322 manually corrected.xlsx')


#-------------------------------------
# Final cleaning to do in R
#-------------------------------------

# Turn all remaining blank NPG into 0 (as we know they scored no goals at all)
all_seasons$NPG[is.na(all_seasons$NPG)] <- 0
all_seasons$PK[is.na(all_seasons$PK)] <- 0

# Removing league-seasons where assists were not collected by FBRef
all_seasons <- all_seasons[!(all_seasons$season %in% season_list[1:7] & all_seasons$Comp == 'de Bundesliga'),]
all_seasons <- all_seasons[!(all_seasons$season %in% season_list[1:7] & all_seasons$Comp == 'fr Division 1'),]
all_seasons <- all_seasons[!(all_seasons$season %in% season_list[1:6] & all_seasons$Comp == 'it Serie A'),]
all_seasons <- all_seasons[!(all_seasons$season %in% season_list[1:6] & all_seasons$Comp == 'es La Liga'),]

# Replacing rows with blank assists with 0
all_seasons$A[all_seasons$A == ''] <- 0
all_seasons$A[is.na(all_seasons$A)] <- 0

# Turning all necessary columns to numeric
all_seasons[7:33] <- lapply(all_seasons[7:33], function(x) as.numeric(as.character(x)))

# Resetting row index
row.names(all_seasons) <- NULL

# Recalculating NPG from manually corrected PK column
all_seasons$NPG <- all_seasons$G - all_seasons$PK

# Recalculating per90 stats
all_seasons$G90 <- (all_seasons$G / all_seasons$Min)*90
all_seasons$A90 <- (all_seasons$A / all_seasons$Min)*90
all_seasons$GA90 <- ((all_seasons$G + all_seasons$A)/ all_seasons$Min)*90
all_seasons$NPG90 <- (all_seasons$NPG /all_seasons$Min)*90
all_seasons$NPGA90 <- ((all_seasons$NPG + all_seasons$A)/ all_seasons$Min)*90

# Rename "fr Division 1" to "fr Ligue 1"
all_seasons$Comp[all_seasons$Comp == 'fr Division 1'] <- 'fr Ligue 1'

## Write final all_seasons file to csv
write_excel_csv(all_seasons,'data/all seasons.csv') #Use this function so player names with non-standard characters are rendered correctly
