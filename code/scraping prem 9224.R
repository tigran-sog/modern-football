####################################################################################################
###############################       SCRAPING.r      ##############################################
####################################################################################################
## All credit for data collection to FBRef, Opta, Nielsen, @npranav and the WorldFootballR library
## Data collected on 3rd June 2024

library(tidyverse)
library(worldfootballR)

`%notin%` <- negate(`%in%`)

# Only collecting data on outfield players
# Stat types: "standard", "shooting", "passing", "passing_types",
#             "gca", "defense", "possession", "playing_time", "misc"

# Leagues have data for...
## Prem 1992/93 onwards
## Serie A from 1998/99 onwards
## La Liga, Bundesliga and Ligue 1 from 1999/00 onwards


##### Contents
### 1) Modal attributes function
### 2) Scraping 1992/93 - 2016/17
### 3) Scraping 2017/18 - 2023/24

#### 1) Function to collect modal attributes of players
modal_attributes <- function(df,season) {
  if (season == TRUE) { # If user wants to group by season or not
    # Modal Position
    modal_position <- df %>%
      group_by(player_id, Position,Season_End_Year) %>%
      summarise(TotalMinutes = sum(Minutes, na.rm = TRUE), .groups = 'drop') %>%
      group_by(player_id,Season_End_Year) %>%
      slice_max(order_by = TotalMinutes, n = 1, with_ties = FALSE) %>%
      select(player_id, Season_End_Year, Position) %>%
      ungroup()
    
    # Modal Squad
    modal_squad <- df %>%
      group_by(player_id,Season_End_Year, Squad) %>%
      summarise(TotalMinutes = sum(Minutes, na.rm = TRUE), .groups = 'drop') %>%
      group_by(player_id,Season_End_Year) %>%
      slice_max(order_by = TotalMinutes, n = 1, with_ties = FALSE) %>%
      select(player_id, Season_End_Year, Squad) %>%
      ungroup()
    
    
    # Modal Competition
    modal_comp <- df %>%
      group_by(player_id, Season_End_Year, Comp) %>%
      summarise(TotalMinutes = sum(Minutes, na.rm = TRUE), .groups = 'drop') %>%
      group_by(player_id, Season_End_Year) %>%
      slice_max(order_by = TotalMinutes, n = 1, with_ties = FALSE) %>%
      select(player_id, Season_End_Year, Comp) %>%
      ungroup()
    
    # Average Age
    average_age <- df %>%
      group_by(player_id, Season_End_Year) %>%
      summarise(Age = mean(Age, na.rm = TRUE, with_ties = FALSE), .groups = 'drop')
    
    # Joining the results
    final_table <- reduce(list(modal_position, modal_squad, modal_comp, average_age), full_join, by = c("player_id","Season_End_Year"))
    
  }
  else if (season == FALSE) {
    # Modal Position
    modal_position <- df %>%
      group_by(player_id, Position) %>%
      summarise(TotalMinutes = sum(Minutes, na.rm = TRUE), .groups = 'drop') %>%
      group_by(player_id) %>%
      slice_max(order_by = TotalMinutes, n = 1, with_ties = FALSE) %>%
      select(player_id, Position) %>%
      ungroup()
    
    # Modal Squad
    modal_squad <- df %>%
      group_by(player_id, Squad) %>%
      summarise(TotalMinutes = sum(Minutes, na.rm = TRUE), .groups = 'drop') %>%
      group_by(player_id) %>%
      slice_max(order_by = TotalMinutes, n = 1, with_ties = FALSE) %>%
      select(player_id, Squad) %>%
      ungroup()
    
    
    # Modal Competition
    modal_comp <- df %>%
      group_by(player_id, Comp) %>%
      summarise(TotalMinutes = sum(Minutes, na.rm = TRUE), .groups = 'drop') %>%
      group_by(player_id) %>%
      slice_max(order_by = TotalMinutes, n = 1, with_ties = FALSE) %>%
      select(player_id, Comp) %>%
      ungroup()
    
    # Joining the results
    final_table <- reduce(list(modal_position, modal_squad, modal_comp), full_join, by = "player_id")
  }
  
  return(final_table)
}



#### 2) Scraping 1992/93 - 2016/17 (before advanced Opta stats available)
### Import data already collected (and manually cleaned)
### Join with 1992/93 - 2016/17 data
fbref_1992_2016 <- read_csv('data/fbref 1992 to 2016 fixed.csv') %>%
  mutate(Comp = case_match(Comp,
                           c('La Liga','es La Liga') ~ 'La Liga',
                           c('Ligue 1','fr Division 1', 'fr Ligue 1') ~ 'Ligue 1',
                           c('Premier League', 'eng Premier League') ~ 'Premier League',
                           c('Serie A','it Serie A') ~ 'Serie A',
                           c('Bundesliga','de Bundesliga') ~ 'Bundesliga')) %>%
  filter(!(Season_End_Year < 1999 & Comp %in% c('Serie A')),
         !(Season_End_Year < 2000 & Comp %in% c('Bundesliga','Ligue 1','La Liga'))) %>%
  filter(Season_End_Year <= 2017) %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>%
  mutate(Position = case_match(Position,
                               'DF' ~ 'Defender',
                               'DF,GK' ~ 'Defender',
                               'DF,FW' ~ 'Defender',
                               'DF,MF' ~ 'Defender',
                               'FW' ~ 'Forward',
                               'FW,DF' ~ 'Forward',
                               'FW,GK' ~ 'Forward',
                               'FW,MF' ~ 'Attacking midfielder',
                               'MF' ~ 'Midfielder',
                               'MF,DF' ~ 'Midfielder',
                               'MF,FW' ~ 'Attacking midfielder')) %>%
  filter(!is.na(Position)) %>%
  select(-Rk,-Full90s,-Season_Start_Year,-player_id) %>%
  # Creating temporary unique player ids (to be dropped and reallocated when joined with 2017-2023 data)
  group_by(Player,Born) %>%
  mutate(player_id = cur_group_id()) %>% 
  ungroup()

# Creating seasons dataset (i.e. summing stats for players with multiple teams within a season)
fbref_1992_2016_seasons <- fbref_1992_2016 %>%
  group_by(Player,Born,Age,player_id,Season_End_Year) %>%
  summarise(across(c(Matches,Starts,Minutes,G,NPG,A,PK,PKatt,CrdY,CrdR), sum), .groups = 'drop') %>% 
  left_join(fbref_1992_2016 %>%
              modal_attributes(season = TRUE)) %>%
  select(-player_id)

# Creating seasons dataset (i.e. summing stats for players with multiple teams within a season) [Prem only]
fbref_1992_2016_seasons_prem <- fbref_1992_2016 %>%
  filter(Comp == 'Premier League') %>%
  group_by(Player,Born,Age,player_id,Season_End_Year) %>%
  summarise(across(c(Matches,Starts,Minutes,G,NPG,A,PK,PKatt,CrdY,CrdR), sum), .groups = 'drop') %>% 
  left_join(fbref_1992_2016 %>%
              modal_attributes(season = TRUE)) %>%
  select(-player_id)

#### 3) Scraping 2017/18 - 2023/24 (after advanced Opta stats available) 
## Collect stats from these seasons across all of FBRef's stat types
seasons <- c(2018,2019,2020,2021,
             2022,2023,2024,2025)

## Standard Stats
standard_list <- list()

for (i in seq_along(seasons)) {
  standard_scrape <- fb_big5_advanced_season_stats(season_end_year= seasons[i], stat_type= "standard", team_or_player= "player")
  standard_list[[i]] <- standard_scrape %>%
    select(1:23,25:26,38)
}

standard <- do.call(rbind,standard_list)

## shooting Stats
shooting_list <- list()

for (i in seq_along(seasons)) {
  shooting_scrape <- fb_big5_advanced_season_stats(season_end_year= seasons[i], stat_type= "shooting", team_or_player= "player")
  shooting_list[[i]] <- shooting_scrape %>%
    select(11,12,18,19,27,1:2)
}

shooting <- do.call(rbind,shooting_list)

## passing Stats
passing_list <- list()

for (i in seq_along(seasons)) {
  passing_scrape <- fb_big5_advanced_season_stats(season_end_year= seasons[i], stat_type= "passing", team_or_player= "player")
  passing_list[[i]] <- passing_scrape %>%
    select(10,11,13,14,15,16,18,19,21,22,26,28,29,30,31,33,1:2)
}

passing <- do.call(rbind,passing_list)

## passing_types Stats
passing_types_list <- list()

for (i in seq_along(seasons)) {
  passing_types_scrape <- fb_big5_advanced_season_stats(season_end_year= seasons[i], stat_type= "passing_types", team_or_player= "player")
  passing_types_list[[i]] <- passing_types_scrape %>%
    select(11:21,23:25,1:2)
}

passing_types <- do.call(rbind,passing_types_list)


## gca Stats
gca_list <- list()

for (i in seq_along(seasons)) {
  gca_scrape <- fb_big5_advanced_season_stats(season_end_year= seasons[i], stat_type= "gca", team_or_player= "player")
  gca_list[[i]] <- gca_scrape %>%
    select(10,12:18,20:26,1:2)
}

gca <- do.call(rbind,gca_list)

## defense Stats
defense_list <- list()

for (i in seq_along(seasons)) {
  defense_scrape <- fb_big5_advanced_season_stats(season_end_year= seasons[i], stat_type= "defense", team_or_player= "player")
  defense_list[[i]] <- defense_scrape %>%
    select(10:16,18:26,1:2)
}

defense <- do.call(rbind,defense_list)

## possession Stats
possession_list <- list()

for (i in seq_along(seasons)) {
  possession_scrape <- fb_big5_advanced_season_stats(season_end_year= seasons[i], stat_type= "possession", team_or_player= "player")
  possession_list[[i]] <- possession_scrape %>%
    select(10:32,1:2)
}

possession <- do.call(rbind,possession_list)

## playing_time Stats
playing_time_list <- list()

for (i in seq_along(seasons)) {
  playing_time_scrape <- fb_big5_advanced_season_stats(season_end_year= seasons[i], stat_type= "playing_time", team_or_player= "player")
  playing_time_list[[i]] <- playing_time_scrape %>%
    select(12,20:31,1:2)
}

playing_time <- do.call(rbind,playing_time_list)


## misc Stats
misc_list <- list()

for (i in seq_along(seasons)) {
  misc_scrape <- fb_big5_advanced_season_stats(season_end_year= seasons[i], stat_type= "misc", team_or_player= "player")
  misc_list[[i]] <- misc_scrape %>%
    select(12:16,19:24,26,1:2)
}

misc <- do.call(rbind,misc_list)


### Collate 2017/18 - 2024/25 data into one df
## Combine stat types into one df
all_2017_2024 <- standard %>% 
  left_join(shooting,by = c('Url','Season_End_Year','Squad')) %>%
  left_join(passing,by = c('Url','Season_End_Year','Squad')) %>%
  left_join(passing_types,by = c('Url','Season_End_Year','Squad')) %>%
  left_join(gca,by = c('Url','Season_End_Year','Squad')) %>%
  left_join(defense,by = c('Url','Season_End_Year','Squad')) %>%
  left_join(possession,by = c('Url','Season_End_Year','Squad')) %>%
  left_join(playing_time,by = c('Url','Season_End_Year','Squad')) %>%  
  left_join(misc,by = c('Url','Season_End_Year','Squad')) %>%
  mutate(Player = iconv(Player, from = "UTF-8", to = "UTF-8")) %>%
  # Create two new columns, Years and Days, by splitting Age on the dash
  separate(Age, into = c("Age", "Days"), sep = "-", fill = "right", remove = FALSE) %>%
  # Convert Years and Days to numeric (Days will be NA where it was not present)
  mutate(Age = as.numeric(Age),
         Days = as.numeric(Days)) %>%
  rename(Matches = MP_Playing,
         Minutes = Min_Playing,
         Starts = Starts_Playing,
         Position_old = Pos,
         G = Gls,
         NPG = G_minus_PK,
         A = Ast,
         xG = xG_Expected,
         npxG = npxG_Expected,
         Shots = Sh_Standard,
         SoT = SoT_Standard,
         Shot_Distance = Dist_Standard,
         FK_Shots = FK_Standard,
         Final_Third_Pass = Final_Third,
         Cross_Penalty_Area = CrsPA,
         xAG = xAG_Expected,
         xA = xA_Expected,
         Switches = Sw_Pass,
         Crosses = Crs_Pass,
         Through_Balls = TB_Pass,
         Throw_Ins = TI_Pass,
         Corners = CK_Pass,
         Offside_Passes = Off_Outcomes,
         Blocked_Passes = Blocks_Outcomes,
         PrgC = PrgC_Progression,
         PrgP = PrgP_Progression,
         Total_SCA = SCA_SCA,
         Total_GCA = GCA_GCA,
         Def_3rd_Tackles = `Def 3rd_Tackles`,
         Mid_3rd_Tackles = `Mid 3rd_Tackles`,
         Att_3rd_Tackles = `Att 3rd_Tackles`,
         Total_Blocks = Blocks_Blocks,
         Touches = Touches_Touches,
         Def_Pen_Touches = `Def Pen_Touches`,
         Def_3rd_Touches = `Def 3rd_Touches`,
         Mid_3rd_Touches = `Mid 3rd_Touches`,
         Att_3rd_Touches = `Att 3rd_Touches`,
         Att_Pen_Touches = `Att Pen_Touches`,
         Total_Carries = Carries_Carries,
         onG = `onG_Team.Success`,
         onGA = `onGA_Team.Success`,
         onxG = `onxG_Team.Success..xG.`,
         onxGA = `onxGA_Team.Success..xG`,
         CrdY_2nd = `2CrdY`) %>%
  filter(Position_old %notin% c('GK','GK,MF','')) %>%
  mutate(Position = case_match(Position_old,
                               'DF' ~ 'Defender',
                               'DF,GK' ~ 'Defender',
                               'DF,FW' ~ 'Defender',
                               'DF,MF' ~ 'Defender',
                               'FW' ~ 'Forward',
                               'FW,DF' ~ 'Forward',
                               'FW,GK' ~ 'Forward',
                               'FW,MF' ~ 'Attacking midfielder',
                               'MF' ~ 'Midfielder',
                               'MF,DF' ~ 'Midfielder',
                               'MF,FW' ~ 'Attacking midfielder')) %>%
    mutate(Comp = case_match(Comp,
                             c('La Liga','es La Liga') ~ 'La Liga',
                             c('Ligue 1','fr Division 1', 'fr Ligue 1') ~ 'Ligue 1',
                             c('Premier League', 'eng Premier League') ~ 'Premier League',
                             c('Serie A','it Serie A') ~ 'Serie A',
                             c('Bundesliga','de Bundesliga') ~ 'Bundesliga'),
           SCA_OpenPlay = PassLive_SCA + TO_SCA + Sh_SCA + Fld_SCA + Def_SCA,
           GCA_OpenPlay = PassLive_GCA + TO_GCA + Sh_GCA + Fld_GCA + Def_GCA) %>%
  select(Season_End_Year, Player,
         Matches, Starts, Minutes, G, NPG, A,
         xG, npxG, Shots, SoT, Shot_Distance, FK_Shots, PK, PKatt,
         xAG, xA, PrgP, Cmp_Total, Att_Total, TotDist_Total, PrgDist_Total, Cmp_Short, Att_Short, Cmp_Medium, Att_Medium, Cmp_Long, Att_Long, Final_Third_Pass, PPA, Cross_Penalty_Area, # Passing
         Live_Pass, Dead_Pass, FK_Pass, Through_Balls, Switches, Crosses, Throw_Ins, Corners, In_Corner, Out_Corner, Str_Corner, Offside_Passes, Blocked_Passes,  # Passing types
         Total_GCA, GCA_OpenPlay, PassLive_GCA, TO_GCA, Sh_GCA, Fld_GCA, Def_GCA, PassDead_GCA, Total_SCA, SCA_OpenPlay, PassLive_SCA, TO_SCA, Sh_SCA, Fld_SCA, Def_SCA, PassDead_SCA, # Goal-creating actions
         Tkl_Tackles, TklW_Tackles, Def_3rd_Tackles, Mid_3rd_Tackles, Att_3rd_Tackles, Tkl_Challenges, Att_Challenges, Lost_Challenges, Total_Blocks, Sh_Blocks, Pass_Blocks, Int, `Tkl+Int`, Recov, Clr, Err, # Defensive
         Touches, Def_Pen_Touches, Def_3rd_Touches, Mid_3rd_Touches, Att_3rd_Touches, Att_Pen_Touches, Live_Touches, Att_Take, Succ_Take, Tkld_Take, Total_Carries, TotDist_Carries, PrgDist_Carries, PrgC_Carries, Final_Third_Carries, CPA_Carries, Mis_Carries, Dis_Carries, Rec_Receiving, PrgR_Receiving,
         PKwon, PKcon, OG, Won_Aerial, Lost_Aerial, Fls, Fld, Off, CrdY, CrdY_2nd, CrdR,
         Nation, Born, Age, Days, Position, Position_old, Squad, Comp, Url) %>%
  # Creating temporary unique player ids (to be dropped and reallocated when joined with 1992-2016 data)
  group_by(Url) %>%
  mutate(player_id = cur_group_id()) %>% 
  ungroup()# Miscellaneous player info

# Back-up and reimport
write_excel_csv(all_2017_2024,'data/2017 to 2024 raw.csv')
all_2017_2024 <- read_csv('data/2017 to 2024 raw.csv')


#### 4) Exporting datasets from scraped data

## Creating seasons dataset for 2017/18 - 2024/25 (combining any rows where a player has played for more than one team)
all_2017_2024_seasons <- all_2017_2024 %>%
  group_by(Player,player_id,Season_End_Year,Nation,Born,Age,Url) %>%
  summarise(across(where(is.numeric), sum), .groups = 'drop') %>% 
  left_join(all_2017_2024 %>%
              modal_attributes(season = TRUE)) 


## [Prem only] Creating seasons dataset for 2017/18 - 2024/25 (combining any rows where a player has played for more than one team)
all_2017_2024_seasons_prem <- all_2017_2024 %>%
  filter(Comp == 'Premier League') %>%
  group_by(Player,player_id,Season_End_Year,Nation,Born,Age,Url) %>%
  summarise(across(where(is.numeric), sum), .groups = 'drop') %>% 
  left_join(all_2017_2024 %>%
              modal_attributes(season = TRUE)) 

## Creating season dataset for 2023/24 (combining any rows where a player has played for more than one team)
all_2023_24_season <- all_2017_2024_seasons %>%
  filter(Season_End_Year == 2024)


## Combining 1992/93 - 2016/17 and 2017/18 - 2024/25 datasets together
all_seasons <- bind_rows(all_2017_2024_seasons,fbref_1992_2016_seasons) %>%
  group_by(Player,Born) %>%
  mutate(player_id = cur_group_id()) %>% 
  ungroup() %>%
  select(-Days)


## Combining 1992/93 - 2016/17 and 2017/18 - 2024/25 datasets together [Prem only]
all_seasons_prem <- bind_rows(all_2017_2024_seasons_prem,fbref_1992_2016_seasons_prem) %>%
  group_by(Player,Born) %>%
  mutate(player_id = cur_group_id()) %>% 
  ungroup() %>%
  select(-Days)


### Exporting datasets
# 1992/93 - 2024/25 (no advanced stats)
write_excel_csv(all_seasons,'data/1992 to 2024 seasons.csv')

# 2016/17 - 2022/23 (for joining with US)

# 2017/18 - 2024/25
write_excel_csv(all_2017_2024_seasons,'data/2017 to 2024 seasons.csv')

# 2023/24 (latest season)
write_excel_csv(all_2023_24_season,'data/2023 24 season.csv')


