# modern-football
**Historical analysis of statistics from football's modern era using FBRef data.**


This repository scrapes a dataset of player statistics since the 1990s across the top 5 European leagues from FBRef, starting from the earliest season in which both non-penalty goals and assists were recorded in that league. Includes stats for 16,242 players over 29 seasons.


### You can find the following datasets:

- **all seasons** - A cleaned and manually corrected dataset of individual-season statistics for every player who played at least one game in the top 5 European leagues in the timeframe (see `scraping.R`)

- **players summed** - Total league career statistics for every player, created by summing the statistics of each season for every player (see `players summed.R`) *[to be released]*

- **best seasons** - A modified version of `all seasons.csv` that merges rows for players that played for more than one team in a single season (see `best seasons.R`)

### You can find the following visualisations:

- **historic goals assists** - *[to be released]*

- **best seasons** - [Interactive version **here**](https://public.tableau.com/views/Bestattackingseasonsmodernfootball/Bestseasons?:language=en-GB&:display_count=n&:origin=viz_share_link&:device=desktop)

![best seasons viz](best_seasons.png?raw=true)
