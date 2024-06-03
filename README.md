# modern-football
**Visualisations of football statistics from the top 5 European leagues since the 1990s**


This repository scrapes and analyses player statistics since the 1992/93 season across the top 5 European leagues from FBRef, starting from the earliest season in which both non-penalty goals and assists were recorded in that league. Includes stats for 16,053 players over 31 seasons.


### You can find the following datasets:

- **all seasons** - A cleaned and manually corrected dataset of individual-season statistics for every player who played at least one game in the top 5 European leagues in the timeframe (see `scraping.R`)

- **players summed** - Total league career statistics for every player, created by summing the statistics of each season for every player (see `players summed.R`) 

- **best seasons** - A modified version of `all seasons.csv` that merges rows for players that played for more than one team in a single season (see `best seasons.R`)

### You can find the following visualisations:

- **historic goals assists** - [Interactive version **here**](https://public.tableau.com/views/Whohavebeenthebiggestattackingthreatsofthemodernera/Biggestattackingthreatsofthemodernera?:language=en-GB&:display_count=n&:origin=viz_share_link&:device=desktop)

- **best seasons** - [Interactive version **here**](https://public.tableau.com/views/Bestattackingseasonsmodernfootball/Bestseasons?:language=en-GB&:display_count=n&:origin=viz_share_link&:device=desktop)


![1992/93 - 2023/24 NPG vs A viz](viz/9223%20npg%20a%20export.png?raw=true)

### /code/

- **scraping.R** - Collecting FBRef data from seasons 1992/93 to 2023/24 and exporting as datasets
- **1992_2023 summed.R** - Analysis of 1992/93 to 2023/24 seasons (player stats summed)
- **2023_24 season.R** - Analysis of 2023/24 season

### /data/

- **1992 to 2023 summed.csv**
- **1992 to 2023 per 90.csv**