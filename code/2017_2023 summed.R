##### 2017_2024 summed
## Data between 2017/18 - 2023/24 for visualising advanced analytics from recent seasons (trends etc)

### Creating summed dataset
summed_1723 <- all_2017_2023_seasons %>%
  select(-Position,-Squad,-Comp,-Age,-Season_End_Year) %>%
  group_by(Player, player_id,Nation,Born,Url) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop') %>%
  left_join(all_2017_2023_seasons %>%
              select(Player,player_id,Position,Squad,Comp,Nation,Born,Minutes) %>%
              modal_attributes(season = FALSE))

### Created per 90 dataset
per90_1723 <- summed_1723 %>%
  filter(Minutes > 3500) %>%
  #mutate(across(where(is.numeric), ~ round(. / (Minutes / 90),4))) %>%
  mutate(across(where(is.numeric) & !all_of(c('player_id','Born','Matches','Starts','Minutes')), ~ round(. / (Minutes / 90),4))) %>%
  mutate(Surname = str_extract(Player, "\\S+$"),
         NPG_A = NPG + A,
         xGdiff = G - xG,
         xNPGdiff = NPG - npxG,
         AxAGdiff = A - xAG,
         AxAdiff = A - xA,
         xAGxAdiff = xAG - xA,
         Pass_Completion = (Cmp_Total / Att_Total),
         LongPass_Completion = (Cmp_Long / (Att_Long)),
         Aerial_Rate = (Won_Aerial / (Won_Aerial + Lost_Aerial)),
         TakeOn_Rate = (Succ_Take / Att_Take)) %>%
  left_join(all_2016_2024_seasons %>% select(Player,player_id,Position,Squad,Comp,Nation,Born,Minutes) %>%
              modal_attributes(season = FALSE)) %>%
  select(Player, Position, Matches, Starts, Minutes, G, NPG, A, NPG_A, # Standard
         xG, xGdiff, npxG, xNPGdiff, Shots, SoT, Shot_Distance, FK_Shots, PK, PKatt, # Shooting
         xAG, xA, AxAGdiff, AxAdiff, xAGxAdiff, PrgP, Pass_Completion, Cmp_Total, Att_Total, TotDist_Total, PrgDist_Total, Cmp_Short, Att_Short, Cmp_Medium, Att_Medium, Cmp_Long, Att_Long, Final_Third_Pass, PPA, Cross_Penalty_Area, # Passing
         Live_Pass, Dead_Pass, FK_Pass, Through_Balls, Switches, Crosses, Throw_Ins, Corners, In_Corner, Out_Corner, Str_Corner, Offside_Passes, Blocked_Passes,  # Passing types
         Total_GCA, GCA_OpenPlay, PassLive_GCA, TO_GCA, Sh_GCA, Fld_GCA, Def_GCA, PassDead_GCA, Total_SCA, SCA_OpenPlay, PassLive_SCA, TO_SCA, Sh_SCA, Fld_SCA, Def_SCA, PassDead_SCA, # Goal-creating actions
         Tkl_Tackles, TklW_Tackles, Def_3rd_Tackles, Mid_3rd_Tackles, Att_3rd_Tackles, Tkl_Challenges, Att_Challenges, Lost_Challenges, Total_Blocks, Sh_Blocks, Pass_Blocks, Int, `Tkl+Int`, Recov, Clr, Err, # Defensive
         Touches, Def_Pen_Touches, Def_3rd_Touches, Mid_3rd_Touches, Att_3rd_Touches, Att_Pen_Touches, Live_Touches, TakeOn_Rate, Att_Take, Succ_Take, Tkld_Take, Total_Carries, TotDist_Carries, PrgDist_Carries, PrgC_Carries, Final_Third_Carries, CPA_Carries, Mis_Carries, Dis_Carries, Rec_Receiving, PrgR_Receiving, # Possession
         PKwon, PKcon, OG, Won_Aerial, Lost_Aerial, Fls, Fld, Off, CrdY, CrdY_2nd, CrdR,
         Nation, Born, Squad, Comp, player_id, Url) %>%
  mutate(Position = case_when(
    Player == "Kevin De Bruyne" ~ "Attacking midfielder",
    Player == "Mesut Özil" ~ "Attacking midfielder",
    Player == "Florian Wirtz" ~ "Attacking midfielder",
    Player == "Dele Alli" ~ "Attacking midfielder",
    Player == "Gonzalo Higuaín" ~ "Forward",
    Player == "Shinji Kagawa" ~ "Attacking midfielder",
    Player == "Martin Ødegaard" ~ "Attacking midfielder",
    Player == "Bruno Fernandes" ~ "Attacking midfielder",
    Player == "Domenico Berardi" ~ "Attacking midfielder",
    Player == "Christian Eriksen" ~ "Attacking midfielder",
    Player == "David Silva" ~ "Attacking midfielder",
    TRUE ~ Position  # Default case to leave Position unchanged if no condition is met
  ),
  Surname = str_extract(Player, "\\b[\\w-]+$"))


write_excel_csv(per90_1723,'data/2017 to 2023 per 90.csv')


###### VISUALISATION
library(ggrepel)
library(showtext)
showtext_auto()
font_add("Titillium Web", "C:/WINDOWS/FONTS/TITILLIUMWEB-REGULAR.TTF")
font_add("Titillium Web SemiBold", "C:/WINDOWS/FONTS/TITILLIUMWEB-SEMIBOLD.TTF")


# Add 'Grey' to the start or end of your list of unique positions
positions_colors <- c("Grey", "Defender", "Midfielder", "Attacking midfielder", "Forward")

# Then make a corresponding vector of colors
colors <- c("darkgrey", "#F3AA60", "#EF6262", "#2D791D", "#1D5B79")

### GRAPH: Open play SCA vs Dead-ball SCA (HORIZONTAL)
per90_1723_display <- read_csv('data/2017 to 2023 per 90 edit.csv')

ggplot(per90_1723_display %>% mutate(color =  ifelse(is.na(DisplayOSCAGCA), "Grey", Position)) %>%
         arrange(desc(color == "Grey")),
       aes(y = PassDead_SCA, x = SCA_OpenPlay, label = DisplayOSCAGCA, size = Minutes, col = color)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = setNames(colors, positions_colors)) +
  geom_text_repel(col = '#222222',
                  hjust = -0.15, vjust = 0.05,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9,
                  force = 2.25,
                  point.padding = .5) +
  scale_size(range = c(4.5, 16), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  scale_x_continuous(limits = c(1.75, 6.95), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.8), expand = c(0, 0.1)) +
  #geom_abline(slope = 1, intercept = 0) + 
  labs(x = 'Open play shot-creating actions per 90',
       y = 'Dead-ball shot-creating passes per 90',
       title = 'Best open play vs. dead-ball playmakers of this generation',
       subtitles = 'Players with at least 3,500 minutes played between 2017/18 and 2023/24. Stats from top 5 European leagues only. The larger the name, the more\nminutes played. Open play shot-creating actions are the last two passes, take-ons, shots, fouls drawn or defensive actions leading to a shot.') +
  football_theme() +
  theme(plot.title = element_text(size = 80, color = '#1D5B79',
                                        family = 'Titillium Web SemiBold',
                                        margin = margin(t = 30, b = 10)), # Adjust title size
        plot.subtitle = element_text(size = 32, color = '#222222',
                                           margin = margin(b = 20)) # Adjust subtitle size
  ) +      
  guides(size = 'none',
         col = 'none') -> plot_1723_sca_openplay_vs_deadball
ggsave("viz/1723_sca_openplay_vs_deadball.pdf", plot_1723_sca_openplay_vs_deadball, width = 40, height = 25,
       bg = 'transparent')
ggsave("viz/1723_sca_openplay_vs_deadball.png", plot_1723_sca_openplay_vs_deadball, width = 40, height = 25, dpi = 96,
       bg = 'transparent')

### GRAPH: Open play SCA vs Dead-ball SCA (VERTICLA)
per90_1723_display <- read_csv('data/2017 to 2023 per 90 edit.csv')

ggplot(per90_1723_display %>% mutate(color =  ifelse(is.na(DisplayOSCAGCA), "Grey", Position)) %>%
         arrange(desc(color == "Grey")),
       aes(x = PassDead_SCA, y = SCA_OpenPlay, label = DisplayOSCAGCA, size = Minutes, col = color)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = setNames(colors, positions_colors)) +
  geom_text_repel(col = '#222222',
                  hjust=-.075,vjust=0,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9) +
  scale_size(range = c(4, 14), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  scale_y_continuous(limits = c(1.75, 7), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 1.8), expand = c(0, 0.1)) +
  #geom_abline(slope = 1, intercept = 0) + 
  labs(y = 'Open play shot-creating actions per 90',
       x = 'Dead-ball shot-creating actions per 90',
       title = 'Best open play and dead-ball playmakers since 2017 ',
       subtitles = 'Players between 2017/18 and 2023/24 with at least 3,500 minutes played. Names sized by minutes played.\nOpen play shot-creating actions are the last two passes, take-ons, shots, fouls drawn or defensive actions leading to a shot.') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_1723_sca_openplay_vs_deadball_vertical
ggsave("viz/1723_sca_openplay_vs_deadball vertical.pdf", plot_1723_sca_openplay_vs_deadball_vertical , width = 25, height = 29,
       bg = 'transparent')


### GRAPH: SCA Open plays
ggplot(per90_1723_display %>% mutate(color =  ifelse(is.na(DisplayOSCA), "Grey", Position)) %>%
         arrange(desc(color == "Grey")),
       aes(x = PassLive_SCA, y = TO_SCA+Sh_SCA+Fld_SCA+Def_SCA, label = DisplayOSCA, size = Minutes, col = color)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = setNames(colors, positions_colors)) +
  geom_text_repel(col = '#222222',
                  hjust=-.075,vjust=0,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9) +
  scale_size(range = c(4, 14), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  #scale_x_continuous(limits = c(1.5, 5.1), expand = c(0, 0)) +
  #scale_y_continuous(limits = c(0, 2.4), expand = c(0, 0)) +
  #geom_abline(slope = 1, intercept = 0) + 
  labs(x = 'Live ball shot-creating actions per 90',
       y = 'Other open-play shot-creating actions per 90',
       title = 'Best playmakers from open play since 2017',
       subtitles = 'All players between 2017/18 and 2023/24 with at least 3,500 minutes played. Points scaled by minutes played.') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_1723_sca_open_play
ggsave("viz/1723_sca_pass vs other open play.pdf", plot_1723_sca_open_play, width = 25, height = 29,
       bg = 'transparent')


### GRAPH: Box-to-box (midfielders)

ggplot(per90_1723_display %>% mutate(color =  ifelse(is.na(DisplayB2B), "Grey", Position)) %>%
         arrange(desc(color == "Grey")) %>% filter(Position %in% c('Midfielder','Attacking midfielder')),
       aes(x = `Tkl+Int`, y = SCA_OpenPlay, label = DisplayB2B, size = Minutes, col = color)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = setNames(colors, positions_colors)) +
  geom_text_repel(col = '#222222',
                  hjust=-.075,vjust=0,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9) +
  scale_size(range = c(4, 14), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  #scale_x_continuous(limits = c(1.5, 5.1), expand = c(0, 0)) +
  #scale_y_continuous(limits = c(0, 2.4), expand = c(0, 0)) +
  #geom_abline(slope = 1, intercept = 0) + 
  labs(x = 'Tackles + interceptions per 90',
       y = 'Open play shot-creating actions per 90',
       title = 'Most box-to-box action from midfield',
       subtitles = 'All central and attacking midfielders between 2017/18 and 2023/24 with at least 3,500 minutes played. Names sized by minutes played.') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_1723_box_to_box
ggsave("viz/1723_box_to_box.pdf", plot_1723_box_to_box, width = 25, height = 29,
       bg = 'transparent')

### GRAPH: Box-to-box (attackers)

ggplot(per90_1723_display %>% mutate(color =  ifelse(is.na(DisplayB2B), "Grey", Position)) %>%
         arrange(desc(color == "Grey")) %>% filter(Position %in% c('Forward','Attacking midfielder')),
       aes(x = `Tkl+Int`, y = SCA_OpenPlay, label = DisplayB2B, size = Minutes, col = color)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = setNames(colors, positions_colors)) +
  geom_text_repel(col = '#222222',
                  hjust=-.075,vjust=0,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9) +
  scale_size(range = c(4, 14), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  #scale_x_continuous(limits = c(1.5, 5.1), expand = c(0, 0)) +
  #scale_y_continuous(limits = c(0, 2.4), expand = c(0, 0)) +
  #geom_abline(slope = 1, intercept = 0) + 
  theme_minimal() +
  labs(x = 'Tackles + interceptions per 90',
       y = 'Open play shot-creating actions per 90',
       title = 'Most box-to-box action from attackers',
       subtitles = 'All forwards and attacking midfielders between 2017/18 and 2023/24 with at least 3,500 minutes played. Names sized by minutes played.') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_1723_box_to_box_attackers
ggsave("viz/1723_box_to_box_attackers.pdf", plot_1723_box_to_box_attackers, width = 25, height = 29,
       bg = 'transparent')

### GRAPH: A vs xA

ggplot(per90_1723_display %>% mutate(color =  ifelse(is.na(DisplayOSCAGCA), "Grey", Position)) %>%
         arrange(desc(color == "Grey")),
       aes(x = xA, y = A, label = DisplayOSCAGCA, size = Minutes, col = color)) +
  geom_abline(intercept = 0, slope = 1, color = '#1D5B79') +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = setNames(colors, positions_colors)) +
  geom_text_repel(col = '#222222',
                  hjust=-.075,vjust=0,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9) +
  scale_size(range = c(4, 14), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  scale_x_continuous(limits = c(.15, 0.45), expand = c(0, 0)) +
  #scale_y_continuous(limits = c(0, .95), expand = c(0, 0.01)) +
  #geom_abline(slope = 1, intercept = 0) + 
  theme_minimal() +
  labs(x = 'Expected assists per 90',
       y = 'Assists per 90',
       title = 'Real vs expected assists per 90',
       subtitles = 'All players between 2017/18 and 2023/24 with at least 3,500 minutes played. Points scaled by minutes played.') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_1723_A_xA
ggsave("viz/1723_A_xA.pdf", plot_1723_A_xA, width = 25, height = 29,
       bg = 'transparent')

### GRAPH: Long balls vs long ball completion
per90_1723_display <- read_csv('data/2017 to 2024 per 90 edit.csv')

ggplot(per90_1723_display %>% mutate(color =  ifelse(is.na(DisplayLong), "Grey", Position)) %>%
         arrange(desc(color == "Grey")) %>% filter(Position == 'Midfielder'),
       aes(y = Cmp_Long, x = round((Cmp_Long/Att_Long)*100,1), label = DisplayLong, size = Minutes, col = color)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = setNames(colors, positions_colors)) +
  geom_text_repel(col = '#222222',
                  hjust=-.075,vjust=0,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9) +
  scale_size(range = c(4, 14), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  #scale_x_continuous(limits = c(0, 0.5), expand = c(0, 0)) +
  #scale_y_continuous(limits = c(0, .95), expand = c(0, 0.01)) +
  #geom_abline(slope = 1, intercept = 0) + 
  theme_minimal() +
  labs(x = 'Long ball accuracy %',
       y = 'Completed long balls per 90',
       title = 'Who have been the best long passers since 2017?',
       subtitles = 'All players between 2017/18 and 2022/23 with at least 3,500 minutes played.\nPoints scaled by minutes played.') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_1723_longball
ggsave("viz/1723_longball.pdf", plot_1723_longball, width = 25, height = 29,
       bg = 'transparent')
