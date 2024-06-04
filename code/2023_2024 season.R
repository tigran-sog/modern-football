##### 2023/24 data collected on 03/06/24 #######

### Theme
football_theme <- function() {
  theme_minimal() +
    theme(
      panel.grid.major = element_line(color = '#ebebeb', size = 1), # Adjust gridline thickness here
      panel.grid.minor = element_line(color = '#ebebeb', size = 0), # Adjust minor gridline thickness here
      text = element_text(family = 'Titillium Web'),
      plot.caption = element_text(size = 20),
      plot.title = element_text(size = 70, color = '#1D5B79',
                                family = 'Titillium Web SemiBold',
                                margin = margin(t = 30, b = 10)), # Adjust title size
      plot.subtitle = element_text(size = 35, color = '#222222',
                                   margin = margin(b = 20)), # Adjust subtitle size
      axis.title.x = element_text(size = 40, margin = margin(t = 20, b = 20)), # Adjust X axis title size and margin
      axis.title.y = element_text(size = 40, margin = margin(r = 20, l = 20)), # Adjust Y axis title size and margin
      axis.text = element_text(size = 35, vjust = .5),
      axis.line = element_line(linewidth = .5, color = '#1D5B79'),  # adjust line size as needed
      legend.title = element_text(size = 35), # adjust size as needed
      legend.text = element_text(size = 35)  # adjust size as needed
    )
}


### Created per 90 dataset
all_2023_24_season_per90 <- all_2023_24_season %>%
  filter(Minutes >= 1000) %>%
  mutate(across(where(is.numeric) & !all_of(c('player_id','Born','Matches','Starts','Minutes')), ~ round(. / (Minutes / 90),4))) %>%
  mutate(Surname = str_extract(Player, "\\S+$"),
         NPG_A = NPG + A,
         xGdiff = G - xG,
         xNPGdiff = NPG - npxG,
         AxAGdiff = A - xAG,
         AxAdiff = A - xA,
         xAGxAdiff = xAG - xA,
         Pass_Completion = (Cmp_Total / Att_Total) * 100,
         TakeOn_Rate = (Succ_Take / Att_Take) * 100,
         NPG_xA = NPG + xA) %>%
  select(Player, Position, Matches, Starts, Minutes, G, NPG, A, NPG_A, # Standard
         xG, xGdiff, npxG, xNPGdiff, Shots, SoT, Shot_Distance, FK_Shots, PK, PKatt, # Shooting
         xAG, xA, AxAGdiff, AxAdiff, xAGxAdiff, PrgP, Pass_Completion, Cmp_Total, Att_Total, TotDist_Total, PrgDist_Total, Cmp_Short, Att_Short, Cmp_Medium, Att_Medium, Cmp_Long, Att_Long, Final_Third_Pass, PPA, Cross_Penalty_Area, # Passing
         Live_Pass, Dead_Pass, FK_Pass, Through_Balls, Switches, Crosses, Throw_Ins, Corners, In_Corner, Out_Corner, Str_Corner, Offside_Passes, Blocked_Passes,  # Passing types
         Total_GCA, GCA_OpenPlay, PassLive_GCA, TO_GCA, Sh_GCA, Fld_GCA, Def_GCA, PassDead_GCA, Total_SCA, SCA_OpenPlay, PassLive_SCA, TO_SCA, Sh_SCA, Fld_SCA, Def_SCA, PassDead_SCA, # Goal-creating actions
         Tkl_Tackles, TklW_Tackles, Def_3rd_Tackles, Mid_3rd_Tackles, Att_3rd_Tackles, Tkl_Challenges, Att_Challenges, Lost_Challenges, Total_Blocks, Sh_Blocks, Pass_Blocks, Int, `Tkl+Int`, Recov, Clr, Err, # Defensive
         Touches, Def_Pen_Touches, Def_3rd_Touches, Mid_3rd_Touches, Att_3rd_Touches, Att_Pen_Touches, Live_Touches, TakeOn_Rate, Att_Take, Succ_Take, Tkld_Take, Total_Carries, TotDist_Carries, PrgDist_Carries, PrgC_Carries, Final_Third_Carries, CPA_Carries, Mis_Carries, Dis_Carries, Rec_Receiving, PrgR_Receiving, # Possession
         PKwon, PKcon, OG, Won_Aerial, Lost_Aerial, Fls, Fld, Off, CrdY, CrdY_2nd, CrdR,
         Nation, Born, Squad, Comp, player_id, Url, NPG_xA, Surname)

write_excel_csv(all_2023_24_season_per90,'data/2023 24 per 90.csv')

### GRAPH: NPG per 90 vs xA
library(ggrepel)
library(showtext)
showtext_auto()
font_add("Titillium Web", "C:/WINDOWS/FONTS/TITILLIUMWEB-REGULAR.TTF")
font_add("Titillium Web SemiBold", "C:/WINDOWS/FONTS/TITILLIUMWEB-SEMIBOLD.TTF")


# Add 'Grey' to the start or end of your list of unique positions
positions_colors <- c("Grey", "Defender", "Attacking midfielder", "Forward", "Midfielder")
league_colors <- c("Grey", "La Liga", "Ligue 1", "Serie A", "Premier League","Bundesliga")

# Then make a corresponding vector of colors
colors <- c("darkgrey", "#F3AA60",  "#2D791D", "#1D5B79", "#EF6262")
colors <- c("darkgrey", "#ac92eb", "#EF6262", "#2D791D", "#1D5B79")
colors <- c("grey", "#eb92d9",  "#2D791D", "#1D5B79", "#EF6262")

colors_league <- c("grey", "#F3AA60", "#d466be", "#EF6262", "#2D791D", "#1D5B79")
colors_league <- c("grey", "#F3AA60", "#eb92d9",  "#2D791D", "#1D5B79", "#EF6262")


full_2324_per90_display <- read_csv('data/2023 24 per 90 display.csv')

ggplot(full_2324_per90_display %>% mutate(color =  ifelse(is.na(Display2324), "Grey", Position)) %>%
         arrange(desc(color == "Grey")),
       aes(x = xA, y = NPG, label = Display2324, size = Minutes, col = color)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = setNames(colors, positions_colors)) +
  geom_text_repel(col = '#222222',
                  hjust = -0.15, vjust = 0.05,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9,
                  force = 2,
                  point.padding = .5) +
  scale_size(range = c(6, 14), name="Minutes played") +
  #xlim(0,0.54) +
  #ylim(0,0.101) +
  scale_x_continuous(limits = c(0, 0.56), expand = c(0, 0.0075)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.02)) +
  #geom_abline(slope = 1, intercept = 0) + 
  labs(x = 'Expected Assists per 90',
       y = 'Non-penalty Goals per 90',
       title = 'Who were the biggest attacking threats\nof the 2023/24 season?',
       subtitles = 'All players in the top 5 leagues by non-penalty goals and expected assists per 90 minutes.\nPoints scaled by minutes played (minimum 1,000 minutes).') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_2324_xa_npg
ggsave("viz/2324 xa vs npg.pdf", plot_2324_xa_npg, width = 25, height = 29,
       bg = 'transparent')
ggsave("viz/2324 xa vs npg.png", plot_2324_xa_npg, width = 25, height = 29, dpi = 96,
       bg = 'transparent')


### GRAPH: GCA vs SCA
full_2324_per90_display <- read_csv('data/2023 24 per 90 display.csv')

ggplot(full_2324_per90_display %>% mutate(color =  ifelse(is.na(DisplayNPGSCA), "Grey", Position)) %>%
         arrange(desc(color == "Grey")) %>% filter(Minutes >= 800),
       aes(x = SCA_OpenPlay, y = NPG, label = DisplayNPGSCA, size = Minutes, col = color)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = setNames(colors, positions_colors)) +
  geom_text_repel(col = '#222222',
                  hjust=-.2,vjust=0,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9,
                  force = 2) +
  scale_size(range = c(5, 15), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  scale_x_continuous(limits = c(1.75, 6.99), expand = c(0, 0.02)) +
  scale_y_continuous(limits = c(0, 1.1), expand = c(0, 0.02)) +
  #geom_abline(slope = 1, intercept = 0) + 
  labs(x = 'Open play shot-creating actions per 90',
       y = 'Non-penalty Goals per 90',
       title = 'Which players have been the most involved in open play attacks\nin the 2023/24 season so far?',
       subtitles = 'Players in the top 5 leagues by non-penalty goals and open play shot-creating actions (the last two passes, dribbles, fouls drawn, shots or defensive\nactions before a shot). Points scaled by minutes played (minimum 700 minutes).') +
  football_theme() +
  guides(size = 'none', # Hide the size legend
         col = 'none') -> plot_2324_sca
ggsave("viz/2324 sca.pdf", plot_2324_sca, width = 36, height = 24,
       bg = 'transparent')
ggsave("viz/2324 sca.png", plot_2324_sca, width = 36, height = 24, dpi = 96,
       bg = 'transparent')

### GRAPH: BOX TO BOX (MIDFIELD)
full_2324_per90_display <- read_csv('data/2023 24 per 90 display.csv')

ggplot(full_2324_per90_display %>% mutate(color =  ifelse(is.na(DisplayBox), "Grey", Comp)) %>%
         arrange(desc(color == "Grey")) %>% filter(Position == 'Midfielder'),
       aes(x = `Tkl+Int`, y = SCA_OpenPlay, label = DisplayBox, size = Minutes, col = color)) +
  geom_vline(xintercept = full_2324_per90_display %>% filter(Position == 'Midfielder') %>% pull(`Tkl+Int`) %>% mean(),
             linetype="dashed", color = "darkgrey", alpha = 1, size = 1) +
  geom_hline(yintercept = full_2324_per90_display %>% filter(Position == 'Midfielder') %>% pull(SCA_OpenPlay) %>% mean(),
             linetype="dashed", color = "darkgrey", alpha = 1, size = 1) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = setNames(colors_league, league_colors)) +
  geom_text_repel(col = '#222222',
                  hjust=-.075,vjust=0,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9) +
  scale_size(range = c(6, 14), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  #scale_x_continuous(limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.5, 5.4), expand = c(0, 0.01)) +
  #geom_abline(slope = 1, intercept = 0) + 
  labs(y = 'Open play shot-creating actions per 90',
       x = 'Tackles plus interceptions per 90',
       title = 'Midfielders with the most box-to-box actions\nin the 2023/24 season so far',
       subtitles = 'All central midfielders in the top 5 leagues by open play shot-creating actions (the last two passes, dribbles,\nfouls drawn, shots or defensive actions before a shot) and tackles plus interceptions per 90.\nPoints scaled by minutes played (minimum 700 minutes).') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_2324_boxtobox

ggsave("viz/2324 boxtobox.pdf", plot_2324_boxtobox, width = 25, height = 29,
       bg = 'transparent')
ggsave("viz/2324 boxtobox.png", plot_2324_boxtobox, width = 25, height = 29, dpi = 96,
       bg = 'transparent')


### GRAPH: PROGRESSIVE PASSES VS CARRIES (MIDFIELD)
full_2324_per90_display <- read_csv('data/2023 24 per 90 display.csv')

ggplot(full_2324_per90_display %>% mutate(color =  ifelse(is.na(DisplayProg), "Grey", Comp)) %>%
         arrange(desc(color == "Grey")) %>% filter(Position == 'Midfielder'),
       aes(x = PrgC_Carries, y = PrgP, label = DisplayProg, size = Minutes, col = color)) +
  geom_vline(xintercept = full_2324_per90_display %>% filter(Position == 'Midfielder') %>% pull(`PrgC_Carries`) %>% mean(),
             linetype="dashed", color = "darkgrey", alpha = 1, size = 1) +
  geom_hline(yintercept = full_2324_per90_display %>% filter(Position == 'Midfielder') %>% pull(PrgP) %>% mean(),
             linetype="dashed", color = "darkgrey", alpha = 1, size = 1) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = setNames(colors_league, league_colors)) +
  geom_text_repel(col = '#222222',
                  hjust=-.075,vjust=0,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9,
                  force = 2) +
  scale_size(range = c(6, 14), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  scale_x_continuous(limits = c(0, 4.5), expand = c(0, 0)) +
  #scale_y_continuous(limits = c(0, .95), expand = c(0, 0.01)) +
  #geom_abline(slope = 1, intercept = 0) + 
  labs(y = 'Progressive passes per 90',
       x = 'Progressive carries per 90',
       title = 'Who have been the most progressive midfielders\nin the 2023/24 season so far?',
       subtitles = 'All central midfielders in the top 5 leagues by progressive passes and carries per 90.\nPoints scaled by minutes played (minimum 700 minutes).') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_2324_progmid

ggsave("viz/2324 progmid.pdf", plot_2324_progmid, width = 25, height = 29,
       bg = 'transparent')
ggsave("viz/2324 progmid.png", plot_2324_progmid, width = 25, height = 29, dpi = 96,
       bg = 'transparent')

