###### 1992-2023 summed analysis #######

# Leagues have data for...
## Prem 1992/93 onwards
## Serie A from 1998/99 onwards
## La Liga, Bundesliga and Ligue 1 from 1999/00 onwards


### Creating summed dataset
# Summing the 1992/93 - 2023/24 seasons data into one row per player
all_summed <- all_seasons %>% select(Player,player_id,Position,Squad,Comp,Nation,Born,Age,Matches,Starts,Minutes,G,NPG,A,PK,PKatt,CrdY,CrdR) %>%
  filter(!is.na(Born)) %>%
  group_by(Player,player_id,Born) %>%
  summarise(across(c(Minutes,G,NPG,A,PK,PKatt,CrdY,CrdR), sum, na.rm = TRUE),
            minAge = min(Age, na.rm = TRUE),
            .groups = 'drop') %>%
  left_join(all_seasons %>% select(Player,player_id,Position,Squad,Comp,Nation,Born,Age,Minutes) %>%
              modal_attributes(season = FALSE)) %>%
  # Manually correcting player positions
  mutate(Position = case_when(
    Player == "Mehmet Scholl" ~ "Attacking midfielder",
    Player == "Kevin De Bruyne" ~ "Attacking midfielder",
    Player == "Mesut Özil" ~ "Attacking midfielder",
    Player == "Javier de Pedro" ~ "Attacking midfielder",
    Player == "Florian Wirtz" ~ "Attacking midfielder",
    Player == "Dele Alli" ~ "Attacking midfielder",
    Player == "Gonzalo Higuaín" ~ "Forward",
    Player == "Shinji Kagawa" ~ "Attacking midfielder",
    Player == "Juninho Pernambucano" ~ "Attacking midfielder",
    Player == "Martin Ødegaard" ~ "Attacking midfielder",
    Player == "Burno Fernandes" ~ "Attacking midfielder",
    Player == "Domenico Berardi" ~ "Attacking midfielder",
    Player == "Christian Eriksen" ~ "Attacking midfielder",
    Player == "Zvjezdan Misimović" ~ "Attacking midfielder",
    # Add more correction as needed
    TRUE ~ Position  # Default case to leave Position unchanged if no condition is met
  ))

write_excel_csv(all_summed,'data/1992 to 2023 summed.csv')


### Created per 90 dataset
all_per90 <- all_summed %>%
  filter(Minutes > 7000) %>%
  mutate(across(c(G,NPG,A,PK,PKatt,CrdY,CrdR), ~ round(. / (Minutes / 90),4))) %>%
  mutate(Surname = str_extract(Player, "\\S+$"),
         NPG_A = NPG + A)


write_excel_csv(all_per90,'data/1992 to 2023 per 90.csv')


### GRAPH: NPG per 90 vs xA
library(ggrepel)
library(showtext)
showtext_auto()
font_add("Titillium Web", "C:/WINDOWS/FONTS/TITILLIUMWEB-REGULAR.TTF")
font_add("Titillium Web SemiBold", "C:/WINDOWS/FONTS/TITILLIUMWEB-SEMIBOLD.TTF")


# Add 'Grey' to the start or end of your list of unique positions
positions_colors <- c("Grey", "Defender", "Midfielder", "Attacking midfielder", "Forward")

# Then make a corresponding vector of colors
colors <- c("darkgrey", "#F3AA60", "#EF6262", "#2D791D", "#1D5B79")


all_per90_display <- read_csv('data/1992 to 2023 per 90 display.csv')

ggplot(all_per90_display %>% mutate(Display = if_else(!is.na(Display) & minAge >= 27, paste0(Display, "*"), Display),
        color =  ifelse(is.na(Display), "Grey", Position)) %>%
         arrange(desc(color == "Grey")),
       aes(x = A, y = NPG, label = Display, size = Minutes, col = color)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = setNames(colors, positions_colors)) +
  geom_text_repel(col = '#222222',
                  hjust = -0.15, vjust = 0.05,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9,
                  force = 2,
                  point.padding = .5) +
  scale_size(range = c(4, 15), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  scale_x_continuous(limits = c(0, 0.5), expand = c(0, 0.005)) +
  scale_y_continuous(limits = c(0, .87), expand = c(0, 0.01)) +
  #geom_abline(slope = 1, intercept = 0) + 
  labs(x = 'Assists per 90',
       y = 'Non-penalty Goals per 90',
       title = 'Who have been the most productive players\nsince the 1990s?',
       subtitles = 'All players since the 1990s by non-penalty goals and assists with at\nleast 7,000 minutes played. Points scaled by minutes played.') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_9223_xa_npg
ggsave("viz/9223 a vs npg.pdf", plot_9223_xa_npg, width = 25, height = 29,
       bg = 'transparent')
ggsave("viz/9223 a vs npg.png", plot_9223_xa_npg, width = 25, height = 29, dpi = 96,
       bg = 'transparent')


### GRAPH: NPG per 90 vs xA (coloured by age)
# Plot
ggplot(all_per90_display %>% mutate(color =  ifelse(is.na(Display), NA, Born)) %>%
         arrange(desc(color == "Grey")),
       aes(x = A, y = NPG, label = Display, size = Minutes, col = color)) +
  geom_point(alpha = 0.5) +
  #scale_color_gradient(low = "purple", high = "yellow", na.value = "grey") +
  scale_color_viridis_c(na.value = "grey") +
  geom_text_repel(col = '#222222',
                  hjust=-.075,vjust=0,
                  show.legend = FALSE,
                  family = 'Titillium Web',
                  alpha = 0.9) +
  scale_size(range = c(4, 14), name="Minutes played") +
  #xlim(0,0.6) +
  #ylim(0,1) +
  scale_x_continuous(limits = c(0, 0.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, .95), expand = c(0, 0.01)) +
  #geom_abline(slope = 1, intercept = 0) + 
  labs(x = 'Assists per 90',
       y = 'Non-penalty Goals per 90',
       title = 'Who have been the most productive players\nsince the 1990s?',
       subtitles = 'All players since the 1990s by non-penalty goals and assists\nwith at least 3,500 minutes played. Points scaled by minutes played.',
       caption = 'Data from FBRef between 1992/3 and 2023/24 seasons (depending on the league).\n@tigran_football   /u/Jinshanling') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_9224_xa_npg_age
ggsave("viz/9224 a vs npg age.pdf", plot_9224_xa_npg_age, width = 25, height = 29,
       bg = 'transparent')
