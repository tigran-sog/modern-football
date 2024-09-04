###### 1992-2023 summed analysis #######

# Leagues have data for...
## Prem 1992/93 onwards
## Serie A from 1998/99 onwards
## La Liga, Bundesliga and Ligue 1 from 1999/00 onwards



### Creating summed dataset
# Summing the 1992/93 - 2024/25 seasons data into one row per player
all_summed_prem <- all_seasons_prem %>% select(Player,player_id,Position,Squad,Comp,Nation,Born,Age,Matches,Starts,Minutes,G,NPG,A,PK,PKatt,CrdY,CrdR) %>%
  filter(!is.na(Born)) %>%
  group_by(Player,player_id,Born) %>%
  summarise(across(c(Minutes,G,NPG,A,PK,PKatt,CrdY,CrdR), sum, na.rm = TRUE),
            minAge = min(Age, na.rm = TRUE),
            .groups = 'drop') %>%
  left_join(all_seasons_prem %>% select(Player,player_id,Position,Squad,Comp,Nation,Born,Age,Minutes) %>%
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

write_excel_csv(all_summed_prem,'data/1992 to 2024 summed prem.csv')


### Created per 90 dataset
all_per90_prem <- all_summed_prem %>%
  filter(Minutes > 3000) %>%
  mutate(across(c(G,NPG,A,PK,PKatt,CrdY,CrdR), ~ round(. / (Minutes / 90),4))) %>%
  mutate(Surname = str_extract(Player, "\\S+$"),
         NPG_A = NPG + A)


write_excel_csv(all_per90_prem,'data/1992 to 2024 per 90 prem.csv')


### GRAPH: NPG per 90 vs xA
library(ggrepel)
library(showtext)
showtext_auto()
font_add("Titillium Web", "C:/WINDOWS/FONTS/TITILLIUMWEB-REGULAR.TTF")
font_add("Titillium Web SemiBold", "C:/WINDOWS/FONTS/TITILLIUMWEB-SEMIBOLD.TTF")


# Add 'Grey' to the start or end of your list of unique positions
positions_colors <- c("Grey", "Defender", "Midfielder", "Attacking midfielder", "Forward")

# Then make a corresponding vector of colors
colors <- c("#D3D3D3", "#F3AA60", "#EF6262", "#2D791D", "#1D5B79")

# Re-import dataset with highlighted surnames for labelling
all_per90_display_prem <- read_csv('data/1992 to 2024 per 90 prem display.csv')

# Plot
ggplot(all_per90_display_prem %>% mutate(Display = if_else(!is.na(Display) & minAge >= 27, paste0(Display, "*"), Display),
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
                  force = 1.5,
                  point.padding = .5) +
  scale_size(range = c(3.5, 15.5), name="Minutes played") +
  scale_x_continuous(limits = c(0, 0.51), expand = c(0, 0.005)) +
  scale_y_continuous(limits = c(0, .89), expand = c(0, 0.01)) +
  labs(x = 'Assists per 90',
       y = 'Non-penalty Goals per 90',
       title = 'Who have been the most productive players\nin Premier League history',
       subtitles = 'All players with at least a season\'s game time (3,500 minutes). The larger the name, the more minutes played.') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_9224_xa_npg_prem
ggsave("viz/9224 a vs npg prem.pdf", plot_9224_xa_npg_prem, width = 25, height = 29,
       bg = 'transparent')
ggsave("viz/9224 a vs npg prem.png", plot_9224_xa_npg_prem, width = 25, height = 29, dpi = 96,
       bg = 'transparent')

# Plot (small title)
ggplot(all_per90_display_prem %>% mutate(Display = if_else(!is.na(Display) & minAge >= 27, paste0(Display, "*"), Display),
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
                  force = 1.25,
                  point.padding = .5) +
  scale_size(range = c(4.5, 16), name="Minutes played") +
  scale_x_continuous(limits = c(0, 0.51), expand = c(0, 0.005)) +
  scale_y_continuous(limits = c(0, .89), expand = c(0, 0.01)) +
  labs(x = 'Assists per 90',
       y = 'Non-penalty Goals per 90') +
  football_theme() +
  guides(size = 'none',
         col = 'none') -> plot_9224_xa_npg_prem_no_title
ggsave("viz/9224 a vs npg prem no title.pdf", plot_9224_xa_npg_prem_no_title, width = 25, height = 29,
       bg = 'transparent')
ggsave("viz/9224 a vs npg prem no title.png", plot_9224_xa_npg_prem_no_title, width = 25, height = 29, dpi = 96,
       bg = 'transparent')
