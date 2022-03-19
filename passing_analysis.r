## Getting R packages ------------------------------------

library(ggplot2)
library(dplyr)
library(devtools)
library(remotes)
library(SDMTools)
library(tidyverse)
library(StatsBombR)
library(FC.rSTATS)
library(cowplot)
library(soccermatics)
library(ggsoccer)
library(SBpitch)
library(formattable)
library(RColorBrewer)
library(png)
library(ggrepel)
library(extrafont)
library(janitor)
library(ggtext)
library(ggExtra)
library(hexbin)
font_import()
loadfonts(device = "win")
# fonts() to check fonts available

## reading in functions --------------------------------------------------------
source('functions.r')

## Getting England data --------------------------------------------------------

euros_data <- FreeCompetitions() %>% filter(competition_name == "UEFA Euro")

euros_matches <- FreeMatches(euros_data)

euros_matches <- euros_matches %>% 
  mutate(england_match = ifelse(home_team.country.name == "England" | away_team.country.name == "England", 1, 0))

england_euros_matches <- euros_matches %>% filter(england_match == 1)


## England Vs. Croatia ------------------------------------

england_croatia_match <- england_euros_matches %>% filter(match_week == 1)

england_croatia_match <- StatsBombFreeEvents(england_croatia_match)

england_croatia_match <- allclean(england_croatia_match)

england_croatia_match$location.y.inverse <- 80 - england_croatia_match$location.y

england_croatia_match$pass.end_location.y.inverse <- 80 - england_croatia_match$pass.end_location.y

eng_player_df <- england_croatia_match %>% filter(team.name == "England")

match1_eng_dfs <- adapted_soccerPassMap(eng_player_df, minPass = 3, player.x.location = "location.x", 
                                  player.y.location = "location.y.inverse")

eng_edges_match1 <- match1_eng_dfs[[1]] 
eng_nodes_match1 <- match1_eng_dfs[[2]]

## Xg timeline -----------------------------------------------------------------

eng_croatia_xg_timeline <- england_croatia_match %>% soccerxGTimeline(homeCol = "red", awayCol = "blue", y_buffer = 0.4)

## England passing dataframe ---------------------------------------------------

passes <- eng_player_df %>% 
  filter(type.name == "Pass") %>% 
  group_by(pass.outcome.name) %>% 
  tally() %>% 
  filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown")) %>% 
  mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete"))

pass_n <- sum(passes$n)
pass_pc <- passes[passes$pass.outcome.name == "Complete",]$n / pass_n * 100

## England passing network vs. Croatia -----------------------------------------
# Only includes passes until first substitution was made

engl_croatia_pass_network <- ggplot() +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 dimensions = pitch_statsbomb) +
  geom_segment(data = eng_edges_match1, aes(x, y, xend = xend, yend = yend, size = n/2 ),
               col = "#3F95F7", alpha = 0.9, show.legend = F) +
  geom_point(data = eng_nodes_match1, aes(x, y, size = events, color = "red", fill = events), pch = 21, show.legend = F) + 
  scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_size_identity() + 
  guides(size = "none") +
  theme_pitch() + 
  ggtitle("England vs Croatia, 13-06-2021 Euro 2020") + 
  theme(panel.background = element_rect(fill = "springgreen4"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(hjust = 0.15, vjust = -10, colour = 'white')) +
  coord_cartesian(clip = "on") + 
  annotate("text", 104, 1, label = paste0("Passes: ", pass_n, "\nCompleted: ", sprintf("%.1f", pass_pc), "%"), 
           hjust = 1, vjust = 0, size = 4 * 7/8, col = "black") + 
  geom_label_repel(data = eng_nodes_match1, aes(x, y, label = name), size = 2.5) 

## Deeper analysis of England passing vs. Croatia ------------------------------

eng_passes_to_from <- england_croatia_match %>% filter(team.name == "England")  %>% filter(type.name == 'Pass') %>% 
  mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete")) %>%
  filter(type.name == "Pass" & pass.outcome.name == "Complete") %>% 
  select(from = player.name, to = pass.recipient.name) %>% 
  group_by(from, to) %>% 
  dplyr::summarise(passes = n()) %>% 
  stats::na.omit() %>% 
  arrange(desc(passes))

eng_passes <- eng_passes_to_from %>% group_by(from) %>%
  summarise(passes = sum(passes, na.rm = T)) %>% 
  arrange(desc(passes))

## Progressive England passes vs. Croatia --------------------------------------

passing_analysis_game1 <- england_croatia_match %>% filter(team.name == 'England') %>% filter(type.name == 'Pass') %>%
  # code gets the completed passes of the england team by replacing na with complete
  mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete")) %>%
  filter(type.name == "Pass" & pass.outcome.name == "Complete") %>% 
  # then we see how far the ball travelled vertically and horizontally on the pitch
  mutate(location.x.moved = pass.end_location.x - location.x,
         location.y.moved = pass.end_location.y.inverse - location.y.inverse,
         in.own.half = ifelse(location.x <= 60, 1, 0),
         #pass.end.own.half = ifelse(pass.end_location.x > 60, 1, 0),
         #in.opposition.half = ifelse(location.x > 60, 1, 0),
         pass.end.opposition.half = ifelse(pass.end_location.x > 60, 1, 0),
         pass.progress.x = (120 - location.x) - (120 - pass.end_location.x),
         pass.progress.y = abs(40 - location.y.inverse) - abs(40 - pass.end_location.y.inverse),
         ball.movement = round(pass.progress.x + pass.progress.y,2),
         # then categorised depending on where the ball was on the pitch to begin with
         progressive.pass.category = case_when(
           in.own.half == 1 & pass.end.opposition.half == 0 & ball.movement >= 30 ~ 'category.1',
           in.own.half == 1 & pass.end.opposition.half == 1 & ball.movement >= 15 ~ 'category.2',
           in.own.half == 0 & pass.end.opposition.half == 1 & ball.movement >= 10 ~ 'category.3',
           TRUE ~ 'NA'
         )) %>% 
  select(player.name, pass.recipient.name,
         in.own.half, 
         #pass.end.own.half, in.opposition.half, 
         pass.end.opposition.half,
         location.x, location.y.inverse,
         pass.end_location.x,
         pass.end_location.y.inverse,
         location.x.moved, location.y.moved,
         pass.progress.x, pass.progress.y, ball.movement,
         progressive.pass.category
         )

## progressive passes

passing_analysis_game1 <- progressive_pass_assess(passing_analysis_game1)

## England passing map ---------------------------------------------------------

eng_passes_vs_croatia <-  ggplot() +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 dimensions = pitch_statsbomb) +
  geom_point(data = passing_analysis_game1, aes(x = location.x, y = location.y.inverse), 
             color = "red", alpha = 0.4, size = 6) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  labs(subtitle = "A clear focus on the left side of the pitch, but also noticeably fewer passes around the central part of the 18 yard area.",
       title = "All England passes vs. Croatia, by location of where pass made")


## Progressive passes by player ------------------------------------------------

passing_analysis_game1 <- passing_analysis_game1 %>% 
  mutate(progressive.pass = if_else(progressive.pass.category %in% c("category.1","category.2","category.3"), 1, 0))

passing_analysis_game1 %>% filter(progressive.pass == 1) %>%
  mutate(pass_n = 1) %>% 
  pivot_wider(names_from = progressive.pass.category, values_from = pass_n) %>% 
  group_by(player.name) %>% 
  summarise(progressive.passes = sum(category.1, na.rm = TRUE) 
            + sum(category.2, na.rm = TRUE)
            + sum(category.3, na.rm = TRUE)) %>% 
  arrange(desc(progressive.passes))

## progressive passing map, colour = category of pass --------------------------

eng_croatia_prog_pass_map <- passing_analysis_game1 %>%
  filter(progressive.pass == 1) %>% 
  filter(player.name == "Kieran Trippier") %>% 
  ggplot() +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 dimensions = pitch_statsbomb) +
  geom_segment(size = .9, aes(x = location.x, y = location.y.inverse, 
                               xend = pass.end_location.x, yend = pass.end_location.y.inverse, linetype = "solid"),
               col = "Red", alpha = 1, show.legend = F, arrow = NULL, 
               lineend = "round") +
  geom_point(aes(x = pass.end_location.x, y = pass.end_location.y.inverse), size = 3.5, col = 'Red') +
  geom_segment(data = passing_analysis_game1 %>% filter(progressive.pass == 1) %>% filter(player.name != "Kieran Trippier"), 
               size = 1, aes(x = location.x, y = location.y.inverse, xend = pass.end_location.x, yend = pass.end_location.y.inverse ),
               col = "#3F95F7", alpha = 1, show.legend = F, arrow = NULL) +
  geom_point(data = passing_analysis_game1 %>% filter(progressive.pass == 1) %>% filter(player.name != "Kieran Trippier"),
             aes(x = pass.end_location.x, y = pass.end_location.y.inverse), size = 3.5, col = '#3F95F7') +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  labs(title = "England's progressive passes (51) primarily came from left flank, either advancing the ball \nup the pitch, or coming inside.",
       subtitle = "Trippier (red) was England's most progressive passer (15) advancing the ball on average 20m up the pitch") 
  
## England defensive actions ---------------------------------------------------

england_croatia_def_actions <- england_croatia_match %>% 
  filter(team.name == "England") %>%
  filter(type.name %in% c("Ball Recovery","Interception", "Pressure", "Duel")) %>% 
  filter(duel.type.name %in% c("Aerial Lost",NA)) %>% 
  select(player.name, type.name) %>% mutate(val = 1, row = row_number()) %>%
  pivot_wider(names_from = type.name, values_from = "val", values_fill = 0) %>% 
  select(-row) %>% 
  group_by(player.name) %>% 
  summarise(pressure = sum(Pressure, na.rm = TRUE),
            ball.recovery = sum(`Ball Recovery`, na.rm = TRUE),
            interception = sum(Interception, na.rm = TRUE),
            aerial.battle.lost = sum(Duel, na.rm = TRUE)) %>%
  arrange(desc(pressure))

england_duels <- england_croatia_match %>% 
  filter(team.name == "England") %>%
  filter(type.name == "Duel") %>% 
  filter(duel.type.name == "Tackle") %>% 
  select(player.name, duel.outcome.name) %>% mutate(val = 1, row = row_number()) %>%
  pivot_wider(names_from = duel.outcome.name, values_from = "val", values_fill = 0) %>% 
  select(-row) %>% 
  group_by(player.name) %>% 
  summarise(tackles.attempted = sum(Won, na.rm = TRUE) + 
              sum(`Success In Play`, na.rm = TRUE) + 
              sum(`Lost Out`, na.rm = TRUE) + 
              sum(`Lost In Play`, na.rm = TRUE),
            tackles.won = sum(Won, na.rm = TRUE),
            success.in.play = sum(`Success In Play`, na.rm = TRUE),
            lost.out = sum(`Lost Out`, na.rm = TRUE),
            lost.in.play = sum(`Lost In Play`, na.rm = TRUE)) %>%
  arrange(desc(tackles.attempted))

england_duels <- england_duels %>% mutate(succesful.tackles = tackles.won + success.in.play,
                         unsuccesful.tackles = tackles.attempted - succesful.tackles) %>% 
                         select(player.name, succesful.tackles, unsuccesful.tackles)

england_croatia_def_actions <- left_join(england_croatia_def_actions , england_duels, by = 'player.name')

england_croatia_def_actions[is.na(england_croatia_def_actions)] <- 0

england_croatia_def_actions <- england_croatia_def_actions %>%
                               mutate(defensive.actions = pressure + ball.recovery + interception +
                               aerial.battle.lost + succesful.tackles + unsuccesful.tackles) %>% 
                               arrange(desc(defensive.actions)) %>% select(-defensive.actions)

## England pressure map --------------------------------------------------------

england_croatia_match %>% 
  filter(team.name == "England") %>%
  filter(type.name %in% c("Pressure","Ball Recovery","Interception")) %>% 
  ggplot() +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 dimensions = pitch_statsbomb) +
  geom_jitter(aes(x = location.x, y = location.y.inverse, colour = type.name),size = 2) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4"))
  