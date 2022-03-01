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

engl_croatia_pass_network <- create_Pitch(middlethird = TRUE) + 
#soccerPitch(lengthPitch = 120, widthPitch = 80, theme = 'grass') + 
  geom_segment(data = eng_edges_match1, aes(x, y, xend = xend, yend = yend, size = n/2 ),
               col = "#3F95F7", alpha = 0.9, show.legend = F) +
  geom_point(data = eng_nodes_match1, aes(x, y, size = events, color = "red", fill = events), pch = 21, show.legend = F) + 
  scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_size_identity() + 
  guides(size = "none") +
  theme_classic() + 
  ggtitle("England vs Croatia, 13-06-2021 Euro 2020") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(hjust = 0.15, vjust = -10)) +
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

eng_passes_vs_croatia <- create_Pitch(middlethird = TRUE) +
  geom_point(data = passing_analysis_game1, aes(x = location.x, y = location.y.inverse), 
             color = "red", alpha = 0.5, size = 6) +
  labs(subtitle = "A clear focus on the left side of the pitch, but also noticeably fewer passes around the central part of the 18 yard area.",
       title = "All England passes vs. Croatia, by location of where pass made") + 
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(padding = margin(7.5, 77.5, -20, 50.5)),
        plot.subtitle = element_text(hjust = 0.65, vjust = -10))


## Progressive passes by player ------------------------------------------------

passing_analysis_game1 %>% filter(progressive.pass != 0) %>%
  mutate(pass_n = 1) %>% 
  pivot_wider(names_from = progressive.pass.category, values_from = pass_n) %>% 
  group_by(player.name) %>% 
  summarise(progressive.passes = sum(progressive.pass, na.rm = TRUE)) %>% 
  arrange(desc(progressive.passes))

## progressive passing map, colour = category of pass --------------------------

eng_croatia_prog_pass_map <- create_Pitch(middlethird = TRUE) + 
  geom_segment(data = passing_analysis_game1 %>% filter(progressive.pass == "1") %>% filter(player.name != "Kieran Trippier"), 
               size = 1, aes(x = location.x, y = location.y.inverse, xend = pass.end_location.x, yend = pass.end_location.y.inverse ),
               col = "#3F95F7", alpha = 1, show.legend = F, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(data = passing_analysis_game1 %>% filter(progressive.pass == "1") %>% filter(player.name == "Kieran Trippier"), 
               size = 1, aes(x = location.x, y = location.y.inverse, xend = pass.end_location.x, yend = pass.end_location.y.inverse ),
               col = "Red", alpha = 1, show.legend = F, arrow = arrow(length = unit(0.5, "cm"))) + 
  labs(title = "England's progressive passes (52) primarily came from left flank, either advancing the ball up the pitch, or coming inside.",
       subtitle = "Trippier (red) was England's most progressive passer (12)") +
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(padding = margin(7.5, 77.5, -30, 50.5)),
        plot.subtitle = element_text(hjust = 0.14, vjust = -15))
# make improvements to the above  


