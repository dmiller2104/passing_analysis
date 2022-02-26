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

## Getting England data ------------------------------------

euros_data <- FreeCompetitions() %>% filter(competition_name == "UEFA Euro")

euros_matches <- FreeMatches(euros_data)

euros_matches <- euros_matches %>% 
  mutate(england_match = ifelse(home_team.country.name == "England" | away_team.country.name == "England", 1, 0))

england_euros_matches <- euros_matches %>% filter(england_match == 1)

## Developing network dataset creation function -------------------
#@ Adapted from the soccermatics passmap function
#@ this largely lifts the script they've produced with a few minor tweaks
#@ aim is for the function to be used for a varierty of differents visauls, beyond passing.

adapted_soccerPassMap <- function(df, minPass = 3, player.x.location = "location.x", player.y.location = "location.y",
                                  label = TRUE, maxNodeSize = 30, maxEdgeSize =30
                                  )
  {type.name<-pass.outcome.name<-period<-timestamp<-player.name<-pass.recipient.name<-from<-to<-xend<-yend<-events<-NULL
  
  if(length(unique(df$team.name)) > 1) stop("Data contains more than one team")
   
  df <- as.data.frame(df)
  
  # set variable names
  x <- player.x.location
  y <- player.y.location
  id <- "player.id"
  player_name <- "player.name"
  team_name <- "team.name"
  
  df$x_loc <- df[,x]
  df$y_loc <- df[,y]
  df$id <- df[,id]
  df$name <- df[,player_name]
  df$team <- df[,team_name]
  
  # full game passing stats for labels
  passes <- df %>% 
    filter(type.name == "Pass") %>% 
    group_by(pass.outcome.name) %>% 
    tally() %>% 
    filter(!pass.outcome.name %in% c("Injury Clearance", "Unknown")) %>% 
    mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete"))
  pass_n <- sum(passes$n)
  pass_pc <- passes[passes$pass.outcome.name == "Complete",]$n / pass_n * 100
  
  
  # filter events before time of first substitution, if at least one substitution
  min_events <- df %>% 
    group_by(id) %>% 
    dplyr::summarise(period = min(period), timestamp = min(timestamp)) %>% 
    stats::na.omit() %>% 
    arrange(period, timestamp)
  
  if(nrow(min_events) > 11) {
    max_event <- min_events[12,]
    idx <- which(df$period == max_event$period & df$timestamp == max_event$timestamp) - 1
    df <- df[1:idx,]
  }
  
  
  # get nodes and edges for plotting
  # node position and size based on touches
  nodes <- df %>% 
    filter(type.name %in% c("Pass", "Ball Receipt*", "Ball Recovery", "Shot", "Dispossessed", "Interception", "Clearance", "Dribble", "Shot", "Goal Keeper", "Miscontrol", "Error")) %>% 
    group_by(id, name) %>% 
    dplyr::summarise(x = mean(x_loc, na.rm=T), y = mean(y_loc, na.rm=T), events = n()) %>% 
    stats::na.omit() %>% 
    as.data.frame()
  
  # edges based only on completed passes
  edgelist <- df %>% 
    mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete")) %>%
    filter(type.name == "Pass" & pass.outcome.name == "Complete") %>% 
    select(from = player.name, to = pass.recipient.name) %>% 
    group_by(from, to) %>% 
    dplyr::summarise(n = n()) %>% 
    stats::na.omit()
  
  edges <- left_join(edgelist, 
                     nodes %>% select(id, name, x, y),
                     by = c("from" = "name"))
  
  edges <- left_join(edges, 
                     nodes %>% select(id, name, xend = x, yend = y),
                     by = c("to" = "name"))
  
  edges <- edges %>% 
    group_by(player1 = pmin(from, to), player2 = pmax(from, to)) %>% 
    dplyr::summarise(n = sum(n), x = x[1], y = y[1], xend = xend[1], yend = yend[1])
  
  
  # filter minimum number of passes and rescale line width
  nodes <- nodes %>% 
    mutate(events = scales::rescale(events, c(2, maxNodeSize), c(1, 200)))
  
  # rescale node size
  edges <- edges %>% 
    filter(n >= minPass) %>%
    mutate(n = scales::rescale(n, c(1, maxEdgeSize), c(minPass, 75)))
  
  # return the three dfs
  return(list(edges, nodes, df))
}


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

## Progressive passes by player and category -----------------------------------

passing_analysis_game1 %>% filter(progressive.pass.category != "NA") %>%
       mutate(pass_n = 1) %>% 
  pivot_wider(names_from = progressive.pass.category, values_from = pass_n) %>% 
  group_by(player.name) %>% 
  summarise(category.1 = sum(category.1, na.rm = TRUE),
            category.2 = sum(category.2, na.rm = TRUE),
            category.3 = sum(category.3, na.rm = TRUE),
            Total.progressive.passes = sum(category.1, na.rm = TRUE) + 
              sum(category.2, na.rm = TRUE) + 
              sum(category.3, na.rm = TRUE)) %>% 
  arrange(desc(Total.progressive.passes), desc(category.3), desc(category.2), desc(category.1))

## progressive passing map, colour = category of pass --------------------------

eng_croatia_prog_pass_map <- create_Pitch(middlethird = TRUE) + 
  geom_segment(data = passing_analysis_game1 %>% filter(progressive.pass.category == "category.1"), 
               size = 1, aes(x = location.x, y = location.y.inverse, xend = pass.end_location.x, yend = pass.end_location.y.inverse ),
               col = "#3F95F7", alpha = 1, show.legend = F, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(data = passing_analysis_game1 %>% filter(progressive.pass.category == "category.2"), 
               size = 1, aes(x = location.x, y = location.y.inverse, xend = pass.end_location.x, yend = pass.end_location.y.inverse ),
               col = "Dark Green", alpha = 1, show.legend = F, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(data = passing_analysis_game1 %>% filter(progressive.pass.category == "category.3"), 
               size = 1, aes(x = location.x, y = location.y.inverse, xend = pass.end_location.x, yend = pass.end_location.y.inverse ),
               col = "Red", alpha = 1, show.legend = F, arrow = arrow(length = unit(0.5, "cm"))) +
  labs(subtitle = "Trippier, at left back, played the most progressive passes in all areas of the pitch (15), with seven in the opposition half",
       title = "The selection of two right footed players on the left side reflects England's passing decisions to go more\n centrally with its progressive passes.") + 
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(padding = margin(7.5, 77.5, -20, 50.5)),
        plot.subtitle = element_text(hjust = 0.60, vjust = -10)) +
  annotate("text", 104, 1,
           label = paste0("Category one: ",
           passing_analysis_game1 %>% filter(progressive.pass.category == 'category.1') %>% nrow()), 
           hjust = 5.23, vjust = -4.5, size = 5 * 7/8, col = "#3F95F7") +
  annotate("text", 104, 1,
           label = paste0("Category two: ",
           passing_analysis_game1 %>% filter(progressive.pass.category == 'category.2') %>% nrow()), 
           hjust = 4.97, vjust = -3, size = 5 * 7/8, col = "Dark Green") +
  annotate("text", 104, 1,
           label = paste0("Category three: ",
           passing_analysis_game1 %>% filter(progressive.pass.category == 'category.3') %>% nrow()), 
           hjust = 4.5, vjust = -1.5, size = 5 * 7/8, col = "Red")
  



