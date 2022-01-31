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

eng_player_df <- england_croatia_match %>% filter(team.name == "England")

match1_eng_dfs <- adapted_soccerPassMap(eng_player_df, minPass = 3, player.x.location = "location.x", 
                                  player.y.location = "location.y.inverse")

eng_edges_match1 <- match1_eng_dfs[[1]] 
eng_nodes_match1 <- match1_eng_dfs[[2]]

match1_eng_dfs[[3]]


#create_Pitch(middlethird = TRUE) + 
ggplot() + 
  xlim(0, 120) +
  ylim(0, 80) + 
  geom_segment(data = eng_edges_match1, aes(x, y, xend = xend, yend = yend, size = n ), col = "light blue", alpha = 0.9) +
  geom_point(data = eng_nodes_match1, aes(x, y, size = events, color = "red", fill = events), pch = 21) + 
  scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_size_identity() + 
  guides(size = "none") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 
