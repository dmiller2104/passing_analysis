## packages --------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(devtools)
library(remotes)
library(SDMTools)
library(tidyverse)

## Developing network dataset creation function --------------------------------
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

## Progressive pass function ---------------------------------------------------
#@ function tests to see if ball was advanced 25% towards goal, and at least 5m vertically
#@ should work on dfs

progressive_pass_assess <- function(dataset, location.x = "location.x", location.y = "location.y.inverse", 
                                    pass.end_location.x = "pass.end_location.x" ,
                                    pass.end_location.y = "pass.end_location.y.inverse"){
  
  goal.center <- 40
  # vertical ball advancement
  distance.to.goal <- 120 - dataset[,location.x]
  aim.pass.advancement.25percent <- dataset[,location.x] + (distance.to.goal * 0.25)
  distance.to.goal.after.pass <- 120 - dataset[,pass.end_location.x]
  actual.pass.advancement <- 1 - (dataset[,pass.end_location.x] / distance.to.goal)
  pass.progress.x <- (120 - dataset[,location.x]) - (120 - dataset[,pass.end_location.x])
  more.than.5m <- ifelse(pass.progress.x > 5, 1, 0)
  progressive.x <- ifelse(dataset[,pass.end_location.x] > aim.pass.advancement.25percent, 1, 0)
  
  # horizontal ball advancement - is the ball more central
  distance.to.center <- abs(goal.center - dataset[,location.y])
  horizontal.aim.pass.advancement.upper <- 40 + (distance.to.center * 0.75)
  horizontal.aim.pass.advancement.lower <- 40 - (distance.to.center * 0.75)
  distance.to.center.after.pass <- abs(40 - dataset[,pass.end_location.y])
  actual.horizontal.advancement <- 1 - (dataset[,pass.end_location.y] / distance.to.center)
  pass.progress.y <- abs(40 - dataset[,location.y]) - abs(40 - dataset[,pass.end_location.y])
  progressive.y <- ifelse(dataset[,pass.end_location.y] < horizontal.aim.pass.advancement, 1, 0)
  
  all_progress <- ifelse(actual.pass.advancement + actual.horizontal.advancement > 25, 1, 0)
  
  # final outcome
  progressive.pass <- ifelse(more.than.5m == 1 & progressive.x == 1, 1, 0)
  dataset$progressive.pass <- progressive.pass
  
  return(dataset)
  
}