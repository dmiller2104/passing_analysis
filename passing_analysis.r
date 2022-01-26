## Getting R packages ------------------------------------

library(ggplot2)
library(dplyr)
library(devtools)
library(remotes)
library(SDMTools)
library(tidyverse)
library(StatsBombR)
library(FC.rSTATS)
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

## England Vs. Croatia ------------------------------------

england_croatia_match <- england_euros_matches %>% filter(match_week == 1)

england_croatia_match <- StatsBombFreeEvents(england_croatia_match)

england_croatia_match <- allclean(england_croatia_match)

eng_player_df <- england_croatia_match %>% filter(team.name == "England") %>% 
  filter(type.name == "Pass") %>% group_by(player.name) %>% 
  separate(location, c("x_location","y_location"), sep = ",")

eng_player_df$x_location <- as.numeric(gsub("[a-zA-Z()]", "", eng_player_df$x_location))
 
eng_player_df$y_location <- as.numeric(gsub("[a-zA-Z()]", "", eng_player_df$y_location))

eng_player_df <- eng_player_df %>% group_by(player.name) %>% 
            summarise(x_location = mean(x_location,na.rm = TRUE),
                      y_location = mean(y_location,na.rm = TRUE))

pass_connections <- england_croatia_match %>% filter(team.name == "England") %>% 
  filter(type.name == "Pass") %>% select(player.name,pass.recipient.name) %>% 
  filter(!is.na(pass.recipient.name)) %>% 
  mutate(pass_made = 1)

pass_connections <- pass_connections %>% group_by(player.name, pass.recipient.name) %>% 
  summarise(passes.made = sum(pass_made, na.rm = TRUE))

pass_connections <- pass_connections %>% 
              group_by(player.name) %>% 
              pivot_wider(names_from = pass.recipient.name, 
              values_from = passes.made, values_fill = 0)

pass_connections <- pass_connections %>% janitor::clean_names()

pass_connections <- pass_connections %>% mutate(
                    Total = rowSums(across(where(is.numeric))))
