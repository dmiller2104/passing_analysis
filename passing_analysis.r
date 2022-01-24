## Getting R pacakges ------------------------------------

library(ggplot2)
library(dplyr)
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
font_import()
loadfonts(device = "win")
# fonts() to check fonts available

## Getting England data ------------------------------------

euros_data <- FreeCompetitions() %>% filter(competition_name == "UEFA Euro")

euros_matches <- FreeMatches(euros_data)

euros_matches <- euros_matches %>% 
  mutate(england_match = ifelse(home_team.country.name == "England" | away_team.country.name == "England", 1, 0))

england_euros_matches <- euros_matches %>% filter(england_match == 1)

england_euros_matches <- StatsBombFreeEvents(MatchesDF = england_euros_matches, Parallel = T)
