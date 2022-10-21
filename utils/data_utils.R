library(tidyverse)

# * This function reads the data catalogues

games <- read.csv("data/games.csv")
players <- read.csv("data/players.csv")
plays <- read.csv("data/plays.csv")

fetch_team_colors <- function() {
  team_colors_ <- suppressMessages(readr::read_tsv("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/data/nfl_team_colors.tsv"))
  # add football color
  team_colors_ = rbind(team_colors_, c("football","#935e38","black","#935e38"))
  return(team_colors_)
}
team_colors_ <- fetch_team_colors()

fetch_play <- function(df,playId_,gameId_)
{
  # ' Function that returns a play dataframe with its correspondent information
  play <- df %>% 
    filter(gameId == gameId_ & playId == playId_) %>% 
    left_join(plays, by = c("playId" = "playId", "gameId" = "gameId")) %>% 
    left_join(team_colors_, by = c("team"="teams"))
  return(play)
}

