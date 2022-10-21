# * Load libraries ----
library(gganimate)
library(tidyverse)

# * Load helper functions ---- 
source("utils/plot_utils.R")
source("utils/data_utils.R")

# * read week data ----
df <- read.csv("data/week1.csv")

# ---- establish game and play to plot ----
playId_ = 137
gameId_ = 2021090900
#-------------------------------------------

# exctract play df ----
play <- fetch_play(df, playId_, gameId_)

# plot a specific frame ----
plot_frame(play,1)

# * animate a play ----
play_length <- length(unique(play$frameId))
# customize duration of gif
duration_ = 7
# Animate our play! :D
animate(
  play_animation(play),
  duration = 7,
  fps = 10, 
  nframe = play_length,
  width = 850,
  height = 500,
  end_pause = 10
)
anim_save('plots/play.gif')

