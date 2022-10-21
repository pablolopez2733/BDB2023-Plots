# * Load libraries ----
library(gganimate)
library(tidyverse)

# * Load helper functions ---- 
source("utils/plot_utils.R")
source("utils/data_utils.R")

# * read week data ----
df <- read.csv("data/week1.csv")

# exctract play df
play <- fetch_play(df, playId_ = 137, gameId = 2021090900)

# plot 1 frame
plot_frame(play,1)

# ensure timing of play matches 10 frames-per-second (h/t NFL Football Ops)
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
play_anim
