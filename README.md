Generating Animated GIFs and plots for NFL Tracking data using ggplot
================

This is meant to be a very simple implementation of a collection of
functions to plot frames and animate the tracking data housed in the
[Big Data Bowl Kaggle
Competition](https://www.kaggle.com/competitions/nfl-big-data-bowl-2023/data).

## Credits

All of the codes were based on `asonty`’s `ngs_highlights` repo. Check
out his work [here](https://github.com/asonty/ngs_highlights)!

## Important Note

If you want to use theses functions locally, you should first download
the competition data and place it in a folder called `data` within your
**home directory**.

## Generating the plot

To generate a play animation, head to the `src.R` script, change the
`playId_` and `gameId_` parameters and run the script.

------------------------------------------------------------------------

# Code Breakdown

### Setup

``` r
# * Load libraries ----
library(gganimate)
library(tidyverse)
```

Based on user **asonty**’s work, I coded helper functions to extract and
plot data.

``` r
# * Load helper functions ---- 
source("utils/plot_utils.R")
source("utils/data_utils.R")
```

### Read data

Because of the size of the files, I did not include the data directory
in this repository. Nevertheless, you can download the data from
[Kaggle](https://www.kaggle.com/competitions/nfl-big-data-bowl-2023/data)
and place it in a folder called `data` within the home directory.

``` r
# * read week 1 data ----
df <- read.csv("data/week1.csv")
```

### Parameters

Set the play which you want to plot and extract the info:

``` r
playId_ = 137
gameId_ = 2021090900

play <- fetch_play(df, playId_, gameId_)
```

#### Plot a single frame

You can plot a specific frame of a play by using the `plot_frame`
function.

``` r
plot_frame(play,1)
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### Generate an animated gif of a play

``` r
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
```

![](README_files/figure-gfm/unnamed-chunk-6-1.gif)<!-- -->
