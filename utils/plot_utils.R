library(tidyverse)
library(patchwork)


plot_field <- function(field_color="#00b140", line_color = "#ffffff") {
  field_height <- 160/3
  field_width <- 120
  
  field <- ggplot() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(hjust = 1),
      legend.position = "bottom",
      # legend.title = element_text(color = "#212529", size = 12, vjust = 1),
      legend.title.align = 1,
      # legend.text = element_text(color = "#343a40", size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      # panel.background = element_blank(),
      panel.background = element_rect(fill = field_color, color = "white"),
      panel.border = element_blank(),
      aspect.ratio = field_height/field_width
    ) +
    # major lines
    annotate(
      "segment",
      x = c(0, 0, 0,field_width, seq(10, 110, by=5)),
      xend = c(field_width,field_width, 0, field_width, seq(10, 110, by=5)),
      y = c(0, field_height, 0, 0, rep(0, 21)),
      yend = c(0, field_height, field_height, field_height, rep(field_height, 21)),
      colour = line_color
    ) +
    # hashmarks
    annotate(
      "segment",
      x = rep(seq(10, 110, by=1), 4),
      xend = rep(seq(10, 110, by=1), 4),
      y = c(rep(0, 101), rep(field_height-1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101)),
      yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101)),
      colour = line_color
    ) +
    # yard numbers
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      size = 7,
      family = "mono",
      colour = line_color, # "#495057",
    ) +
    # yard numbers upside down
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(field_height-12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      angle = 180,
      size = 7,
      family = "mono",
      colour = line_color, 
    )
  
  return(field)
}

plot_frame <- function(onePlay_, frame_)
{
  #  Take one frame from one play, plot a scatter plot image.
  
  # Check for data
  if(is.null(onePlay_)) {
    print("error: need to provide play data")
    return()
  }
  if(is.null(frame_)) {
    print("error: need to provide frame of play to visualize")
    return()
  }
  # * get play metadata ----
  play_desc <- onePlay_$playDescription %>% .[1]
  play_dir <- onePlay_$playDirection %>% .[1]
  yards_togo <- onePlay_$yardsToGo %>% .[1]
  los <- onePlay_$absoluteYardlineNumber %>% .[1]
  togo_line <- if(play_dir=="left") los-yards_togo else los+yards_togo
  
  fr <- onePlay_ %>% 
    filter(frameId == frame_)
  
  colores        <- unique(fr$color1)
  names(colores) <- colores
  
  
  # one frame scatterplot
  one_frame <- plot_field() +
    # line of scrimmage
    annotate(
      "segment",
      x = los, xend = los, y = 0, yend = 160/3,
      colour = "#0d41e1"
    ) +
    # 1st down marker
    annotate(
      "segment",
      x = togo_line, xend = togo_line, y = 0, yend = 160/3,
      colour = "#f9c80e"
    )+
    geom_point(
      data = fr,
      mapping = aes(x = x, y = y, color = color1 )
      ) +
    scale_colour_manual(values = colores) +
    theme(title = )
  
  return(one_frame)
  
}

play_animation <- function(onePlay_)
{
  #  Take one frame from one play, plot a scatter plot image.
  
  # Check for data
  if(is.null(onePlay_)) {
    print("error: need to provide play data")
    return()
  }
  # * get play metadata ----
  play_desc <- onePlay_$playDescription %>% .[1]
  play_dir <- onePlay_$playDirection %>% .[1]
  yards_togo <- onePlay_$yardsToGo %>% .[1]
  los <- onePlay_$absoluteYardlineNumber %>% .[1]
  togo_line <- if(play_dir=="left") los-yards_togo else los+yards_togo
  
  colores        <- unique(onePlay_$color1)
  names(colores) <- colores
  
  anim <- plot_field() +
    # line of scrimmage
    annotate(
      "segment",
      x = los, xend = los, y = 0, yend = 160/3,
      colour = "#0d41e1"
    ) +
    # 1st down marker
    annotate(
      "segment",
      x = togo_line, xend = togo_line, y = 0, yend = 160/3,
      colour = "#f9c80e"
    )+
    geom_point(
      data = onePlay_,
      mapping = aes(x = x, y = y, color = color1 )
    ) +
    scale_colour_manual(values = colores) +
    labs(
      title = play_desc,
      caption = "Data: Big Data Bowl 2023"
    ) +
    theme(legend.position="none")
    # animation stuff
    transition_time(frameId) +
    ease_aes('linear') +
    NULL
  
  return(anim)
}
