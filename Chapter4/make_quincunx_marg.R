# The simulation uses the chipmunkbasic package
# for which one first has to install chipmunk2d
# see here for installation instructions
# https://github.com/coolbutuseless/chipmunkbasic

# An installation of ffmpeg is needed to make an mp4
# from png files.

# The code is a modified version of the vignette here:
# 

library(chipmunkbasic) 
library(ggplot2)
library(data.table)
library(magrittr)

for (my_seed in 1:10) {
  set.seed(my_seed)
  
  # adapt board to new number of balls (circles)
  f.circles = 1.5
  f.advance = 1
  
  n_circles = 500 * f.circles
  advance = 5
  Nt = round((1000 * f.circles)/f.advance) # number of frames
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initialize a simulation space
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cm <- Chipmunk$new(time_step = 0.005)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add slots 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  floor <- -18 # cheating. original was 0, need higher slots because of more balls
  width <- 60
  for (x in seq(-width, width, 4)) {
    cm$add_static_segment(x, floor,  x,  20)
  }
  
  cm$add_static_segment(-width, floor, width, floor)
  cm$add_static_segment(-width, floor-0.2, width, floor-0.2)
  
  cm$add_static_segment(-width, 120, width, 120)
  cm$add_static_segment(-width, 120-0.2, width, 120-0.2)
  
  # add side walls or floor to avoid that balls 
  # fly out of the original frame
  # side walls
  # cm$add_static_segment(-width, floor, -width,  100)
  # cm$add_static_segment(width, floor,  width,  100)
  #floor
  # cm$add_static_segment(-width*3, floor-250, width*3, floor-250)
  # cm$add_static_segment(-width*3, floor-0.2-250, width*3, floor-0.2-250)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fetch all the segments. Use for plotting
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  segments_df <- data.table(cm$get_static_segments())
  
  ggplot() +
    geom_segment(data = segments_df[y1 < 100 & x2 < 100], aes(x = x1, y = y1, xend = x2, yend = y2)) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = 'none') 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add some circles 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  library(MASS)
  S = matrix(c(1,.5,.5,1), ncol = 2)
  mu = c(0,0)
  circle.locations = mvrnorm(500, mu = mu, Sigma = S)
  circle.locations[,1] = circle.locations[,1]*15
  circle.locations[,1] = round(circle.locations[,1]/2)*2+1 # + (runif(500)-.5)/100
  circle.locations[,2] = 80 + circle.locations[,2]*10
  for (i in 1:nrow(circle.locations)) {
    cm$add_circle(
      x        = circle.locations[i,1],
      y        = circle.locations[i,2],
      radius   = 0.7,
      friction = 0.01
    )
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # (1) advance the simulation (2) plot the circles (3) Repeat.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split advance and saving of pngs to color balls.
  
  root_dir <- "margin"
  png_dir  <- file.path(root_dir, "png")
  unlink(list.files(png_dir, pattern = "*.png", full.names = TRUE))
  
  # The original Nt is needed if no ball leaves the frame. 
  # With set.seed(4) fewer frames are needed for the movie
  Nt.plot = 250
  
  
  x0 <- seq(-60,60,len=100)
  for (i in 1:Nt.plot) {
    cm$advance(advance)
    circles = cm$get_circles()
    if(i == 1) circles1 = circles
    if (i  %% 25 == 0) message(paste("plot",i))
    p <- ggplot(circles) +
      geom_point(data = circles1, aes(x, y), size = 1.6, alpha = .2) +
      geom_point(aes(x, y), size = 1.6) +
      geom_segment(data = segments_df[y1 < 100 & x2 < 100], aes(x = x1, y = y1, xend = x2, yend = y2), alpha = .25) +
      geom_segment(data = segments_df[y1 > 100 & x2 > 100], aes(x = x1, y = y1, xend = x2, yend = y2), alpha = 0) +
      coord_fixed() +
      scale_color_manual(values = c("grey","red","blue")) +
      theme_void() +
      theme(legend.position = 'none', plot.background = element_rect(fill = "white", colour = NA))
    
    
    outfile <- sprintf("margin/%04i.png", i)
    ggsave(outfile, p, width = 7, height = 7)
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ffmpeg/gifsicle to create animations
  #  - create mp4 from PNG files (use in vignettes)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  root_name <- paste0("margins",my_seed)
  mp4_name  <- paste0(root_name,".mp4")
  png_dir = "margin"
  system(glue::glue("ffmpeg -y -framerate 30 -pattern_type glob -i '{png_dir}/*.png' -c:v libx264 -pix_fmt yuv420p -s 800x800 '{mp4_name}'"))
  #system(glue::glue("ffmpeg -y -i '{mp4_name}' -filter_complex 'fps=30,scale=800:-1:flags=lanczos,split [o1] [o2];[o1] palettegen [p]; [o2] fifo [o3];[o3] [p] paletteuse' '{tmp_name}'"))
  
  
}
