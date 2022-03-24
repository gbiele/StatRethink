library(chipmunkbasic)
library(ggplot2)
library(data.table)
library(magrittr)

all.circles = vector(mode = "list", length = 9)

for (sx in 1:25) {
  set.seed(sx)
  
  f.circles = 1.5
  f.advance = 1
  
  n_circles = 500 * f.circles
  advance = 5
  Nt = round((1000 * f.circles)/f.advance) # 1000
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initialize a simulation space
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cm <- Chipmunk$new(time_step = 0.005)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add funnel segments
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gap <- 2
  cm$add_static_segment( -65, 130, -gap, 104)
  cm$add_static_segment(  65, 130,  gap, 104)
  cm$add_static_segment(-gap,  103, -gap, 104)
  cm$add_static_segment( gap,  103,  gap, 104)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add pins
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in 1:20) {
    y <- 103 - i * 3
    if (i %% 2 == 1) {
      xs <- seq(0, 40, 2)
    } else {
      xs <- seq(1, 40, 2)
    }
    xs <- 1.0 * sort(unique(c(xs, -xs)))
    
    w <- 0.05
    xstart <- xs - w
    xend   <- xs + w
    
    for (xi in seq_along(xs)) {
      cm$add_static_segment(xstart[xi], y,  xend[xi],  y)
    }
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add slots 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  floor <- -18
  width <- 60
  for (x in seq(-width, width, 3)) {
    y = ifelse(abs(x) == width,40,40)
    cm$add_static_segment(x, floor,  x,  y)
  }
  
  cm$add_static_segment(-width, floor, width, floor)
  cm$add_static_segment(-width, floor-0.2, width, floor-0.2)
  
  #floor
  cm$add_static_segment(-width*3, floor-200, width*3, floor-200)
  cm$add_static_segment(-width*3, floor-0.2-200, width*3, floor-0.2-200)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fetch all the segments. Use for plotting
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  segments_df <- cm$get_static_segments()
  
  ggplot() +
    geom_segment(data = segments_df, aes(x = x1, y = y1, xend = x2, yend = y2)) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = 'none') 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add some circles 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in 1:n_circles) {
    cm$add_circle(
      x        = runif(1,  -30,  30),
      y        = runif(1,  116, 130),
      radius   = 0.7,
      friction = 0.01
    )
  }
  
  root_dir <- "quincunx"
  png_dir  <- file.path(root_dir, "png")
  unlink(list.files(png_dir, pattern = "*.png", full.names = TRUE))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # (1) advance the simulation (2) plot the circles (3) Repeat.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  circles = cm$get_circles()
  tmp = matrix(NA,ncol = ncol(circles), nrow = n_circles*Nt-n_circles)
  colnames(tmp) = names(circles)
  circles = rbind(circles,tmp)
  rm(tmp)
  gc()
  
  for (i in 2:Nt) {
    if (i  %% 25 == 0) message(i)
    cm$advance(advance)
    circles[(1:n_circles)+(i-1)*n_circles,] = cm$get_circles()
  }
  circles = data.table(circles)
  circles[, t := sort(rep(1:Nt,n_circles))]
  
  circles %>% 
    .[, col := "1"] %>% 
    .[idx %in% circles[t == Nt & x > 20,idx], col := "2"] %>% 
    .[idx %in% circles[t == Nt & x < (-20),idx], col := "3"]
  
  x0 <- seq(-60,60,len=100)
  for (i in 1:Nt) {
    
    if (i  %% 25 == 0) message(paste("plot",i))
    
    p <- ggplot(circles[t == i]) +
      geom_point(aes(x, y, col = col), size = 1.6) +
      geom_segment(data = segments_df, aes(x = x1, y = y1, xend = x2, yend = y2)) +
      coord_fixed() +
      scale_color_manual(values = c("grey","red","blue")) +
      theme_void() +
      theme(legend.position = 'none', plot.background = element_rect(fill = "white", colour = NA)) +
      geom_line(data=data.frame(x0),inherit.aes = F,aes(x=x0,y=floor+Nt*exp(-x0^2/(2*10^2))/sqrt(2*pi*10^2)),col="darkred",alpha=0.8,lwd=1) +
      NULL
    
    
    outfile <- sprintf("quincunx/%04i.png", i)
    ggsave(outfile, p, width = 7, height = 7)
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ffmpeg/gifsicle to create animations
  #  - create mp4 from PNG files (use in vignettes)
  #  - create gif from mp4 (use in github readme)
  #  - simplify gif with gifsicle
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  root_name <- "galton"
  mp4_name  <- paste0(root_name, sx, ".mp4")
  tmp_name  <- tempfile(fileext = ".gif")
  gif_name  <- paste0(root_name, sx, ".gif")
  png_dir = "quincunx"
  system(glue::glue("ffmpeg -y -framerate 30 -pattern_type glob -i '{png_dir}/*.png' -c:v libx264 -pix_fmt yuv420p -s 800x800 '{mp4_name}'"))
  system(glue::glue("ffmpeg -y -i '{mp4_name}' -filter_complex 'fps=30,scale=800:-1:flags=lanczos,split [o1] [o2];[o1] palettegen [p]; [o2] fifo [o3];[o3] [p] paletteuse' '{tmp_name}'"))
  
}


system(glue::glue("gifsicle -O99 -o '{gif_name}' -k 16 '{tmp_name}'"))

