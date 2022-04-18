library(smoother)

library(dagitty)
library(rethinking)

dag1 = dagitty(
  "dag{
  A->C;
  B->A;
  B->C
  }")
coord.list = 
  list(
    x=c(A=0,B=100,C=200),
    y=c(A=0,B=-100,C=0))
coordinates(dag1) = coord.list
drawdag(dag1, cex = 2, radius = 5, shapes = list(B = "c"))

x = seq(200,0, length.out = 500)
y = c(seq(0,100,length.out = 250),
      seq(100,0,length.out = 250))
ys = smth.gaussian(y, tails = F)
ys[which(is.na(ys))] = y[which(is.na(ys))]
ys = ys + 7

file.remove(list.files("anim",full.names = T))
for (k in seq(2,500,5)) {
  png(paste0("anim/anim",sprintf("%03d", k), ".png"))
  drawdag(dag1, cex = 2)
  lines(x[1:k],ys[1:k],col = "red", lwd = 2)
  arrows(
    x0 = x[k-1],
    y0 = ys[k-1],
    x1 = x[k],
    y1 = ys[k],
    col = "red",
    lwd = 2
  )
  dev.off()
}



make_gif = function(dir = NULL, fn = NULL, fps = 20) {
  library(magick)
  ## list file names and read in
  imgs <- list.files("anim", full.names = TRUE)
  img_list <- lapply(imgs, image_read)
  
  ## join the images together
  img_joined <- image_join(img_list)
  
  ## animate at 20 frames per second
  img_animated <- image_animate(img_joined, fps = fps) 
  
  ## view animated image
  # img_animated
  
  ## save to disk
  image_write(image = img_animated,
              path = fn)
}


make_gif("anim","dag_flow.gif")


# av::av_encode_video(list.files("anim", '*.png', full.names = T),
#                     framerate = 25,
#                     output = 'test.mp4')
# 
# gifski(dir("anim/", full.names = T),gif_file = "dag_flow.gif")



