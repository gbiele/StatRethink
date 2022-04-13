


animate_residulas = function(m.B_D,d) {
  
  baseplot = function(k, b = NULL, is = NULL, plot.abline = T) {
    png(paste0("animation/anim",sprintf("%03d", k), ".png"),
        width = 900, height = 750,pointsize = 24)
    par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01, bg = "white")
    plot(0,type = "n", ylim = c(-3,3), xlim = range(x),
         ylab = "",
         xlab = "D")
    if(plot.abline == T) {
      abline(a = a, b = b, col = "blue")
      legend("topleft",
             lty = 1,
             col = "blue",
             bty = "n",
             legend = expression(widehat(B)))
    }
    
    arrows(x0 = x[is], y0 = p[is], y1 = p[is] + resid[is],
           length = 0, col = adjustcolor("blue",alpha = .5))
    points(x,y.tmp, col = clr)
    mtext(expression(B[ ]), side = 2, line = 2, col = clr1)
    mtext(expression(B[R]), side = 2, line = 2, col = clr2)
    dev.off()
    
  }
  
  x = d$D[idx]
  y = d$C[idx]
  
  coefs = coef(m.B_D)
  a = coefs[1]
  resid = y - (coefs[1] + x*coefs[2])
  
  k = 0
  pp = 2
  N2 = ceiling(length(y)/pp)
  b = coefs[2]
  clr = "black"
  clr1 = "black"
  clr2 = adjustcolor("black",alpha = 0)
  p = a + x*b
  y.tmp = p+resid
  for (j in 1:(N2+25)) {
    k = k + 1
    if (j < 26) {
      is = c()
      } else {
      is = 1:(pp*(j-25))
    }
    baseplot(k,
             b = b,
             is = is,
             plot.abline = ifelse(j<13,F,T))

  }
  
  black_blue_ramp = colorRampPalette(c("black", "blue"))
  bs = seq(coefs[2],0,length.out = 48)
  bs = c(rep(coefs[2],25),bs)
  for (b in bs) {
    k = k+1
    p = a + x*b
    y.tmp = p+resid
    clr = ifelse(k<89,"black",black_blue_ramp(48)[k-88])
    
    is = 1:length(y)
    clr1 = adjustcolor("black", alpha = ifelse(k < 26, 1, 1-(k-25)/48 ))
    clr2 = adjustcolor("black", alpha = ifelse(k < 26, 0, (k-25)/48 ))
    
    baseplot(k, b = b, is = is)
  }
  
  for (j in 1:N2) {
    k = k+1

    is = setdiff(1:length(y),1:(pp*j))
    baseplot(k, b = b, is = is)
  }
  k = k+1
  baseplot(k, b = b, is = is, plot.abline = F)
  
  system(glue::glue("ffmpeg -y -pattern_type sequence -i animation/anim%03d.png -c:v libx264 -pix_fmt yuv420p residuals.mp4"))
}

  