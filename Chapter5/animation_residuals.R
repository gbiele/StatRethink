
animate_residulas = function(m.B_D,d) {
  x = d$D[idx]
  y = d$C[idx]
  
  coefs = coef(m.B_D)
  a = coefs[1]
  resid = y - (coefs[1] + x*coefs[2])
  
  black_blue_ramp = colorRampPalette(c("black", "blue"))
  bs = seq(coefs[2],0,length.out = 48)
  bs = c(rep(coefs[2],25),bs)
  k = 0
  for (b in bs) {
    k = k+1
    p = a + x*b
    y.tmp = p+resid
    par(mar = c(3,3,0,1))
    clr = ifelse(k<26,"black",black_blue_ramp(48)[k-25])
    png(paste0("animation/anim",sprintf("%03d", k), ".png"),
        width = 900, height = 750,pointsize = 24)
    plot(0,type = "n", ylim = c(-3,3), xlim = range(x),
         ylab = "",
         xlab = "D")
    clr1 = adjustcolor("black", alpha = ifelse(k < 26, 1, 1-(k-25)/48 ))
    clr2 = adjustcolor("black", alpha = ifelse(k < 26, 0, (k-25)/48 ))
    mtext(expression(widehat(B)), side = 2, line = 2, col = clr1)
    mtext(expression(B[r]), side = 2, line = 2, col = clr2)
    abline(a = a, b = b, col = "blue")
    points(x,y.tmp, col = clr)
    arrows(x0 = x, y0 = p, y1 = p + resid,length = 0, col = "blue")
    dev.off()
  }
  pp = 2
  N2 = ceiling(length(y)/pp)
  for (j in 1:N2) {
    k = k+1
    p = a + x*b
    y.tmp = p+resid
    par(mar = c(3,3,0,1))
    png(paste0("animation/anim",sprintf("%03d", k), ".png"),
        width = 900, height = 750,pointsize = 24)
    plot(0,type = "n", ylim = c(-3,3), xlim = range(x),
         ylab = expression(B[r]),
         xlab = "D")
    abline(a = a, b = b, col = "blue")
    points(x, y.tmp, col = "blue")
    is = setdiff(1:length(y),1:(pp*j))
    arrows(x0 = x[is], y0 = p[is], y1 = p[is] + resid[is],length = 0, col = "blue")
    dev.off()
  }
  k = k+1
  png(paste0("animation/anim",sprintf("%03d", k), ".png"),
      width = 900, height = 750,pointsize = 24)
  plot(0,type = "n", ylim = c(-3,3), xlim = range(x),
       ylab = expression(B[r]),
       xlab = "D")
  points(x, a + resid, col = "blue")
  dev.off()
  
  system(glue::glue("ffmpeg -y -pattern_type sequence -i animation/anim%03d.png -c:v libx264 -pix_fmt yuv420p residuals.mp4"))
}

  