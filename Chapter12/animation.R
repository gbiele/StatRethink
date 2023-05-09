load("ol_fit.Rdata")

plot_dlogis.thresh = function(threshs, xlim = c(-5,5), plot.text = TRUE) {
  threshs = as.numeric(threshs)
  x.st = xlim[1]-1
  x.en = xlim[2]+1
  curve(dlogis(x),x.st,x.en,xlim = xlim,
        xlab = "latent scholastic aptitude")
  n_cat = length(threshs) + 1
  clrs =
    colorRampPalette(c("blue","blue4"))(n_cat)
  for (k in 1:n_cat) {
    st = ifelse(k == 1, x.st, threshs[k-1])
    en = ifelse(k == n_cat, x.en, threshs[k])
    x = seq(st,en,length.out = 50)
    polygon(c(min(x),x,max(x)), c(0,dlogis(x),0), col = clrs[k])
    arrows(x0 =  threshs[k], y0 = 0, y1 = dlogis(0), lty = 2, col = "red", lwd = 2,length = 0)
    if (plot.text == TRUE) {
      text((st+en)/2,dlogis(0)/2,paste0("R=",k), col = "grey50", cex = 1.5)
      text(threshs[k],dlogis(0),round(threshs[k],2), col = "red", pos = 2)
    }
  }
}

df = as.data.frame(ol.fit@data)


post = ol.fit@stanfit@sim$samples[[2]]
thresholds = post[, grepl("thresholds",names(post))]
bM = post[, "bM"]
bFE = post[, "bFE"]
bF = post[, "bF"]

pp = sim(ol.fit)
pp.counts = t(apply(pp,1, function(x) table(cut(x, breaks = seq(.5,10.5,1)))))
pp.counts = cbind(pp.counts)
obs.counts = cut(df$G3, breaks = seq(.5,10.5,1)) %>% table()
names(obs.counts) = (1:length(obs.counts))


plot_olr = function(k, plotlines = FALSE) {
  par(mfrow = c(2,1), mar = c(3,3,1,.5))
  
  threshs = as.numeric(thresholds[k,])
  plot_dlogis.thresh(threshs, xlim = c(-7,7), plot.text = FALSE)
  
  iFE = bF[k] + bFE[k] * df$Medu
  phi = bM[k] * df$Medu + iFE * df$failures
  
  if (plotlines == TRUE) {
    plot(0:10,c(0,obs.counts), 'S', xaxt = "n", ylim = c(0,150), ylab = "grade") 
    matlines(0:10, t(cbind(0,pp.counts[1:k,,drop=FALSE])), 'S', col = adjustcolor("blue",alpha = .25), lty = 1)
    lines(0:10,c(0,obs.counts), 'S', lwd = 3) 
  } else {
    rbind(obs.counts,
          pp.counts[k,]) %>%
      barplot(beside = T,
              col = c("grey25","blue"),
              ylab="proportion",
              xlab = "Grade",
              ylim = c(0,125))
    legend("topleft", fill = c("grey25","blue"),
           legend = c("data",expression("model predictions | "~theta)), bty = "n")
  }
}


plot_olr(50, plotlines = TRUE)

library(av)
av_capture_graphics(
  for (k in 1:200) {
    plot_olr(k)
  }  ,
  output = "slow.mp4",
  framerate = 5,
  width = 1000, height = 1000, pointsize = 20)


cum_prob = df$G3 %>% 
  table() %>% 
  prop.table() %>% 
  cumsum()
cum_prob = cum_prob[cum_prob!=1]
simple_thresholds = log(cum_prob/(1-cum_prob))
offsets = seq(-3,2.5,length.out = 500)
plot_olr.b = function(k) {
  set_par(mfrow = c(2,1), mar = c(3,3,1,.5))
  
  threshs = simple_thresholds+offsets[k]
  plot_dlogis.thresh(threshs, xlim = c(-8.5,8.5), plot.text = FALSE)
  
  cat.probs = diff(plogis(c(-Inf, threshs, Inf)))
  
  rbind(df$G3 %>% 
          table %>% 
          prop.table(),
        cat.probs) %>% 
    barplot(beside = T, 
            col = c("grey25","blue"),
            ylab="proportion",
            xlab = "Grade",
            ylim = c(0,.4))
  legend("topleft", fill = c("grey25","blue"), 
         legend = c("data",expression("model predictions | "~theta)), bty = "n")
  
}

plot_olr.b(2)

av_capture_graphics(
  for (k in 1:length(offsets)) {
    plot_olr.b(k)
  }  ,
  output = "swipe_thresholds.mp4",
  framerate = 20,
  width = 1000, height = 1000, pointsize = 20)
