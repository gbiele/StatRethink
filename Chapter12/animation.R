load("ol_fit.Rdata")
post = ol.fit@stanfit@sim$samples[[1]]
thresholds = post[, grepl("thresholds",names(post))]
bM = post[, "bM"]
bFE = post[, "bFE"]
bF = post[, "bF"]

plot_olr = function(k) {
  set_par(mfrow = c(2,1), mar = c(3,3,1,.5))
  
  threshs = as.numeric(thresholds[k,])
  plot_dlogis.thresh(threshs, xlim = c(-7,7), plot.text = FALSE)
  
  iFE = bF[k] + bFE[k] * df$Medu
  phi = bM[k] * df$Medu + iFE * df$failures
  
  cat.probs = 
    do.call(rbind,
            lapply(1:length(phi),
                   function(j)
                     diff(plogis(c(-Inf, threshs - phi[j], Inf))))) %>% 
    colMeans()
  
  rbind(df$G1 %>% 
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

plot_olr(100)

library(av)
av_capture_graphics(
  for (k in 1:200) {
    plot_olr(k)
  }  ,
  output = "slow.mp4",
  framerate = 5,
  width = 1000, height = 1000, pointsize = 20)

av_capture_graphics(
  for (k in 1:500) {
    plot_olr(k)
  }  ,
  output = "medium.mp4",
  framerate = 10,
  width = 1000, height = 1000, pointsize = 20)


library(av)
av_capture_graphics(
  for (k in 1:1000) {
    plot_olr(k)
  }  ,
  output = "fast.mp4",
  framerate = 25,
  width = 1000, height = 1000, pointsize = 20)
