
plot_quap_preds = function(q.model, dt, pred.var, out.var = as.character(q.model[[1]])[2], plot.data = TRUE, start.list = NULL, fix.ylim = NULL, return.yhat = FALSE) {
  if (class(q.model) == "map") {
  } else {
    q.fit = quap(q.model, dt, start = start.list)
  }
  post = extract.samples(q.fit,n = 10000)
  s.from = min(dt[,pred.var]) - diff(range(dt[,pred.var]))*.15
  s.to = max(dt[,pred.var]) + diff(range(dt[,pred.var]))*.15
  pred_seq = seq(from=s.from,to=s.to,length.out=100)
  l.data = list(pred_seq)
  names(l.data) = pred.var
  l = link(q.fit,data=l.data)
  mu= apply(l,2,mean)
  ci= apply(l,2,PI)
  if (is.null(fix.ylim)) {
    ylim = range(dt[,pred.var])
  } else {
    n.ci = prod(dim(ci))
    ylim = range(sort(ci)[fix.ylim:(n.ci-fix.ylim)])
  }
  
  if(return.yhat == FALSE) {
    if (plot.data == TRUE)
      plot(dt[,pred.var], dt[,out.var],
           ylab = out.var,
           xlab = pred.var,
           ylim = ylim,
           pch = 16)
    lines(pred_seq, mu, col = "blue")
    shade(ci, pred_seq, col = adjustcolor("blue",alpha = .25))
    R2 = R2.qf(q.fit,dt)
    mtext(side = 1, bquote(R^2~"="~.(round(R2,2))),adj = .95, cex = .75, line = -1.5)
  } else {
    return(data.frame(x = pred_seq, yhat = mu))
  }
}


R2.qf = function(quap_fit,dt,out.var = as.character(quap_fit@formula[[1]])[2]){
  s = sim(quap_fit,refresh=0)
  r = apply(s,2,mean)-dt[,out.var]
  1 -var2(r)/var2(dt[,out.var])
}

plot_mu.CIs = function(q.fit,data,col = "black", spaghetti = FALSE, type = "posterior") {
  if (type == "posterior") {
    mu = link(q.fit,data=data)
    lines(data$A, colMeans(mu), col =  col)
  } else {
    mu = link(q.fit,data=data, post = extract.prior(q.fit))
    spaghetti = TRUE
  }
  if (spaghetti == F) {
    CIs = apply(mu, 2, PI)
    shade(CIs,data$A,col = adjustcolor(col,alpha = .25))
  } else {
    alpha = ifelse(type == "posterior",.05,.75)
    N.s = ifelse(type == "posterior",250,50)
    matlines(data$A,t(mu[1:N.s,]),col = adjustcolor(col,alpha = alpha),lty = 1)
  }
}


plot.pred = function(q.fit, dt, type = "posterior") {
  qs = quantile(SES, probs = seq(0,1,.25))
  par(mfrow = c(1,4), mar=c(2.5,2.5,2,.5), mgp=c(1.5,.5,0), tck=-.01)
  for (k in 2:length(qs)) {
    idx = which(dt$SES > qs[k-1] & dt$SES < qs[k])
    tmp.dt = dt[idx,]
    with(tmp.dt,
         plot(IQ~A, pch = 16, main = paste0(k-1,". quantile SES"),
              ylim = range(dt$IQ), xlim = range(dt$A)))
    plot_mu.CIs(q.fit,
                data = 
                  data.frame(A = seq(2,20,.5),
                             C = seq(2,20,.5),
                             SES = mean(dt[idx,"SES"])),
                col = adjustcolor("blue",alpha = .5),
                spaghetti = TRUE,
                type = type)
  }
}

make_gif = function(dr = NULL, fn = NULL, fps = 20) {
  library(magick)
  ## list file names and read in
  imgs <- list.files(dr, full.names = TRUE, pattern = "*.png")
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

