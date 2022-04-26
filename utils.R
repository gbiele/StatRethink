



plot_quap_preds = function(q.model, dt, pred.var, out.var = as.character(q.model[[1]])[2], plot.data = TRUE, start.list = NULL, fix.ylim = NULL) {
  q.fit = quap(q.model, dt, start = start.list)
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
}


R2.qf = function(quap_fit,dt,out.var = as.character(quap_fit@formula[[1]])[2]){
  s = sim(quap_fit,refresh=0)
  r = apply(s,2,mean)-dt[,out.var]
  1 -var2(r)/var2(dt[,out.var])
}
