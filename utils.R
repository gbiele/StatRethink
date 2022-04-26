N = 1000
x = rnorm(N,mean = 0)
bf.s = splines::bs(x,knots = 3)
bf.b = matrix(c(1,1,1,0),ncol = 1)
y = rnorm(N,bf.s %*% bf.b , sd = .1)
par(mfrow = c(1,1))
plot(x,y)
lines(sort(x),sort(bf.s %*% bf.b), col = "red")


N = 500
x = rnorm(N,mean = 0)
bf.s = splines::bs(x,knots = 3)
y = rnorm(N,b1*x - b2*x^2, sd = 1)
par(mfrow = c(1,1))
plot(x,y)
lines(sort(x),b1*sort(x) - b2*sort(x)^2, col = "red")

par(mfrow = c(5,5), mar=c(3,3,.5,.5), mgp=c(2,.7,0), tck=-.01)
for (k in 1:5) {
  idx = sample(N,50)
  plot_xy = function(x,y) {
    plot(x[idx],y[idx], 
         ylab = "y",
         xlab = "x",
         xlim = quantile(x,c(.01,.99)), 
         ylim = quantile(y,c(.01,.99)))
    lines(sort(x),b1*sort(x) - b2*sort(x)^2, col = "red")
  }
  
  dt = data.frame(x = x[idx], y = y[idx])
  
  q.model = alist(
    y ~ dnorm(mu,exp(log_sigma)),
    mu <- a + b[1]*x,
    a ~ dnorm(0.5,1),
    b ~ dnorm(0,10),
    log_sigma ~ dnorm(0,1)
  )
  
  plot_xy(x,y)
  plot_quap_preds(q.model,dt,"x", start.list = list(b=rep(0,5)), plot = F)
  
  plot_xy(x,y)
  q.model[[2]]  = alist(mu <- a + b[1]*x + b[2]*x^2)[[1]]
  plot_quap_preds(q.model,dt,"x", start.list = list(b=rep(0,5)), plot = F)
  
  q.model[[2]]  = alist(mu <- a + b[1]*x + b[2]*x^2 + b[3]*x^3)[[1]]
  plot_xy(x,y)
  plot_quap_preds(q.model,dt,"x", start.list = list(b=rep(0,5)), plot = F)
  
  q.model[[2]]  = alist(mu <- a + b[1]*x + b[2]*x^2 + b[3]*x^3 + b[4]*x^4)[[1]]
  plot_xy(x,y)
  plot_quap_preds(q.model,dt,"x", start.list = list(b=rep(0,5)), plot = F)
  
  q.model[[2]]  = alist(mu <- a + b[1]*x + b[2]*x^2 + b[3]*x^3 + b[4]*x^4 + b[5]*x^5)[[1]]
  plot_xy(x,y)
  plot_quap_preds(q.model,dt,"x", start.list = list(b=rep(0,5)), plot = F)
}

q.model = alist(
  brain_std ~ dnorm(mu,exp(log_sigma)),
  mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4 + b[5]*mass_std^5,
  a ~ dnorm(0.5,1),
  b ~ dnorm(0,10),
  log_sigma ~ dnorm(0,1)
)



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
}

plot_quap_preds(q.model,dt,"mass_std", start.list = list(b=rep(0,5)))
plot(brain_std~mass_std, data = dt, ylim = c(0,1.5))
plot_quap_preds(q.model,dt,"mass_std", start.list = list(b=rep(0,5)),plot.data = FALSE)
