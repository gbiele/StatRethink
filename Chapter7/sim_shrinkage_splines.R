N = 1000
set.seed(123456)
x = sort(rnorm(N,mean = 0))
bf.s = splines::bs(x,knots = 3)
bf.b = matrix(c(1,1,1,0),ncol = 1)
mu = bf.s %*% bf.b
y = rnorm(N, mu , sd = .1)
par(mfrow = c(1,1))
plot(x,y)

quap.spline.model = 
  alist(
    y ~ dnorm(mu,sigma),
    mu <- bf.s %*% b ,
    b ~ dnorm(0,b.sd),
    sigma ~ dnorm(.5,.01)
  )

for (my.seed in 1:50) {
  png(paste0("F", my.seed, ".png"), width = 15, height = 10,
      res = 300, units = "cm")
  set.seed(my.seed)
  idx = sample(length(x),25)
  # plot(x,y, col = adjustcolor("black", alpha = .1))
  # points(x[idx],y[idx], pch = 16)
  # lines(x,mu, col = "red")
  
  B.m = bs(x, knots = quantile(x,probs = seq(0,1, .2))) 
  par(mfrow = c(3,4), mar = c(3,3,.5,.5))
  for (b.sd in exp(seq(log(1),log(100) , length.out = 12))) {
    plot(x[idx],y[idx], pch = 16, col = adjustcolor("black", alpha = .25), cex = .5)
    lines(x,mu, col = "red")
    q.fit = 
      quap(quap.spline.model,
           data = list(bf.s = B.m[idx,], y = y[idx]),
           start=list( b=rep( 0 , ncol(B.m) ) ))
    
    p = extract.samples(q.fit)
    yhat = rowMeans(B.m %*% t(p$b))
    lines(x,yhat,col = "blue")
    
  }
  dev.off()
}



#points(x[idx],colMeans(link(q.fit,n = 1e4)),col = "blue")
