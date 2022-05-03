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
    sigma ~ dnorm(.25,.1)
  )

N.sim = 25
b.sds = exp(seq(log(1),log(15) , length.out = 8))
b.sds = c(1,2,3,5,10,20)
elpd.test = elpd.train = matrix(NA,nrow = N.sim, ncol = length(b.sds))
yhats = vector(mode = "list", length = length(b.sds))
for (my.seed in 1:N.sim) {
  if (my.seed <= 25)
    png(paste0("anim/F", my.seed, ".png"), width = 15, height = 10,
      res = 300, units = "cm")
  set.seed(my.seed)
  idx = sample(length(x),75)
  test.idx = sample(setdiff(1:1000,idx),75)
  # plot(x,y, col = adjustcolor("black", alpha = .1))
  # points(x[idx],y[idx], pch = 16)
  # lines(x,mu, col = "red")
  
  B.m = bs(x, knots = quantile(x,probs = seq(0,1, .2)), intercept = F)
  B.m = B.m[,-ncol(B.m)]
  par(mfrow = c(2,3), mar=c(2.5,2.5,0,.5), mgp=c(1,.1,0), tck=-.01)
  for (b.sd in b.sds) {
    q.fit = 
      quap(quap.spline.model,
           data = list(bf.s = B.m[idx,], y = y[idx]),
           start = list(b=rep(0, ncol(B.m)), sigma = .1))
    
    
    p = extract.samples(q.fit)
    yhat = rowMeans(B.m %*% t(p$b))
    yhats[[which(b.sd == b.sds)]] = rbind(yhats[[which(b.sd == b.sds)]],yhat)
    if (my.seed <= 25) {
      plot(x,y, col = adjustcolor("black", alpha = .025))
      points(x[idx],y[idx], pch = 16, 
             col = adjustcolor("black", alpha = .5), cex = .5)
      lines(x,mu, col = "red")
      lines(x,yhat,col = "blue")
      text(-4,1.2, paste0("b ~ normal(0, ",round(b.sd,1),")"), pos = 4)
    }
    elpd.train[my.seed,which(b.sds == b.sd)] = sum(lppd(q.fit))
    q.fit@data = list(bf.s = B.m[test.idx,], y = y[test.idx])
    elpd.test[my.seed,which(b.sds == b.sd)] = sum(lppd(q.fit))
  }
  if (my.seed <= 25) dev.off()
}
save(elpd.test,elpd.train, b.sds, yhats,x,y, file = "sim_lppd.Rdata")
for (b.sd in b.sds) {
  plot(x,y, col = adjustcolor("black", alpha = .025))
  matlines(x,t(yhats[[which(b.sd == b.sds)]]), lty = 1, 
           col = adjustcolor("blue",alpha = .05))
  text(-4,1.2, paste0("b ~ normal(0, ",round(b.sd,1),")"), pos = 4)
  lines(x,mu, col = "red", lwd = 2)
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


make_gif("anim/", "shrinkage.gif", fps = 2)

log_p = do.call(rbind,
                lapply(1:15, 
                       function(x) dnorm(q.fit@coef,0,x, log = TRUE)))
colnames(log_p) = c()
ax = barplot(exp(log_p[c(1,2,5),]), beside = TRUE, legend.text = paste0("sd = ",c(1,2,5)))
axis(1, at = ax[2,], labels = round(q.fit@coef,1))

log_p = do.call(rbind,
                lapply(seq(1,15, .1), 
                       function(x) dnorm(p$b[id,],0,x, log = TRUE)))
plot(seq(1,15,.1) ,exp(rowSums(log_p)),'l')

D.b = 

D.d = sum(rethinking::lppd(q.fit))


log_lik_normal = function(q.fit) {
  y = q.fit@data[["y"]]
  p = extract.samples(q.fit)
  mu = q.fit@data$bf.s %*% t(p$b)
  ll_mat = 
    apply(data.frame(i = 1:ncol(mu)),1,
        function(i) {dnorm(y,mu[,i],p$sigma[i], log = T)})
  ll_vec= apply(ll_mat,2,sum)
  
  bp_vec = 
    apply(p$b,1,function(b) dnorm(b,0,b.sd, log = T)) %>% 
    apply(2,function(bp) sum(bp))
  
  lls = cbind(ll_data = ll_vec, ll_param = bp_vec)
}

ll_mat = log_lik_normal(q.fit) 
plot(ll_mat, pch = 16, col = adjustcolor("black",alpha = .25))
