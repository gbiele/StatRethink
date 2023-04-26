library(magrittr)
set.seed(1)

Sigma = matrix(c(1,.95,.95,1), nrow = 2)
X = MASS::mvrnorm(100,mu = c(0,0), Sigma = Sigma)
Y = X %*% c(1,1) + rnorm(100)

metropolis = function(start.list = starts) {
  iter = 10001
  post.theta = matrix(NA, ncol = 4, nrow = iter) # a b1 b2 sigma
  P = vector(length = iter)
  acceptance = vector(length = iter)
  keep.old = rep(FALSE,iter)
  
  calc.P = function(theta) {
    return(
      dnorm(Y, theta[1] + X %*% theta[2:3], exp(theta[4]), log = T) %>% sum() + # likelihood
        dnorm(theta, mean = 0, sd = 1, log = T) %>% sum() # standard normal prior on everything
    )
  }
  post = vector(mode = "list", length = length(start.list))
  for (m in 1:length(start.list)) {
    post.theta[1,1] = start.list[[m]][1]
    post.theta[1,2:3] = c(start.list[[m]][2],start.list[[m]][3])
    post.theta[1,4] = start.list[[m]][4]
    P[1] = calc.P(post.theta[1,])
    for (k in 2:iter) {
      propposal.theta = post.theta[k-1,] + rnorm(4,sd = .05)
      proposal.P = calc.P(propposal.theta)
      acceptance[k] = min(1, exp(proposal.P-P[k-1]))
      if (acceptance[k] >= 1) {
        post.theta[k,] = propposal.theta
        P[k] = proposal.P
      } else {
        if (runif(1) < acceptance[k]) {
          post.theta[k,] = propposal.theta
          P[k] = proposal.P
        } else {
          post.theta[k,] = post.theta[k-1,]
          P[k] = P[k-1]
          keep.old[k] = TRUE
        }
      }
    }
    post[[m]] = post.theta
  }
  return(post)
}


make_anim = function(theta, fn = NULL) {
  library(av)
  clr = adjustcolor("blue", alpha = .25)
  clr2 = adjustcolor("blue", alpha = .05)
  ks = c(1:750, seq(751,1750,5), seq(1750,5001,10), seq(5001,10001,25))
  
  xlim.l = min(theta[,2])
  xlims.r = rep(max(theta[-(1:1000),2]), length(ks))
  xlims.r[1:350] = rep(max(theta[,2]),350)
  xlims.r[351:750] = seq(xlims.r[350],xlims.r[750], length.out = 400)
  
  
  ylims.b = rep(min(theta[-(1:1000),3]),length(ks))
  ylims.b[1:350] = rep(min(theta[,3]),350)
  ylims.b[351:750] = seq(ylims.b[350],ylims.b[750], length.out = 400)
  ylim.t = max(theta[,3])
  
  av_capture_graphics(
    {
      i = 0
      layout.mat = cbind(matrix(1,ncol = 2,nrow = 2), rbind(c(2,2),c(3,3)))
      for (k in ks) {
        i = i+1
        layout(layout.mat)
        par(mar=c(3,3,.5,.5), mgp=c(1.75,.5,0), tck=-.01)
        plot(theta[1:k,2], theta[1:k,3],
             ylim = c(ylims.b[i], ylim.t),
             xlim = c(xlim.l, xlims.r[i]),lwd = .25, 
             'l',ylab = "b2", xlab = "b1", col = clr)
        dot.col = "blue" # dot.col = ifelse(keep.old[k] == TRUE, "red","blue")
        points(theta[k,2], theta[k,3], col = dot.col, pch = 16)
        points(theta[1:k,2], theta[1:k,3], col = clr2, pch = 16)
        if (k < 1000) {
          idx = 1:k
        } else {
          idx = (k-1000):k
        }
        plot(idx,theta[idx,2],"l", ylab = "b1", xlab = "iteration", ylim = c(xlim.l, xlims.r[i]))
        plot(idx,theta[idx,3],"l", ylab = "b2", xlab = "iteration", ylim = c(ylims.b[i], ylim.t))
      }  
    },
    output = fn,
    framerate = 20,
    width = 900*1.6, height = 450*1.6, pointsize = 25*1.6)
}

post.theta = metropolis(start.list = list(c(-2,3,-3,1.5)))[[1]]
make_anim(post.theta, fn = "metropolis_hard.mp4")

post.theta.4 = metropolis(
  start.list = list(c(-2,3,3,1.5),
                    c(-2,3,-3,1.5),
                    c(-2,-3,3,1.5),
                    c(-2,-3,-3,1.5))
  )

# library(cmdstanr)
# library(posterior)
# sm = cmdstan_model("MetropolisHard.stan")
# stan_data = list(N = length(Y), Y = as.vector(Y), X = X)
# sf = sm$sample(data = stan_data, chains = 4, 
#                iter_warmup = 1000, iter_sampling = 10001, 
#                save_warmup = T,
#                init = list(list(theta = c(-2,3,3,1.5)),
#                            list(theta = c(-2,3,-3,1.5)),
#                            list(theta = c(-2,-3,3,1.5)),
#                            list(theta = c(-2,-3,-3,1.5))))

library(rethinking)
data.list = list(Y = Y, X1 = X[,1], X2 = X[,2])

model = alist(
  Y ~ normal(mu,exp(log_sigma)),
  mu <- a + b1*X1 + b2*X2,
  a ~ dnorm(0,1),
  b1 ~ dnorm(0,1),
  b2 ~ dnorm(0,1),
  log_sigma ~ dnorm(0,1)
)

ulam.startlist = list(
  a = rep(-2,3),
  b1 = c(3,3,-3,-3),
  b2 = c(3,-3,3,-3),
  log_sigma = rep(1.5,4))

u.fit = ulam(
  model,
  iter = 10001,
  warmup = 1000,
  data = data.list,
  start = ulam.startlist,
  cores = 4,
  chains = 4,
  cmdstan = TRUE)
theta = rstan::extract(u.fit@stanfit,permuted = FALSE, inc_warmup = TRUE)
theta.4 = list(
  rbind(c(-2,3,3,1.5),theta[,1,1:4]),
  rbind(c(-2,3,-3,1.5),theta[,2,1:4]),
  rbind(c(-2,-3,3,1.5),theta[,3,1:4]),
  rbind(c(-2,-3,-3,1.5),theta[,4,1:4]))

theta = theta.4[[2]]

make_anim(theta, fn = "metropolis_hard_stan.mp4")


theta.a = post.theta
theta.b = theta
tmp = rmvnorm(500000, mean = colMeans(theta[-(1:1000),2:3]), sigma = cov(theta[-(1:1000),2:3]))
d2d = MASS::kde2d(tmp[,1],tmp[,2],n = 40)
make_anim.2 = function(theta.a,theta.b,d2d) {
  library(av)
  clr = adjustcolor("blue", alpha = .5)
  clr2 = adjustcolor("blue", alpha = .05)
  ks = c(1:600, seq(601,1501,5), seq(1501,5001,10), seq(5001,10001,25))
  
  theta.list = list(theta.a, theta.b)
  av_capture_graphics(
    {
      i = 0
      xlim = range(c(theta.a[,2],theta.b[,2]))
      ylim = range(c(theta.a[,3],theta.b[,3]))
      layout.mat = matrix(c(1,2),nrow = 1)
      titles = c("Metropolis", "Hamiltonian (Stan)")
      for (k in ks) {
        i = i+1
        layout(layout.mat)
        par(mar=c(3,3,.5,.5), mgp=c(1.75,.5,0), tck=-.01)
        for (j in 1:2) {
          plot(theta.list[[j]][1:k,2], theta.list[[j]][1:k,3],
               ylim = ylim,
               xlim = xlim,lwd = .25, 
               'l',ylab = "b2", xlab = "b1", col = clr)
          mtext(titles[j], side = 3, cex = 1, line = -2, adj = .95)
          contour(d2d, levels = .01, drawlabels = FALSE, col = "grey", add = TRUE)
          dot.col = "blue" # ifelse(keep.old[k] == TRUE, "red","blue")
          points(theta.list[[j]][k,2], theta.list[[j]][k,3], col = dot.col, pch = 16, cex = .5)
          points(theta.list[[j]][1:k,2], theta.list[[j]][1:k,3], col = clr2, pch = 16, cex = .5)
          if (j == 1) text(0,-3,paste0("iteration ", k), pos = 4)
        }
      }  
    },
    output = "metropolis_hard_both.mp4",
    framerate = 20,
    width = 900, height = 450, pointsize = 17.5)
}

make_anim.2(theta.a, theta.b, d2d)

make_anim.4 = function(theta.list, fn = NULL, stop.trace = 1000) {
  library(av)
  iterations = nrow(theta.list[[1]])
  colors = c("blue","red","orange","green")
  clr = adjustcolor(colors, alpha = .25)
  clr2 = adjustcolor(colors, alpha = .05)
  ks = c(1:600, seq(601,1501,5), seq(1501,5001,10), seq(5001,10001,25))
  b1 = do.call(c,lapply(theta.list, function(x) x[,2]))
  b2 = do.call(c,lapply(theta.list, function(x) x[,3]))
  clr2 = do.call(c,lapply(colors, function(x) rep(adjustcolor(x,alpha = .05),iterations)))
  av_capture_graphics(
    {
      i = 0
      lims = apply(do.call(rbind, post.theta.4), 2, range)
      xlim = lims[, 2]
      ylim = lims[, 3]
      for (k in ks) {
        par(mfrow = c(1,1), mar=c(3,3,2,.5), mgp=c(1.75,.5,0), tck=-.01)
        plot(0, type = "n", ylim = ylim,
             xlim = xlim,lwd = .25, 
             ylab = "b2", xlab = "b1")
        idx = matrix(rep(1:k,4),ncol = 4)
        idx = do.call(c,lapply(1:4, function(x) idx[,x] + iterations*(x-1)))
        idx = sample(idx,length(idx))
        points(b1[idx],b2[idx], col = clr2[idx], pch = 16, cex = .5)
        for (j in sample(4,4)) {
          lines(theta.list[[j]][1:min(k,stop.trace),2], 
                theta.list[[j]][1:min(k,stop.trace),3],
                col = clr[j])
          points(theta.list[[j]][k,2], theta.list[[j]][k,3], 
                 col = colors[j], 
                 pch = 16, 
                 cex = .5)
        }
      }  
    },
    output = fn,
    framerate = 20,
    width = 600, height = 600, pointsize = 22.5)
}

#make_anim.4(post.theta.4, "metropolis_hard_4_chains.mp4", stop.trace = 750)
make_anim.4(theta.4, "Stan_hard_4_chains.mp4", stop.trace = 100)

