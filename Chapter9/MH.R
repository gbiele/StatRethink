library(magrittr)
set.seed(1)
x = rnorm(100)

iter = 3001
post.mu = vector(length = iter)
post.log_sigma = vector(length = iter)
P = vector(length = iter)
acceptance = vector(length = iter)
keep.old = rep(FALSE,iter)

calc.P = function(mu,log_sigma,x) {
  return(
    dnorm(x, mu, exp(log_sigma), log = T) %>% sum() + # likelihood
      dnorm(mu, mean = 0, sd = 1, log = T) +          # prior mu
      dnorm(log_sigma, mean = 0, sd = 1, log = T)     # prior sigma
  )
}


post.mu[1] = -2
post.log_sigma[1] = 1.5
P[1] = calc.P(post.mu[1], post.log_sigma[1], x)

for (k in 2:iter) {
  propposal.mu = rnorm(1,mean = post.mu[k-1], sd = .05)
  propposal.log_sigma = rnorm(1, mean = post.log_sigma[k-1], sd = .05)
  proposal.P = calc.P(propposal.mu, propposal.log_sigma, x)
  acceptance[k] = min(1, exp(proposal.P-P[k-1]))
  if (acceptance[k] >= 1) {
    post.mu[k] = propposal.mu
    post.log_sigma[k] = propposal.log_sigma
    P[k] = proposal.P
  } else {
    if (runif(1) < acceptance[k]) {
      post.mu[k] = propposal.mu
      post.log_sigma[k] = propposal.log_sigma
      P[k] = P[k-1]
    } else {
      post.mu[k] = post.mu[k-1]
      post.log_sigma[k] = post.log_sigma[k-1]
      P[k] = proposal.P
      keep.old[k] = TRUE
    }
  }
}

clr = adjustcolor("blue", alpha = .25)
clr2 = adjustcolor("blue", alpha = .05)
ks = c(1:250, seq(251,1501,5), seq(1501,3001,10))

ylims.b = rep(-.3,length(ks))
ylims.b[1:250] = rep(min(post.mu),250)
ylims.b[251:500] = seq(ylims.b[250],ylims.b[500], length.out = 250)
ylim.t = max(post.mu)

xlim.l = min(exp(post.log_sigma))
xlims.r = rep(1.2, length(ks))
xlims.r[1:250] = rep(exp(max(post.log_sigma)),250)
xlims.r[251:500] = seq(xlims.r[250],xlims.r[500], length.out = 250)
i = 0
layout.mat = cbind(matrix(1,ncol = 2,nrow = 2), rbind(c(2,2),c(3,3)))
for (k in ks) {
  i = i+1
  png(filename = paste0("sim/MH",stringr::str_pad(i,4,pad = "0"),".png"),width = 10, height = 5, units = "cm", res = 300,pointsize = 7)
  layout(layout.mat)
  par(mar=c(3,3,.5,.5), mgp=c(1.75,.5,0), tck=-.01)
  plot(exp(post.log_sigma)[1:k], post.mu[1:k],
       ylim = c(ylims.b[i], ylim.t),
       xlim = c(xlim.l, xlims.r[i]),
       'l',ylab = "mu", xlab = "sigma", col = clr)
  abline(h = mean(x), col = adjustcolor("red",alpha = .2))
  abline(v = sd(x), col = adjustcolor("red",alpha = .2))
  dot.col = ifelse(keep.old[k] == TRUE, "red","blue")
  points(exp(post.log_sigma[k]), post.mu[k], col = dot.col, pch = 16)
  points(exp(post.log_sigma[1:k]), post.mu[1:k], col = clr2, pch = 16)
  if (k < 500) {
    idx = 1:k
  } else {
    idx = (k-500):k
  }
  plot(idx,post.mu[idx],"l", ylab = "mu", xlab = "iteration")
  plot(idx,exp(post.log_sigma[idx]),"l", ylab = "sigma", xlab = "iteration")
  dev.off()
}

make_gif("sim","mh.gif")

