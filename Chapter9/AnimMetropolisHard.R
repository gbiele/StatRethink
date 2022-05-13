library(magrittr)
set.seed(1)

Sigma = matrix(c(1,.95,.95,1), nrow = 2)
X = MASS::mvrnorm(100,mu = c(0,0), Sigma = Sigma)
Y = X %*% c(1,1) + rnorm(100)


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

post.theta[1,1] = -2
post.theta[1,2:3] = c(3,-3)
post.theta[1,4] = 1.5
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

plot(post.theta[,2:3], pch = 16, col = adjustcolor("black",alpha = .25))
title(round(mean(acceptance),2))

clr = adjustcolor("blue", alpha = .25)
clr2 = adjustcolor("blue", alpha = .05)
ks = c(1:600, seq(601,1501,5), seq(1501,5001,10), seq(5001,10001,25))

xlim.l = min(post.theta[,2])
xlims.r = rep(max(post.theta[-(1:1000),2]), length(ks))
xlims.r[1:350] = rep(max(post.theta[,2]),350)
xlims.r[351:600] = seq(xlims.r[350],xlims.r[600], length.out = 250)


ylims.b = rep(min(post.theta[-(1:1000),3]),length(ks))
ylims.b[1:350] = rep(min(post.theta[,3]),350)
ylims.b[351:600] = seq(ylims.b[350],ylims.b[600], length.out = 250)
ylim.t = max(post.theta[,3])


library(av)
av_capture_graphics(
  {
    i = 0
    layout.mat = cbind(matrix(1,ncol = 2,nrow = 2), rbind(c(2,2),c(3,3)))
    file.remove(list.files("sim", pattern = "*.png", full.names = T))
    for (k in ks) {
      i = i+1
      layout(layout.mat)
      par(mar=c(3,3,.5,.5), mgp=c(1.75,.5,0), tck=-.01)
      plot(post.theta[1:k,2], post.theta[1:k,3],
           ylim = c(ylims.b[i], ylim.t),
           xlim = c(xlim.l, xlims.r[i]),lwd = .25, 
           'l',ylab = "b2", xlab = "b1", col = clr)
      dot.col = ifelse(keep.old[k] == TRUE, "red","blue")
      points(post.theta[k,2], post.theta[k,3], col = dot.col, pch = 16)
      points(post.theta[1:k,2], post.theta[1:k,3], col = clr2, pch = 16)
      if (k < 1000) {
        idx = 1:k
      } else {
        idx = (k-1000):k
      }
      plot(idx,post.theta[idx,2],"l", ylab = "b1", xlab = "iteration", ylim = c(xlim.l, xlims.r[i]))
      plot(idx,post.theta[idx,3],"l", ylab = "b2", xlab = "iteration", ylim = c(ylims.b[i], ylim.t))
    }  
  },
  output = "metropolis_hard.mp4",
  framerate = 20,
  width = 1200, height = 600, pointsize = 30)
