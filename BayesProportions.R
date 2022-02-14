bayes_proportions = function(n.obs.prior = 2, p.success.prior = .5, n.obs.data = 10, p.success.data = .5) {
  a.prior = n.obs.prior * p.success.prior
  b.prior = n.obs.prior - a.prior
  
  a.data = n.obs.data * p.success.data
  b.data = n.obs.data - a.data
  
  a.post = a.prior + a.data
  b.post = b.prior + b.data
  
  x = seq(.001,.999,.001)
  max.y = max(
    dbeta(x,a.prior,b.prior),
    dbeta(x,a.data,b.data),
    dbeta(x,a.post,b.post)
  )
  
  curve(dbeta(x,a.prior,b.prior), col = "blue", 
        ylim = c(0, max.y), bty = "none",
        ylab = "density", xlab = "theta")
  curve(dbeta(x,a.data,b.data), col = "red", add = T)
  curve(dbeta(x,a.post,b.post), col = "purple", add = T)
  
  legend("topright",
         lty = 1,
         col = c("red","blue","purple"),
         legend = c("Prior","Likelihood","Posterior"),
         bty = "none")
}