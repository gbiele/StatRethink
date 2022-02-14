bayes_proportions = function(n.prior = 2, p.prior = .5, n.data = 10, p.data = .5, max.y = NULL) {
  a.prior = n.prior * p.prior
  b.prior = n.prior - a.prior
  
  a.data = n.data * p.data
  b.data = n.data - a.data
  
  a.post = a.prior + a.data
  b.post = b.prior + b.data
  
  if (is.null(max.y)) {
    x = seq(.001,.999,.001)
    max.y = max(
      dbeta(x,a.prior,b.prior),
      dbeta(x,a.data,b.data),
      dbeta(x,a.post,b.post)
    )
  }
  
  params = 
    paste0(
      c("n.prior","p.prior","n.data", "p.data"),": ",
      c(n.prior,p.prior,n.data, p.data))
  params = paste0(paste(params[1:2],collapse = ", "),"\n",
                 paste(params[3:4],collapse = ", "))
  
  curve(dbeta(x,a.prior,b.prior), col = "blue", 
        ylim = c(0, max.y), bty = "none", lwd = 2,
        ylab = "density", xlab = "theta", n = 500,
        main = params)
  curve(dbeta(x,a.data,b.data), col = "red", add = T, lwd = 2, n = 500)
  curve(dbeta(x,a.post,b.post), col = "purple", add = T, lwd = 2, n = 500)
  
  legend("topright",
         lty = 1,
         lwd = 2,
         col = c("blue","red","purple"),
         legend = c("Prior","Likelihood","Posterior"),
         bty = "n")
}