set.seed(11)
x = seq(-3,3,length.out = 500)
knot.locations = seq(-3,3, length.out = 25)
X.splines = bs(x, knots = knot.locations)

matplot(X.splines,type = 'l')

b = rnorm(ncol(X.splines))

pre.y = X.splines %*% b
y = pre.y + rnorm(length(x),0,.3)
plot(x,pre.y,"l")
points(x,y)

df = data.frame(x = x, y = y)

num_knots <-20
knot_list <-quantile(df$x,probs=seq(0,1,length.out=num_knots))
B <-bs(df$x, knots=knot_list, degree=3)

spline.model = 
  alist(
    y ~ dnorm(mu,sigma),
    mu <- B %*% w ,
    w ~ dnorm(0,1),
    sigma ~ dexp(1)
  )
data.list = list(y = df$y, B = B)
spline.quap <-quap(
  spline.model,
  data = data.list,
  start = list(w = rep(0,ncol(B)))
  )

mu <- rethinking::link(spline.quap)
mu_PI <- apply(mu, 2, PI, 0.89)

plot(x,y)
shade(mu_PI, df$x)
