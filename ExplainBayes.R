### prior ####
prior_a = 1.1
prior_b = 1.1


### observed data ###
size = 10
prob = .2
successes = size*prob


### ABC ###

### simulate data ###
prior = rbeta(100000,prior_a,prior_b)
r = rbinom(100000,size,prior)
post = prior[r == successes]

### plot prior ###
par(mfrow = c(2,3))
h = hist(prior,xlab = "theta")

### filter###
tmp.data = data.frame(prior = prior, keep = factor(r == successes)) 
histStack(prior~keep,tmp.data, col = c("red","green"), ylab = "theta", main = "Filter")

### plot posterior ###
hist(post, xlim = c(0,1), xlab = "theta", main = "Posterior")

### Bayesian updating ###
curve(dbeta(x,prior_a,prior_b), xlim = c(0,1), main = "Prior density", xlab = "theta")
curve(dbeta(x,successes,size - successes), xlim = c(0,1), main = "Likelihood", xlab = "theta")

post_a = successes + prior_a
post_b = size - successes + prior_b
curve(dbeta(x, post_a, post_b), xlim = c(0,1), main  = "Posterior density", xlab = "theta")

par(mfrow = c(1,2))
x = seq(0,1,.001)
ymax = max(dbeta(x,prior_a,prior_b),dbeta(x,successes,size - successes),dbeta(x, post_a, post_b))
curve(dbeta(x,prior_a,prior_b), xlim = c(0,1), main = "Prior-Likelihood-Posterior", ylim = c(0,ymax), ylab = "density", col = "blue", xlab = "theta")
curve(dbeta(x,successes,size - successes), xlim = c(0,1),add = T, col = "red")
curve(dbeta(x, post_a, post_b), xlim = c(0,1), add = T, col = "purple")

hist(post, xlim = c(0,1), xlab = "theta", freq = F, col = adjustcolor("black", alpha = .1), border = NA, main = "Simulated & analytic posterior")
curve(dbeta(x, post_a, post_b),0,1,add = T, col = "purple")


library(ggplot2)
library(data.table)
library(ggdist)
library(magrittr)
library(patchwork)
library(distributional)
### observed data ###
size = 10
prob = .2
successes = size*prob

### prior ####
prior_a = 1
prior_b = 1

### simulate data ###
N.sim = 100000
sim.data = 
  data.table(prior.theta = rbeta(N.sim,prior_a,prior_b))

sim.data %>% 
  .[, r := rbinom(1, size, prior.theta), by = 1:N.sim] %>% 
  .[, keep := ifelse(r == successes, T, F)]

# p.prior.abc = sim.data %>% 
#   ggplot(aes(x = prior.theta)) +
#   stat_dots(quantiles = 200)
p.prior.abc = sim.data %>% 
  ggplot(aes(x = prior.theta)) +
  geom_dots(color = NA, binwidth = .025) + 
  theme(legend.position = "top")

# p.check.abc = sim.data %>% 
#   ggplot(aes(x = prior.theta, fill = keep)) +
#   geom_histogram(breaks = seq(0,1,.025)) + 
#   theme(legend.position = "top")

p.check.abc = sim.data %>% 
  ggplot(aes(x = prior.theta, fill = keep, group = NA)) +
  geom_dots(color = NA, binwidth = .025) + 
  theme(legend.position = "top") 

p.posterior.abc = sim.data %>%
  .[keep == T] %>% 
  ggplot(aes(x = prior.theta,fill = keep, group = NA)) +
  geom_dots(color = NA, binwidth = .025) + 
  theme(legend.position = "none") + coord_cartesian(xlim = c(0,1))

# p.posterior.abc = sim.data[keep == T] %>% 
#   ggplot(aes(x = prior.theta, fill = keep, color = keep)) +
#   stat_dots(quantiles = 200) + 
#   theme(legend.position = "none")

p.prior.abc | p.check.abc | p.posterior.abc
steps = c("prior", "likelihood", "posterior")
analytic.data = data.table(
  step = factor(steps, levels = steps),
  alpha = c(prior_a, successes, prior_a + successes),
  beta = c(prior_b, size-successes, prior_b + size-successes)
)

analytic.data %>% 
  ggplot(aes(xdist = dist_beta(alpha,beta), group = step, color = step)) + 
  stat_slab(fill = NA) + 
  facet_grid(~step)

sim.data %>%
  .[keep == T] %>% 
  ggplot(aes(x = prior.theta,fill = keep, group = NA)) +
  geom_dots() + 
  stat_slab(data = analytic.data[step == "posterior"], 
            aes(xdist = dist_beta(alpha,beta), group = step, x = NULL),
            fill = NA, color = "blue")

