
mcmc_intervals_sorted = function(draws) {
  dt = draws %>% mcmc_intervals_data()
  dt$parameter = factor(dt$parameter, levels = dt$parameter[order(dt$m)])
  
  dt %>% 
    ggplot(aes(x = parameter, y = m)) + 
    geom_linerange(aes(ymin = ll, ymax = hh)) + 
    geom_linerange(aes(ymin = l, ymax = h), size = .8) + 
    geom_point() + 
    coord_flip() + 
    theme_classic() +
    ylab("estimate")
}
