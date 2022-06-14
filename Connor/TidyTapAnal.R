# load libraries
library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(ez)
library(tidyr)
library(lme4)
library(lmerTest)
library(effects)

######################################################## STUDY BACKGROUND ########################################################
# The pleasurable urge to move to music has been shown to follow a Wundt curve with rhythmic complexity. This has been           # 
# explained in terms of predictive coding where we move at moderate levels of complexity to reduce prediction errors because     #
# here there are errors to correct but not so many that we don't know when they'll occur in time. We tested this by asking       #
# subjects to rate music that varied in complexity (quantified here as (pulse) entropy) in terms of 1) how much they wanted to   #
# move, 2) how much they liked it, 3) how energetic it sounded, and only for the first presentation, 4) how certain they were    #
# that they knew the song and 5) how often they listen to that style of music. We wanted to test whether subjects' ratings would #
# be higher when they actively tapped along to the music compared to when they passively listened to it, particularly at         #
# moderate levels of rhythmic complexity. Since everyone's subjective experience of complexity and baseline engagement with      #
# music should differ somewhat based on experience and rhythmic ability, we fit random slopes and intercepts for complexity for  #
# each subject and tried to predict the urge to move using linear and quadratic slopes for rhythmic complexity (pulse entropy).  #
##################################################################################################################################

# read in data
setwd("connor")
TapBeh <- readRDS("TapBehfull.RDA")

BehSub <- na.omit(TapBeh) # probably not necessary to cut out NaNs for this analysis
BehSub <- BehSub %>% group_by(Subject, Entropy, Song, Condition) %>% #filter(Subject!=24) %>% subject told us they had ADHD halfway through experiment
  summarize(AvgMove=mean(Move), 
            AvgPleasure=mean(Pleasure), 
            AvgArousal=mean(Arousal), 
            AvgFam=mean(Familiarity), 
            AvgStyle=mean(Style))

BehSum <- BehSub %>% group_by(Entropy, Song, Condition) %>%
  summarize(SumMove=mean(AvgMove), 
            SumPleasure=mean(AvgPleasure), 
            SumArousal=mean(AvgArousal), 
            SumFam=mean(AvgFam), 
            SumStyle=mean(AvgStyle))#,
#MoveSE=sd(AvgMove)/(sqrt(n())), PleasureSE=sd(AvgPleasure)/(sqrt(n())), ArousalSE=sd(AvgArousal)/(sqrt(n())), MoveZSE=sd(AvgZMove)/(sqrt(n())), PleasureZSE=sd(AvgZPleasure)/(sqrt(n())), ArousalZSE=sd(AvgZArousal)/(sqrt(n())))


# plot raw average ratings to compare to predictors with nested effects
ggplot(data=BehSub, aes(x=Entropy, linetype=Condition, shape=Condition))+
  facet_wrap(~Condition)+
  scale_color_manual(values=c("Move"="#ff522f"))+#, "Pleasure"="#5bd1d7", "Arousal"="#004e61"))+
  scale_fill_manual(values=c("Move"="#ff522f"))+#, "Pleasure"="#5bd1d7", "Arousal"="#004e61"))+
  geom_hline(yintercept=0, color="black")+
  geom_line(aes(y=AvgMove, group=Subject))+
  geom_line(data=BehSum, aes(y=SumMove, color="Move",), size=1.25)+
  #geom_line(data=BehSum, aes(y=SumPleasure, color="Pleasure"), size=1.25)+
  #geom_line(data=BehSum, aes(y=SumArousal, color="Arousal"), size=1.25)+
  scale_y_continuous(limits=c(-1,1))+
  scale_size(guide=FALSE)+
  labs(title="Average Raw Ratings by Rhythmic Complexity", y="Rating (0 is neutral)", x="Pulse Entropy")+
  theme_bw()+theme(plot.title=element_text(size=24),axis.title.y=element_text(size=18),
                   axis.title.x=element_text(size=18), axis.text.x=element_text(size=16),axis.text.y=element_text(size=16),
                   legend.title=element_blank(),legend.text=element_text(size=16))

# make some filthy frequentist models
m0 <- lmer(AvgMove~1+(1|Subject), data=BehSub) # null/empty/intercepts-only model
mt <- lmer(AvgMove~1+Condition+(1|Subject), data=BehSub) # effect of tapping vs. listening condition
ml <- lmer(AvgMove~1+Entropy*Condition+(1+Entropy|Subject), data=BehSub) # add linear effect of rhythmic complexity
mq <- lmer(AvgMove~1+Condition*poly(Entropy,2)+(1+poly(Entropy,2)|Subject), data=BehSub) # add quadratic effects of rhythmic complexity
anova(m0,mt,ml,mq) # quadratic model is the best fit, yippee!
summary(mq) # as predicted, there is a significant negative quadratic trend for rhythmic complexity indicating an inverted U-shaped curve 
            # and subjects reported higher urges to move to songs when they were tasked with tapping along to them!

# best Bayesian approach to demonstrate this?

library(rstanarm)
options(mc.cores = 4)
m0.bayes <- stan_lmer(AvgMove~1+(1|Subject), data=BehSub) # null/empty/intercepts-only model
mt.bayes <- stan_lmer(AvgMove~1+Condition+(1|Subject), data=BehSub) # effect of tapping vs. listening condition
ml.bayes <- stan_lmer(AvgMove~1+Entropy*Condition+(1+Entropy|Subject), data=BehSub) # add linear effect of rhythmic complexity
mq.bayes <- stan_lmer(AvgMove~1+Condition*poly(Entropy,2)+(1+poly(Entropy,2)|Subject), data=BehSub) # add quadratic effects of rhythmic complexity

summary(ml.bayes)[, "Rhat"] %>% hist(xlim = c(1,1.1), main = "", xlab = "Rhat")
abline(v = c(1.01,1.05), col = "red", lty = c(3,1))


loo_compare(
  loo(m0.bayes),
  loo(mt.bayes),
  loo(ml.bayes),
  loo(mq.bayes))


my.prior = c(
  prior(class = "b", normal(0,3)),
  prior(class = "sds", normal(0,3))
  )

BehSub$AvgMove.c = as.numeric(cut(BehSub$AvgMove,breaks = seq(-1.001,1.001, length.out = 11)))
f = AvgMove.c ~ s(Entropy, by = Condition) + Condition + (1 | Subject)

standata = make_standata(f,family = cumulative(link = "probit"),data = BehSub)

my.inits = lapply(1:4, function(x) {
  list(
    intercept = seq(-2,2,length.out = standata$nthres),
    bs = rep(0,standata$Ks),
    zs_1_1 = rep(0,standata$knots_1[1]),
    sd_1 = rep(0.001,standata$M_1)
  )
})


sf = brm(f,
         data = BehSub,
         backend = "cmdstanr",
         prior = my.prior,
         family = cumulative(link = "probit"),
         inits = my.inits)

pp = posterior_epred(sf)
pred = 
  apply(pp,1, function(x) {
    apply(x,1, function(z) sum(z*(1:10)))
  })

p = apply(pp,1,colMeans)
CIs = apply(p,1,function(x) quantile(x,probs = c(.025,.975)))
h = 
  rbind(rowMeans(p),
        prop.table(table(BehSub$AvgMove.c))) %>% 
  barplot(beside = TRUE, ylim = c(0,max(CIs)))
arrows(x0 = h[2,], y0 = CIs[1,], y1 = CIs[2,], length = 0)

plot(colMeans(pred),colMeans(pred)-BehSub$AvgMove.c)
cor(colMeans(pred),colMeans(pred)-BehSub$AvgMove.c)

BehSub$Entropy.c = as.numeric(cut(BehSub$Entropy,3))
qs = c(max(BehSub$Entropy[BehSub$Entropy.c == 1]),max(BehSub$Entropy[BehSub$Entropy.c == 2]))
qs = quantile(BehSub$Entropy,c(.25,.75))
p = conditional_effects(sf,"Entropy:Condition")[[1]]
p %>% 
  ggplot(aes(x = Entropy, y = estimate__, color = Condition, fill = Condition)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = .25, color = NA) + 
  geom_vline(xintercept = qs)

pp.test.t1 = 
  posterior_epred(
    sf, 
    newdata = BehSub[BehSub$Entropy <= qs[1],]) %>% 
  apply(1,colMeans) %>% 
  apply(2, function(x) sum(x*(1:10)))

pp.test.t2 = 
  posterior_epred(
    sf, 
    newdata = BehSub[BehSub$Entropy > qs[1] & BehSub$Entropy <= qs[2],]) %>% 
  apply(1,colMeans) %>% 
  apply(2, function(x) sum(x*(1:10)))

pp.test.t3 = 
  posterior_epred(
    sf, 
    newdata = BehSub[BehSub$Entropy > qs[2],]) %>% 
  apply(1,colMeans) %>% 
  apply(2, function(x) sum(x*(1:10)))

par(mfrow = c(1,2))
hist(pp.test.t2-pp.test.t1)
hist(pp.test.t1-pp.test.t3)
