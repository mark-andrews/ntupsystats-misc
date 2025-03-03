library(lme4)
library(lme4)
library(lmerTest)
library(tidyverse)

ggplot(sleepstudy, 
       aes(x = Reaction)
) + geom_histogram(bins = 20)

# fit a normal distribution to the reaction times
M_7_1 <- lm(Reaction ~ 1, data = sleepstudy)
coef(M_7_1) # mean of the normal distribution
sigma(M_7_1) # sd of normal distribution

M_7_2 <- lm(Reaction ~ 1, 
            data = filter(sleepstudy, Subject == '308')
)
coef(M_7_2)
sigma(M_7_2)


M_7_3 <- lm(Reaction ~ 1, 
            data = filter(sleepstudy, Subject == '309')
)
coef(M_7_3)
sigma(M_7_3)

# multilevel normal model
M_7_4 <- lmer(Reaction ~ 1 + (1|Subject), data = sleepstudy)
summary(M_7_4)


# Plot scatterplots for each subject --------------------------------------

ggplot(sleepstudy,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + stat_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~Subject) +
  theme(legend.position = 'none')


# multilevel linear model of effect of 
# sleep deprivation on reaction time
M_7_5 <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)
M_7_5a <- lmer(Reaction ~ 1 + Days + (1 + Days|Subject), data = sleepstudy)

summary(M_7_5)


b <- fixef(M_7_5)
b[1] - qnorm(p = 0.975) * 24.741
b[1] + qnorm(p = 0.975) * 24.741

# 95% inner range of the normal distribution of intercept terms
b[1] + c(-1,1) * qnorm(p = 0.975) * 24.741

# 95% inner range of the normal distribution of slope terms
b[2] + c(-1,1) * qnorm(p = 0.975) * 5.922


