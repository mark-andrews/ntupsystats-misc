library(tidyverse)
library(lme4)

# make a scatterplot of Reaction against Days
# for each subject, include line of best fit
ggplot(sleepstudy,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + stat_smooth(method = 'lm', se = F) +
  facet_wrap(~Subject)


# Multilevel linear model
# varying intercept AND varying slope multilevel model
M_7_1 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)


# ordinary linear model
M_7_lm <- lm(Reaction ~ Days, data = sleepstudy)
M_7_lm <- lm(Reaction ~ 1 + Days, data = sleepstudy)
M_7_lmer <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), data = sleepstudy)
M_7_lmer_2 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

summary(M_7_1)


# varying intercept only multilevel model
M_7_2 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
summary(M_7_2)

# varying slope only multilevel model
M_7_3 <- lmer(Reaction ~ Days + (0 + Days | Subject), data = sleepstudy)
summary(M_7_3)


# multilevel not regression model
# aka a random effects normal model
M_7_4 <- lmer(Reaction ~ 1 + (1 | Subject), data = sleepstudy)
summary(M_7_4)
