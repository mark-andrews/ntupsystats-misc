library(lme4)
library(tidyverse)

ggplot(sleepstudy,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~Subject)

# simple linear regression; effect of sleep deprivation on RT
M_8_1 <- lm(Reaction ~ Days, data = sleepstudy)

# plot the simple linear model
ggplot(sleepstudy,
       aes(x = Days, y = Reaction)
) + geom_point() + stat_smooth(method = 'lm', se = FALSE)

summary(M_8_1)

# prediction according to M_8_1
sleepstudy_1 <- tibble(Days = c(0, 4, 9))

library(modelr) # to load add_predictions
add_predictions(sleepstudy_1, M_8_1)


# multilevel linear model
M_8_2 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
