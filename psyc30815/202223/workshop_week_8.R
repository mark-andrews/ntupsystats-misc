library(tidyverse)
library(lme4)
theme_set(theme_classic())

ggplot(sleepstudy,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~Subject)


# Single simple linear regression
# pooled analysis
ggplot(sleepstudy,
       aes(x = Days, y = Reaction)
) + geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) 

# simple linear regression: pooled model
M_8_1 <- lm(Reaction ~ Days, data = sleepstudy)

summary(M_8_1)
confint(M_8_1)

# graphical diagnostics
plot(M_8_1, which = 2) # QQ plot
plot(M_8_1, which = 1)
plot(M_8_1, which = 3)

# histogram of residuals
hist(residuals(M_8_1))


# plot residuals by subject
sleepstudy_2 <- mutate(sleepstudy, 
                       residuals = residuals(M_8_1)
)

ggplot(sleepstudy_2,
       aes(x = Days, y = residuals, colour = Subject)
) + geom_line() + geom_point()


ggplot(sleepstudy,
       aes(x = Days, y = Reaction)
) + geom_point() + 
  geom_abline(intercept = coef(M_8_1)[1],
              slope = coef(M_8_1)[2]) +
  facet_wrap(~Subject)



# No pooling model analysis -----------------------------------------------

M_8_2 <- lm(Reaction ~ Days * Subject, data = sleepstudy)
M_8_2b <- lm(Reaction ~ Days + Subject + Days:Subject, data = sleepstudy)
M_8_2c <- lm(Reaction ~ 0 + Subject + Days:Subject, data = sleepstudy)

library(modelr)
add_predictions(sleepstudy, M_8_2c) %>% 
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_line(aes(y = pred)) +
  facet_wrap(~Subject)

summary(M_8_2)$r.sq # look at R^2
# compare to the pooled model
summary(M_8_1)$r.sq



# Multilevel model --------------------------------------------------------

library(lme4)
M_8_3 <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)
summary(M_8_3)
