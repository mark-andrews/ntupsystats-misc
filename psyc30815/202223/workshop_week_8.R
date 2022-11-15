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
