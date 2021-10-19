library(tidyverse)
library(psyntur)

M <- lm(weight ~ height + age, data = ansur)

# get the residuals
residuals(M)

# make a quick and dirty histogram
hist(residuals(M))

plot(M, which = 2)

plot(M, which = 1)

plot(M, which = 3)

# RSS, residual sums of squares
sum(residuals(M)^2)

summary(M)$r.squared
