library(tidyverse)
library(afex)

# Look at a data set ------------------------------------------------------

head(laptop_urry, 10)
# select just `condition` and `overall`
head(select(laptop_urry, condition, overall), 10)

# t-test looking at effect of `condition` on `overall`
M_7_1 <- t.test(overall ~ condition, data = laptop_urry, var.equal = TRUE)
M_7_1


# do this analysis using a general linear model
M_7_2 <- lm(overall ~ condition, data = laptop_urry)

# look at the coefficients
summary(M_7_2)$coefficients

# just focus on coefficients
coef(M_7_2)
b <- coef(M_7_2)

# mean for "laptop" condition
b[1] + b[2] * 0

# mean for "longhand" condition
b[1] + b[2] * 1


# general linear model with outcome `overall` and predictor `talk` --------

M_7_3 <- lm(overall ~ talk, data = laptop_urry)
summary(M_7_3)$coefficients

# see all values of `talk`, sorted
sort(unique(laptop_urry$talk))


# Mean for each "talk" ------------------------------------------------------

b <- coef(M_7_3)

# "algorithm" talk mean for `overall`
b[1] + (b[2] * 0) + (b[3] * 0) + (b[4] * 0) + (b[5] * 0)

# "ideas" talk mean for `overall`
b[1] + (b[2] * 1) + (b[3] * 0) + (b[4] * 0) + (b[5] * 0)

# "islam" talk mean for `overall`
b[1] + (b[2] * 0) + (b[3] * 0) + (b[4] * 0) + (b[5] * 1)

# "inequalities" talk mean for `overall`
b[1] + (b[2] * 0) + (b[3] * 0) + (b[4] * 1) + (b[5] * 0)
