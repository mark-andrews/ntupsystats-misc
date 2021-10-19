
# lload packages ----------------------------------------------------------

library(psyntur)
library(tidyverse)

# just for fun, try this
weight_df <- read_csv("http://data.ntupsychology.net/weight.csv")



# Linear regression predicting weight from height and age -----------------

M <- lm(weight ~ height + age, data = ansur)

# look at the estimates
coef(M)
sigma(M)


# inferential stats -------------------------------------------------------

summary(M)

# just look at coefficients table
summary(M)$coefficients


confint(M)
# 99% confidence interval
confint(M, level = 0.99)


# predictions -------------------------------------------------------------

new_df <- tibble(height = 180, age = 25)

predict(M, newdata = new_df)
