# What is the probability that the variable
# takes the value of 2, if the mean is 5
# and the dispersion parameter is 3.5?
dnbinom(x = 2, mu = 5, size = 3.5)

# What is the probability that the variable
# takes the value of 3, if the mean is 2.5
# and the dispersion parameter is 1.5?
dnbinom(x = 3, mu = 2.5, size = 1.5)

library(tidyverse)
library(MASS)

affairs_df <- read_csv("http://data.ntupsychology.net/affairs.csv")
M <- glm.nb(affairs ~ age, data = affairs_df)
summary(M)

coefs <- coef(M)

# predicted log mean of the number of affairs, if age = 35
coefs[1] + coefs[2] * 35

# predicted mean of the number of affairs, if age = 35
exp(coefs[1] + coefs[2] * 35)

new_df <- tibble(age = c(25, 30, 35))

predict(M, newdata = new_df) # predicted log mean
predict(M, newdata = new_df, type= 'response') # predict mean

library(modelr)
add_predictions(new_df, M) # predicted log mean
add_predictions(new_df, M, type = 'response') # predicted log mean

M0 <- glm.nb(affairs ~ 1, data = affairs_df)

anova(M0, M)

