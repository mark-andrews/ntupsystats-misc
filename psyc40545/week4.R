library(tidyverse)
affairs_df <- read_csv("http://data.ntupsychology.net/affairs.csv")

# Model probability of affairs as function of yearsmarried
M <- glm(affairs ~ yearsmarried,
         data = affairs_df,
         family = poisson(link = 'log'))

summary(M)

# what is log of lambda if yearsmarried is 10?
coefs <- coef(M)

log_rate_10 <- coefs[1] + coefs[2] * 10

# what is lambda if yearsmarried is 10?
exp(log_rate_10)

# predictions about log rate and rate for a range of values of yearsmarried
affairs_df2 <- tibble(yearsmarried = seq(1, 25))

library(modelr)

add_predictions(affairs_df2, M) # the predicted *log* of the rates
add_predictions(affairs_df2, M, type = 'response') # the predicted rates

print(
  add_predictions(affairs_df2, M, type = 'response'), 
  n = 25)

# meaning of the coefficients
exp(coefs[2])

M1 <- glm(affairs ~ yearsmarried + age + gender,
         data = affairs_df,
         family = poisson(link = 'log'))

deviance(M1)
deviance(M)
deviance(M) - deviance(M1)

anova(M, M1, test = 'Chisq')
