library(tidyverse)
library(modelr)
library(pscl) # for zero inflated model


affairs_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/affairs.csv")

summarise(affairs_df, mean(affairs)) # average number of affairs
x <- rpois(n = 1e4, lambda = 1.46)
mean(x == 0) # proportion of zeros in a Poisson dist with lambda = 1.46


summarise(affairs_df, mean(affairs == 0))

M_6_1 <- zeroinfl(
  affairs ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating,
  dist = 'poisson',
  data = affairs_df)

summary(M_6_1)

# gender = male
# age = 25
# yearsmarried = 5
# children = no
# religiousness = 3
# education = 16
# occupation = 4
# rating = 4

# Probability of being in the "zero model" group
b <- coef(M_6_1, model = 'zero')
log_odds <- b[1] + b['gendermale'] * 1 + b['age'] * 25 +
  b['yearsmarried'] * 5 +
  b['childrenyes'] * 0 + b['religiousness'] * 3 +
  b['education'] * 16 + b['occupation'] * 4 +
  b['rating'] * 4

plogis(log_odds) # probability of not having an affair 
1 - plogis(log_odds)


b1 <- coef(M_6_1, model = 'count')
log_rate <- b1[1] + b1['gendermale'] * 1 + b1['age'] * 25 +
  b1['yearsmarried'] * 5 +
  b1['childrenyes'] * 0 + b1['religiousness'] * 3 +
  b1['education'] * 16 + b1['occupation'] * 4 +
  b1['rating'] * 4

rate <- exp(log_rate) # average number of affairs by the above individual

new_df <- tibble(
  gender = 'male',
  age = 25,
  yearsmarried = 5, children = 'no', religiousness = 3,
  education = 16, occupation = 4, rating = 4
)

# the probability of that person being in the "zero model" group
predict(M_6_1, newdata = new_df, type = 'zero') 

# the average number of affairs of that person....assuming they are in the count group
avg_affair <- predict(M_6_1, newdata = new_df, type = 'count') 

# Probability of being in the "count model" group
p <- (1 - predict(M_6_1, newdata = new_df, type = 'zero')) 

p * avg_affair 

# or else do ....
predict(M_6_1, newdata = new_df, type = 'response') 
