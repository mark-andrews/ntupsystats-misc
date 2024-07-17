library(tidyverse)

affairs_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/affairs.csv")

count(affairs_df, affairs)

summarise(affairs_df, mean(affairs))

x <- rpois(n = 1000, lambda = 1.46) # sampling 100 values for a Poisson dist with lambda = 1.46

M_6_1 <- glm(affairs ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating,
             data = affairs_df,
             family = poisson(link= 'log'))

M_6_1a <- glm(affairs ~ .,
             data = affairs_df,
             family = poisson(link= 'log'))

library(pscl)
M_6_2 <- zeroinfl(affairs ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating,
                  data = affairs_df)
summary(M_6_2)


# What is the probability that a
# * male
# * aged 25
# * five years married
# * with no children
# * religiousness = 3
# * education = 16
# * occupation = 4
# * rating = 4
# that they are a "cheater AKA not faithful"?

b <- coef(M_6_2, model = 'zero')

phi <- 
  b[1] + b['gendermale'] * 1 + b['age'] * 25 + b['yearsmarried'] * 5 +
  b['childrenyes'] * 0 + b['religiousness'] * 3 + 
  b['education'] * 16 + b['occupation'] * 4 + b['rating'] * 4

plogis(phi) # prob of being faithful
1 - plogis(phi)


affairs_df2 <- tibble(gender = 'male', age = 25, yearsmarried = 5,
                      children = 'no', religiousness = 3,
                      education = 16, occupation = 4,
                      rating = 4)

1 - predict(M_6_2, newdata = affairs_df2, type = 'zero')


# What is the avg number of affairs of a 
# * male
# * aged 25
# * five years married
# * with no children
# * religiousness = 3
# * education = 16
# * occupation = 4
# * rating = 4
# if they are a not faithful type...

bc <- coef(M_6_2, model = 'count')

# log of the mean number of affairs
psi <- 
  bc[1] + bc['gendermale'] * 1 + bc['age'] * 25 + bc['yearsmarried'] * 5 +
  bc['childrenyes'] * 0 + bc['religiousness'] * 3 + 
  bc['education'] * 16 + bc['occupation'] * 4 + bc['rating'] * 4

# mean number of affairs
exp(psi)

predict(M_6_2, newdata = affairs_df2, type = 'count')


# What is the avg number of affairs of a 
# * male
# * aged 25
# * five years married
# * with no children
# * religiousness = 3
# * education = 16
# * occupation = 4
# * rating = 4

predict(M_6_2, newdata = affairs_df2, type = 'response')


vuong(M_6_1, M_6_2)

# zero infl neg bin
M_6_3 <- zeroinfl(affairs ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating,
                  dist = "negbin",
                  data = affairs_df)
