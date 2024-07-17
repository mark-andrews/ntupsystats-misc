library(tidyverse)

smoking <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/smoking.csv")

smoking %>% count(cigs) %>% print(n = Inf)

M_4_1 <- glm(cigs ~ educ + age + lincome,
             data = smoking,
             family = poisson(link = 'log'))

summary(M_4_1)


# What is the predicted LOG of the mean number of cigs smoked
# if age = 50; educ = 20; lincome = 10?

b <- coef(M_4_1)
log_rate <- b[1] + b[2] * 20 + b[3] * 50 + b[4] * 10


# What is the predicted mean number of cigs smoked
# if age = 50; educ = 20; lincome = 10?
exp(log_rate)

# Acording to this model, what is the probability
# that a person with age = 50, educ = 20, lincome = 10
# smokes exactly 8 cigs per day?

dpois(x = 8, lambda = 6.203763)

# what about 2 cigs?
dpois(x = 2, lambda = 6.203763)


# Using the formula:
# e^(-lambda) * lambda ^ k / factorial(k)

k <- 8
lambda <- 6.203763

(exp(-lambda) * lambda ^ k) / factorial(k)


smoking_df2 <- tibble(educ = 20, age = 50, lincome = 10)

predict(M_4_1, newdata = smoking_df2) # predict log of the mean
predict(M_4_1, newdata = smoking_df2, type = 'response') # predicted mean

library(modelr)
add_predictions(smoking_df2, M_4_1)
add_predictions(smoking_df2, M_4_1, type = 'response')


b[3] # change in log mean as age increases by 1 year
exp(b[3]) # factor by which the mean changes for every extra year of age


M_4_2 <- glm(cigs ~ age + lincome,
             data = smoking,
             family = poisson(link = 'log'))

M_4_3 <- glm(cigs ~ lincome,
             data = smoking,
             family = poisson(link = 'log'))

M_4_0 <- glm(cigs ~ 1,
             data = smoking,
             family = poisson(link = 'log'))


deviance(M_4_0)
deviance(M_4_1)
deviance(M_4_2)
deviance(M_4_3)

anova(M_4_3, M_4_2, test = 'Chisq')
anova(M_4_0, M_4_3, test = 'Chisq')
