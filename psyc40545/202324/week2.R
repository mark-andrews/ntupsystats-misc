library(tidyverse)

smoking <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/smoking.csv")

# create a boolean variable indicating whether cigs > 0 
smoking <- mutate(smoking, is_smoker = cigs > 0)

# create a 0 or 1 variable indicating whether cigs > 0
smoking <- mutate(smoking, is_smoker2 = if_else(cigs > 0, 1, 0))

# do a logistic regression 
# modelling the probability of being a smoker
# as a function of age

M_2_1 <- glm(is_smoker ~ age, 
             data = smoking,
             family = binomial(link = 'logit')
)


summary(M_2_1)


# What is the log odds of being a smoker
# if the person's age is 25?
b <- coef(M_2_1)

log_odds_if_age_is_25 <- b[1] + b[2] * 25

# What is the probability of being a smoker
# if the person's age is 25?
prob_if_age_is_25 <- plogis(log_odds_if_age_is_25)


# What is the predicted prob of being a smoker
# if the person's age is between 10 and 50 in steps of 5

smoking2 <- tibble(age = seq(10, 50, by = 5))

predict(M_2_1, newdata = smoking2)

library(modelr)
# predicted log odds
add_predictions(smoking2, M_2_1)