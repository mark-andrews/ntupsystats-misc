library(tidyverse)

smoking_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/smoking.csv")

smoking_df <- mutate(smoking_df, is_smoker = cigs > 0)

# binary logistic regression
# lm(outcome ~ predictor)

M_2_1 <- glm(is_smoker ~ age, 
    data = smoking_df, 
    family = binomial(link = 'logit')
)

summary(M_2_1)

# What is the log odds of being a smoker if age = 25?
# What is the probability of being a smoker if age = 25?

b <- coef(M_2_1)

logodds_if_age_is_25 <- b[1] + b[2] * 25 # linear function of age = 25
prob_if_age_is_25 <- plogis(logodds_if_age_is_25) # using plogis to convert log odds to prob

# What is the log odds of being a smoker if age = 50?
# What is the probability of being a smoker if age = 50?

logodds_if_age_is_50 <- b[1] + b[2] * 50 # linear function of age = 50
prob_if_age_is_50 <- plogis(logodds_if_age_is_50)


# What is the log odds of being a smoker for a range of values of age from 15 to 80?
# What is the probability of being a smoker for a range of values of age from 15 to 80?

# create a data frame with the range of values of age
smoking_df2 <- tibble(age = seq(15, 80))

predict(M_2_1, newdata = smoking_df2)

library(modelr)
add_predictions(smoking_df2, M_2_1)

# and now for probabilities
predict(M_2_1, newdata = smoking_df2, type = 'response')
add_predictions(smoking_df2, M_2_1, type = 'response')

# if the probability of something is 0.75, what is the odds of that thing?
p <- 0.75
p/(1-p) # odds 

# if the odds of something happening is 4, what is the probability it will happen?
odds <- 4
odds/(1 + odds)

# if the probability of something is 0.75, what is the log odds of that thing?
p <- 0.75
odds <- p/(1-p)
log(odds) # log of the odds 
logit <- log(p/(1-p)) # same thing

# if log odds is 1.098612, what is the probability?
plogis(logit)
# same as 
1/(1 + exp(-logit)) # no need to do this because you have plogis 
