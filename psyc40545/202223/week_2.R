library(tidyverse)

smoking_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/smoking.csv")

table(smoking_df$cigs)

# create new variable called `smoker`
# this takes value TRUE if cigs > 0 and FALSE otherwise
smoking_df <- mutate(smoking_df, smoker = cigs > 0)

# binary logistic regression
# predicting the probability the person is a smoker
# as a function of education (`educ`)

# don't do this ...
# lm(y ~ x) # normal linear model

# do this
M_1_1 <- glm(smoker ~ educ, 
             data = smoking_df,
             family = binomial(link = 'logit'))

summary(M_1_1)


betas <- coef(M_1_1)
# what is the log odds of being of smoker 
# if educ = 10 ?
betas[1] + betas[2] * 10

# what is the probability being of smoker if educ = 10 ?
plogis(betas[1] + betas[2] * 10)

# what is the probability being of smoker if educ = 20 ?
plogis(betas[1] + betas[2] * 20)


# do these predictions using `predict` (and friends)
# e.g. predictions if educ = 5 or 10 or 20 or 25
new_df <- tibble(educ = c(5, 10, 20, 25))

# predicted log odds ....
predict(M_1_1, newdata = new_df)

library(modelr)
add_predictions(new_df, M_1_1)

# for predicted probabilities, no need to plogis
# just to `type = 'response'`

# predicted probability ...
predict(M_1_1, newdata = new_df, type = 'response')

add_predictions(new_df, M_1_1, type = 'response')
