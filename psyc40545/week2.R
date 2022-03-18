library(tidyverse)
smoking_df <- read_csv("http://data.ntupsychology.net/smoking.csv")

smoking_df <- mutate(smoking_df, smoker = cigs > 0)
summarise(smoking_df, proportion_smoker = mean(smoker))

# model the probability of being a smoker as a function of `educ`
M <- glm(smoker ~ educ, 
         data = smoking_df, 
         family = binomial(link = 'logit'))

summary(M)

coefs <- coef(M)

# what is log odds of smoking, if educ = 5?
coefs[1] + coefs[2] * 5

# what is log odds of smoking, if educ = 10?
coefs[1] + coefs[2] * 10

# Now, what about the probabilities??? 

# what is log odds of smoking, if educ = 15?
coefs[1] + coefs[2] * 15

# what is probability of smoking, if educ = 5?
plogis(coefs[1] + coefs[2] * 5)

# what is probability of smoking, if educ = 10?
plogis(coefs[1] + coefs[2] * 10)

# what is probability of smoking, if educ = 15?
plogis(coefs[1] + coefs[2] * 15)


# Using `predict` or `add_predictions`
library(modelr)

smoking_df_new <- tibble(educ = seq(1, 20))

# predicted log odds 
predict(M, newdata = smoking_df_new)

# predicted probabilities
predict(M, newdata = smoking_df_new, type = 'response')

# using `add_predictions`, predicted log odds
add_predictions(smoking_df_new, M)

# using `add_predictions`, predicted probability
results <- add_predictions(smoking_df_new, 
                           M, 
                           type = 'response')

ggplot(results, aes(x = educ, y = pred)) + geom_line()

