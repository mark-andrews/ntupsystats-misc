# Load packages -----------------------------------------------------------
library(tidyverse)

# Load data ---------------------------------------------------------------

weight_df <- read_csv("http://data.ntupsychology.net/weight.csv")

# Multiple regression analysis --------------------------------------------

M <- lm(weight ~ height + age, data = weight_df)

# overall model summary
summary(M)

# extract out R^2
summary(M)$r.squared

# extract out adjusted R^2
summary(M)$adj.r.squared

# extract F statistic 
summary(M)$fstatistic

# confidence intervals ----------------------------------------------------

confint(M) # 95% CI
confint(M, level = 0.99) # 99% CI


# t statistics for the null hypothesis tests ------------------------------

summary(M)$coefficients


# Predicted means of the outcome variable ---------------------------------

weight_df_2 <- tibble(height = c(180, 160, 170),
                      age = c(25, 35, 48))

estimates <- coef(M)
# predicted mean of the distribution over weight
# for a person whose height is 180 and age is 25
estimates[1] + estimates[2] * 180 + estimates[3] * 25

# use the `predict` function
predict(M, newdata = weight_df_2)

# or, to format the predictions as a new column
library(modelr)
add_predictions(weight_df_2, M)

# use the `predict` function to the CI on the predicted means
predict(M, newdata = weight_df_2, interval = 'confidence')

predict(M, newdata = weight_df_2, 
        interval = 'confidence', level = 0.99)
