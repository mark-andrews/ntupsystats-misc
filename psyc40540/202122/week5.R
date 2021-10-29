library(tidyverse)

faithful_df <- read_csv("http://data.ntupsychology.net/faithfulfaces.csv")

# simple linear regression predicting attractiveness from trustworthiness
M <- lm(attractive ~ trustworthy, data = faithful_df)

# look at the coefficients
coef(M)

# what is the "sigma" or residual standard deviation
sigma(M)


# What is the average value of attractiveness if trustworthiness is equal to 1
# it is: intercept + slope x (trustworthiness = 1)
# intercept + slope x 1
1.26 + 0.4 * 1

# to be more precise
estimates <- coef(M)
estimates[1] + estimates[2] * 1


# What is the average value of attractiveness if trustworthiness is equal to 5?
estimates[1] + estimates[2] * 5


# multiple linear regression predicting attractiveness from trustworthiness and faithfulness
M1 <- lm(attractive ~ trustworthy + faithful, data = faithful_df)

# what are the coefficients
coef(M1)
sigma(M1)

# What is the average attractiveness if trustworthy is 2, and faithfulness is 3
estimates <- coef(M1)
estimates[1] + estimates[2] * 2 + estimates[3] * 3

# What is the average attractiveness if trustworthy is 7.5, and faithfulness is 2.5?
estimates[1] + estimates[2] * 7.5 + estimates[3] * 2.5

new_df <- tibble(trustworthy = 7.5, faithful = 2.5)
new_df <- data.frame(trustworthy = 7.5, faithful = 2.5)

predict(M1, newdata = new_df)
