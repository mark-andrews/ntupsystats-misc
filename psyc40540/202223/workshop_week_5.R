# linear regression models

# load packages -----------------------------------------------------------
library(tidyverse)


# turn off scientific notation
options(scipen = 50)

# load up some data
happy_df <- read_csv("http://data.ntupsychology.net/happy2019.csv")

# regression analysis predicting happiness from gdp
M_5_1 <- lm(happiness ~ gdp, data = happy_df)

# look at coefficients
coef(M_5_1)
# residual standard deviation
sigma(M_5_1)

# more info with `summary`
summary(M_5_1)

# confidence intervals
confint(M_5_1)

# What is the predicted average happiness if gdp = 2.0?

# method 1: the manual way
intercept <- coef(M_5_1)[1]
slope <- coef(M_5_1)[2]

# do the linear equation
intercept + slope * 2.0 # when gdp is 2.0
intercept + slope * -1.0 # when gdp is -1.0

# method 2: using `predict`
# first, we set up a data frame
new_df <- tibble(gdp = c(-2, -1, 0, 1, 2))
predict(M_5_1, newdata = new_df)

# do the predictions using `add_predictions` from `modelr`
library(modelr)
add_predictions(new_df, M_5_1)



# Multiple linear regression ----------------------------------------------

# predict happiness from `gdp` and `hle` and `freedom` and `support`
M_5_2 <- lm(happiness ~ gdp + hle + freedom + support, data = happy_df)
# look at coefficients
coef(M_5_2)
# residual sd
sigma(M_5_2)

# we see all of that and more with `summary`
summary(M_5_2)

confint(M_5_2)

# what is the predict avg happiness if gdp = 2.0 
# AND all other variables have the average value
# and that means zero in this data

# method 1: the manual way
intercept <- coef(M_5_2)[1]
slope_gdp <- coef(M_5_2)[2]
slope_hle <- coef(M_5_2)[3]
slope_fre <- coef(M_5_2)[4]
slope_sup <- coef(M_5_2)[5]

intercept + slope_gdp * 2.0 + slope_hle * 0 + slope_fre * 0 + slope_sup * 0

# method 2: using `predict`
new_df <- tibble(gdp = 2.0,
                 hle = 0,
                 support = 0,
                 freedom = 0)

new_df_1 <- tibble(gdp = 2.0,
                   hle = 1,
                   support = 2,
                   freedom = 3)

predict(M_5_2, newdata = new_df)
# using `add_predictions`
add_predictions(new_df, M_5_2)

# using new_df_1
add_predictions(new_df_1, M_5_2)
