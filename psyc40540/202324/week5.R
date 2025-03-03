library(tidyverse)
library(modelr)

options(scipen = 50)

# Get a data set ----------------------------------------------------------

happy_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/happy2019.csv")

# Simple linear regression predicting happiness from gdp
M_5_1 <- lm(happiness ~ gdp, data = happy_df)

# look at the results
summary(M_5_1)

# look at the intercept and slope terms
coef(M_5_1)

round(coef(M_5_1), 3) # to 3dp

# residual standard deviation
sigma(M_5_1)

round(summary(M_5_1)$coefficients, 5)


confint(M_5_1, level = 0.95) # 95% CI
confint(M_5_1, level = 0.99) # 99% CI

# Predictions -------------------------------------------------------------

# What is mean of normal distribution over happiness if GDP = 1.0?

intercept <- coef(M_5_1)[1]
slope <- coef(M_5_1)[2]

GDP <- 1.0

intercept + slope * GDP


# What is the mean of that normal distribution for GDP = -0.75 ?

GDP <- -0.75
intercept + slope * GDP

GDP_df <- tibble(gdp = c(1, 0.5, 0.25, -0.5, -0.75))

predict(M_5_1, newdata = GDP_df)

add_predictions(GDP_df, M_5_1)


# Multiple linear regression ----------------------------------------------

M_5_2 <- lm(happiness ~ gdp + support + hle + freedom + generosity + corrupt,
            data = happy_df)

summary(M_5_2)
sigma(M_5_2)

confint(M_5_2)

happy_df2 <- tibble(gdp = 1.0,
                    support = 0.5,
                    hle = 2.0,
                    freedom = -1.0,
                    generosity = 1.0,
                    corrupt = 0.75)
                
predict(M_5_2, newdata = happy_df2)
add_predictions(happy_df2, M_5_2)
