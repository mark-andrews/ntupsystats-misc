library(skimr)
library(tidyverse)

# Load up data and look at it ---------------------------------------------

data("attitude") # load up the data
# look at it
head(attitude, 10) # first 10 rows


# Descriptive stats -------------------------------------------------------

skim(attitude)


# Multiple linear regression ----------------------------------------------

M_11_1 <- lm(rating ~ complaints + privileges + learning, data = attitude)
summary(M_11_1)
confint(M_11_1) # 95% CI
confint(M_11_1, level = 0.99) # 99% CI



# Predictions -------------------------------------------------------------

new_data <- tibble(complaints = 70, privileges = 50, learning = 60)
predict(M_11_1, newdata = new_data)

new_data2 <- tibble(complaints = 50, privileges = 60, learning = 40)
predict(M_11_1, newdata = new_data2)


# Diagnostics -------------------------------------------------------------

plot(M_11_1, which = 2) # QQ plot
