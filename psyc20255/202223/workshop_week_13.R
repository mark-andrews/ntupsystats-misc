library(tidyverse)
library(psyntur)

fomo_df <- read_csv("http://data.ntupsychology.net/fomo.csv")


# Visualize data ----------------------------------------------------------

histogram(x = check, data = fomo_df)
histogram(x = fomo, data = fomo_df)
scatterplot(x = fomo, y = check, 
            best_fit_line = TRUE,
            data = fomo_df)


# Poisson regression ------------------------------------------------------

# lm(y ~ x) # normal linear model

M_13_1 <- glm(check ~ fomo, 
              data = fomo_df, 
              family = poisson())

summary(M_13_1)


# What is the logarithm of the average number of checks
# if fomo = 50
3.34667 + 0.01361 * 50

# What is the average number of checks if fomo = 50
exp(3.34667 + 0.01361 * 50)


# What is the logarithm of the average number of checks
# if fomo = 50
new_df <- tibble(fomo=50)

predict(M_13_1, newdata = new_df)
add_predictions(new_df, M_13_1)

# What is the average number of checks if fomo = 50
exp(predict(M_13_1, newdata = new_df))
# better way
predict(M_13_1, newdata = new_df, type = 'response')

add_predictions(new_df, M_13_1, type = 'response')
