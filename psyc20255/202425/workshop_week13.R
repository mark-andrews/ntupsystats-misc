library(tidyverse)
library(psyntur)

fomo_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/fomo.csv")

scatterplot(x= fomo, y = check, data = fomo_df)

# Poisson regression
M_13_1 <- glm(check ~ fomo, 
              data = fomo_df,
              family = poisson())

summary(M_13_1)
b <- coef(M_13_1)

log_mean <- b[1] + b[2] * 25 # log of the mean if fomo=25; linear function of fomo=25
exp(log_mean) # converts log of the mean to the mean

# if fomo = 75, what is the log of the mean number of times they check 
b[1] + b[2] * 75

# if fomo = 50, what is the log of the mean number of times they check 
b[1] + b[2] * 50

# if fomo = 75, what is the mean number of times they check 
exp(b[1] + b[2] * 75)

# if fomo = 50, what is the mean number of times they check 
exp(b[1] + b[2] * 50)

# do those predictions using add_predictions
library(modelr)

# the values of fomo we want to make predictions about ....
new_df <- tibble(fomo = c(25, 50, 75))

add_predictions(new_df, M_13_1) # log of means
add_predictions(new_df, M_13_1, type = 'response') # means


