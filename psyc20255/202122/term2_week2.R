library(tidyverse)
library(psyntur)

# Get the data
insta_df <- read_csv("http://data.ntupsychology.net/instagram_df.csv")

# Frequency of different values of `insta`
table(insta_df$insta)

# boxplot of the data
tukeyboxplot(y = age, x = insta, 
             data = insta_df,
             jitter = TRUE)

# Do a regression; binary logistic regression

# option 1
M <- glm(I(insta == 'yes') ~ age,
         data = insta_df,
         family = binomial())

# option 2: create a logical variable in the data
insta_df <- mutate(insta_df, insta_user = insta == 'yes')

M1 <- glm(insta_user ~ age,
         data = insta_df,
         family = binomial())

# option 3: create a factor variable in the data
insta_df <- mutate(insta_df, insta_user_f = factor(insta))

M2 <- glm(insta_user_f ~ age,
          data = insta_df,
          family = binomial())


# Look at the results
summary(M)


# Look at predictions
library(modelr)

new_ages <- tibble(age = seq(10, 80, by = 10))
add_predictions(new_ages, M) # predicted log odds 
add_predictions(new_ages, M, type = 'response')

