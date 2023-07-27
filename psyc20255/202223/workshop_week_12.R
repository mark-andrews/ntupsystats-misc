library(tidyverse)
library(psyntur)

instagram_df <- read_csv("http://data.ntupsychology.net/instagram_df.csv")
# see all the rows
print(instagram_df, n = Inf)

# code the `insta` variable to tell it that there are only two possible values
# in general
instagram_df$insta <- factor(
  instagram_df$insta, 
  levels = c("no", "yes")
)

# do a scatterplot 
scatterplot(x = age, y = insta, 
            data = instagram_df)

# boxplot 
tukeyboxplot(y = age, x = insta,
             data = instagram_df)

# binary logistic regression

# this is a simple *linear* regression
# lm(insta ~ age, data = instagram_df)

# Two important differences to `lm`:
# 1) use `glm` not `lm`
# 2) use `family = binomial`


M_12_1 <- glm(insta ~ age, data = instagram_df,
              family = binomial())

# just focus on the coefficients
summary(M_12_1)$coefficients

# predicted value of the outcome variable if 
# age = 25 or 50

# create a new data frame
new_df = tibble(age = c(25, 50))

# use the `predict` function
predict(M_12_1, newdata = new_df)

# using `modelr`
library(modelr)
add_predictions(new_df, M_12_1)

# convert the "log odds" predictions to probabilities
predict(M_12_1, newdata = new_df, type = 'response')
add_predictions(new_df, M_12_1, type = 'response')

# what are the probabilities for 35 or 45 or 55 year olds?
# create a new data frame
new_df = tibble(age = c(35, 45, 55))

predict(M_12_1, newdata = new_df, type = 'response')
add_predictions(new_df, M_12_1, type = 'response')

# What is the predicted log odds (of insta = 'yes') if 
# a person is 25?
new_df = tibble(age = 25)

predict(M_12_1, newdata = new_df)

# What is the predicted probability (of insta = 'yes') if 
# a person is 25?
predict(M_12_1, newdata = new_df, type = 'response')


