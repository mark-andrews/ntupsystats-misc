library(tidyverse)
library(psyntur)

inst_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/instagram_df.csv")

count(inst_df, insta)

tukeyboxplot(y = age, x = insta, data = inst_df)

# change the insta to TRUE and FALSE
inst_df <- mutate(inst_df, insta = insta == 'yes')

# binary logistic regression
M_12_1 <- glm(insta ~ age, 
              data = inst_df,
              family = binomial())

summary(M_12_1)

b <- coef(M_12_1)

# what is log odds of being insta user if age = 25
b[1] + b[2] * 25

# what is log odds of being insta user if age = 50
b[1] + b[2] * 50

# what is probability of being insta user if age = 25
plogis(b[1] + b[2] * 25)
# plogis converts log odds to probabilities 

# what is probability of being insta user if age = 50
plogis(b[1] + b[2] * 50)


library(modelr) # this gives us add_predictions
insta_df2 <- tibble(age = c(25, 50))

add_predictions(insta_df2, M_12_1) # predicted log odds for those values

# to get probabilities, say type = 'response'
add_predictions(insta_df2, M_12_1, type = 'response')





