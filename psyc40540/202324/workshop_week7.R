library(tidyverse)
library(psyntur)

# GLM with `trustworthy` as outcome var, and `cheater` as predictor
M_7_1 <- lm(trustworthy ~ cheater, data = faithfulfaces)
M_7_2 <- lm(trustworthy ~ attractive, data = faithfulfaces)


summary(M_7_1)
coef(M_7_1)

# (Using M_7_1) What is the predicted mean of trustworthy if cheater is FALSE?

# intercept + coef_cheater x dummy_code_for_FALSE

b <- coef(M_7_1)

pred_mean_cheater_false <- b[1] + b[2] * 0

# (Using M_7_1) What is the predicted mean of trustworthy if cheater is TRUE?

# intercept + coef_cheater x dummy_code_for_TRUE

pred_mean_cheater_true <- b[1] + b[2] * 1

# do it the `predict` or `add_predictions` way
new_df <- tibble(cheater = c(TRUE, FALSE))

library(modelr)
add_predictions(new_df, M_7_1)


# Non binary categorical variables ----------------------------------------

library(afex)

# GLM with `overall` as the outcome variable and `talk` is the predictor
M_7_3 <- lm(overall ~ talk, data = laptop_urry)
summary(M_7_3)

coef(M_7_3)

# what is predicted mean of `overall` if `talk` = 'algorithms'
b <- coef(M_7_3)
b[1] + (b[2] * 0) + (b[3] * 0) + (b[4] * 0) + (b[5] * 0)

# what is predicted mean of `overall` if `talk` = 'islam'
b[1] + (b[2] * 0) + (b[3] * 0) + (b[4] * 0) + (b[5] * 1)


# what is predicted mean of `overall` if `talk` = 'indus'
b[1] + (b[2] * 0) + (b[3] * 1) + (b[4] * 0) + (b[5] * 0)

new_df <- tibble(talk = c('algorithms','indus','ideas', 'islam', 'inequalities'))
add_predictions(new_df, M_7_3)


toothgrowth <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/toothgrowth.csv")

# glm: outcome `len`, predictor `supp`
M_7_4 <- lm(len ~ supp, data= toothgrowth)

new_df <- tibble(supp = c('OJ', 'VC'))

add_predictions(new_df, M_7_4)

options(pillar.sigfig = 8) # if you want more decimal places in your tibble


# glm: outcome `len`, predictor `dose`
M_7_5 <- lm(len ~ dose, data= toothgrowth)

# predicted mean of `len` when `dose` is `high`, `low`, `medium`
new_df <- tibble(dose = c('high', 'low', 'medium'))
add_predictions(new_df, M_7_5)
