library(tidyverse)

smoking_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/smoking.csv")

group_by(smoking_df, cigs) %>%
  tally() %>% 
  print(n = Inf)

print(tally(group_by(smoking_df, cigs)), n = Inf)

# sqrt of log of 100
log(sqrt(log(100)))

# also log of 100
100 %>% log() %>% sqrt() %>% log()

# model a Poisson distribution over `cigs` as 
# function of `educ` and `age` `lincome`
M_4_1 <- glm(cigs ~ educ + age + lincome,
             data = smoking_df,
             family = poisson()
)
summary(M_4_1)

# What is the probability that y = k if lambda = 2.75?
lambda <- 2.75
k <- 3
# manual way
exp(-lambda) * lambda ^ k / factorial(k)
# the direct way
dpois(x = k, lambda)


# Coefficients ------------------------------------------------------------

summary(M_4_1)$coefficients

betas <- coef(M_4_1)[2:4]
exp(betas) # e to the power of the coefficients

confint.default(M_4_1)

# 95% CI on the factor by which the mean/rate changes
exp(confint.default(M_4_1))


# Prediction --------------------------------------------------------------

# What is the predicted log of the mean/rate/lambda if
# age = 25, educ = 15, lincome = 10?

betas <- coef(M_4_1)
betas[1] + betas['educ'] * 15 + betas['age'] * 25 + betas['lincome'] * 10

# What is the predicted mean/rate/lambda if
# age = 25, educ = 15, lincome = 10?
exp(betas[1] + betas['educ'] * 15 + betas['age'] * 25 + betas['lincome'] * 10)


# Using predict -----------------------------------------------------------

smoking_new <- tibble(age = 25, educ = 15, lincome = 10)
predict(M_4_1, newdata = smoking_new)
# predicted mean
exp(predict(M_4_1, newdata = smoking_new))
predict(M_4_1, newdata = smoking_new, type = 'response')


# Model comparison --------------------------------------------------------

# model a Poisson distribution over `cigs` as 
# function of just `age` 
M_4_2 <- glm(cigs ~ age,
             data = smoking_df,
             family = poisson()
)


deviance(M_4_1) # three predictors: age, educ, lincome
deviance(M_4_2) # one predictor: age

anova(M_4_2, M_4_1, test = 'Chisq') # log likelihood ratio test
