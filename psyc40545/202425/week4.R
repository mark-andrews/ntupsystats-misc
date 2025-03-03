library(tidyverse)
library(modelr)

smoking_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/smoking.csv")

count(smoking_df, cigs) %>% print(n = Inf)

M_4_1 <- glm(cigs ~ age + educ + lincome, 
    data = smoking_df,
    family = poisson(link = 'log')
)
summary(M_4_1)

# What is the log of mean (num of cig) if 
# age = 25 and educ = 15 and lincome = 10

b <- coef(M_4_1)
log_mean <- b[1] + (b[2] * 25) + (b[3] * 15) + (b[4] * 10)

# What is the mean (num of cig) if 
# age = 25 and educ = 15 and lincome = 10
lambda <- exp(log_mean)

dpois(x = 5, lambda = lambda) # the prob of smoking exactly 5 cigs
dpois(x = 8, lambda = lambda) # the prob of smoking exactly 5 cigs
dpois(x = 10, lambda = lambda) # the prob of smoking exactly 5 cigs
dpois(x = 20, lambda = lambda) # the prob of smoking exactly 5 cigs

(exp(-lambda) * lambda ^ 8) / factorial(8)

smoking_df2 <- tibble(age = c(20, 30, 50), educ = 15, lincome = 10)

# predicted log of the mean
add_predictions(smoking_df2, M_4_1)

# predicted mean
add_predictions(smoking_df2, M_4_1, type = 'response')


# Interpretation of coefficients ------------------------------------------

b['age'] # change in the log of the mean for a unit change in age
exp(b['age']) # the factor by which the mean changes for a unit change in age


smoking_df3 <- tibble(age = seq(20, 30), educ = 15, lincome = 10)
add_predictions(smoking_df3, M_4_1, type = 'response')


# Model comparison --------------------------------------------------------

M_4_2 <- glm(cigs ~ age, 
             data = smoking_df,
             family = poisson(link = 'log')
)

deviance(M_4_1)
deviance(M_4_2)

deviance(M_4_2) - deviance(M_4_1)
anova(M_4_2, M_4_1, test = 'Chisq')


M_4_3 <- glm(cigs ~ age + educ + lincome + cigpric + restaurn + white, 
             data = smoking_df,
             family = poisson(link = 'log')
)
M_4_4 <- glm(cigs ~ age + educ + lincome + restaurn + white, 
             data = smoking_df,
             family = poisson(link = 'log')
)


deviance(M_4_3)
deviance(M_4_1)
anova(M_4_1, M_4_3, test = 'Chisq')


deviance(M_4_4)
anova(M_4_4, M_4_3, test = 'Chisq')

M_4_5 <- glm(cigs ~ age + educ + lincome + restaurn, 
             data = smoking_df,
             family = poisson(link = 'log')
)

deviance(M_4_5)
deviance(M_4_3)

anova(M_4_5, M_4_3, test = 'Chisq')
