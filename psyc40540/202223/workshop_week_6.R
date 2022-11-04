library(tidyverse)

# Read in a data file -----------------------------------------------------

happy_df <- read_csv("http://data.ntupsychology.net/happy2019.csv")


# Linear model predicting happiness from everything -----------------------

M_6_1 <- lm(happiness ~ gdp + support + hle + freedom + generosity + corrupt,
            data= happy_df)

# R^2 for model
summary(M_6_1)$r.sq

# Adjusted R^2 
summary(M_6_1)$adj.r.sq



# Look at coefficients ----------------------------------------------------

round(summary(M_6_1)$coefficients, 3)


# Confidence intervals ----------------------------------------------------

confint(M_6_1) # 95% CI for all variables
# just look at 95% CI for gdp
confint(M_6_1, parm = 'gdp')

# 99% CI
confint(M_6_1, parm = 'gdp', level = 0.99)



# Model null hypothesis ---------------------------------------------------

# see bottom of summary
summary(M_6_1)


# Predictions with linear models
# What is the predicted avg happiness if
# gdp = 1.5, support = 0.25, hle = 0.5
# freedom = -1.0, generosity = 0.35, corrupt = 1.8
# ?

# first set up a data frame with variables with the specified values
new_happy <- tibble(gdp = 1.5,
                    support = 0.25,
                    hle = 0.5,
                    freedom = -1.0,
                    generosity = 0.35,
                    corrupt = 1.8)

# use the `predict`
predict(M_6_1, newdata = new_happy)
# or use `add_predictions` from `modelr`
library(modelr)
add_predictions(new_happy, M_6_1)

# get the prediction confidence interval 
predict(M_6_1, newdata = new_happy, interval = 'confidence')

# get 99% prediction confidence interval
predict(M_6_1, newdata = new_happy, interval = 'confidence', level = 0.99)



# Using psymetr_df_total --------------------------------------------------

psymetr_df <- read_csv("http://data.ntupsychology.net/psymetr_df_total.csv")
M_6_2 <- lm(stress ~ ., data = psymetr_df)
S <- summary(M_6_2)
S$r.sq
S$adj.r.sq
S$fstatistic

#predicted mean and 95% conf int for prediction if
# anxiety is 2, depression is 1, efficacy is -1, sociability is 0 ?
new_df <- tibble(anxiety = 2,
                 depression = 1,
                 efficacy = -1, 
                 sociability = 0)
predict(M_6_2, newdata = new_df, interval = 'confidence')
