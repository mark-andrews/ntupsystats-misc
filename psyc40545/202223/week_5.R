library(tidyverse)
library(MASS) # for the nb regression

insur_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/Insurance.csv")

M_5_1 <- glm.nb(Claims ~ Age, data = insur_df)
# M_5_1 <- glm.nb(Claims ~ 1 + Age, data = insur_df)

#  how many age groups ?
table(insur_df$Age)

# what is the predicted log of the mean if Age group is <25
betas <- coef(M_5_1)

# log of mean if Age group is <25 is just intercept
betas[1]
betas['(Intercept)']

# log of mean if Age group is 25-29 is intercept plus coef for 25-29
betas['(Intercept)'] + betas['Age25-29']

# log of mean if Age group is 30-35 is intercept plus coef for 30-35
betas['(Intercept)'] + betas['Age30-35']

# log of mean if Age group is >35 is intercept plus coef for >35 
betas['(Intercept)'] + betas['Age>35']


# mean if Age group is <25 is just intercept
exp(betas['(Intercept)'])

# log of mean if Age group is 25-29 is intercept plus coef for 25-29
exp(betas['(Intercept)'] + betas['Age25-29'])

# log of mean if Age group is 30-35 is intercept plus coef for 30-35
exp(betas['(Intercept)'] + betas['Age30-35'])

# log of mean if Age group is >35 is intercept plus coef for >35 
exp(betas['(Intercept)'] + betas['Age>35'])


# prediction with the predict function, or add_predictions
new_df <- tibble(Age = c("<25", "25-29", "30-35", ">35"))
predict(M_5_1, newdata = new_df) # predicted log of mean
exp(predict(M_5_1, newdata = new_df)) # predicted mean ... if you prefer
predict(M_5_1, newdata = new_df, type = 'response') # predicted mean

# interpret coefs
summary(M_5_1)$coefficients

# the factor by which the mean changes as we go from <25 to >35 group
exp(2.1991635)

# confidence interval on the facor by which the mean changes ...
exp(confint.default(M_5_1, parm = 'Age>35'))


# Model comparison
# test the null hypothesis the Age does not predict number of claims

# first set up the model
M_5_2 <- glm.nb(Claims ~ 1, data = insur_df)

# get the log of the likelihood of the two models
logLik(M_5_1)
logLik(M_5_2)

# get the deviances of the two models

# DO NOT DO THIS EVER!!!!!! ######
deviance(M_5_1)
M_5_1$deviance

# Do this
D_5_1 <- -2 * logLik(M_5_1)
D_5_2 <- -2 * logLik(M_5_2)

D_5_2 - D_5_1

anova(M_5_2, M_5_1)
