library(tidyverse)

affairs_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/affairs.csv")

# frequency distribution of number of affairs
table(affairs_df$affairs)


# zero inflated Poisson model
# modelling the number of affairs as a function of age
library(pscl)
M_6_zip <- zeroinfl(affairs ~ age, data = affairs_df)

summary(M_6_zip)

# what is the probability that a person aged 35 has affairs?
betas <- coef(M_6_zip)

betas[3] + betas[4] * 35 # log odds of not having an affair if aged 35
plogis(betas[3] + betas[4] * 35) # probability of not having an affair if aged 35
1 - plogis(betas[3] + betas[4] * 35) # probability of having an affair if aged 35

# do that predict with predict
new_df <- tibble(age= 35)
prob_of_zero <- predict(M_6_zip, newdata = new_df, type = 'zero')


# What the average number of affairs for a 35 year old who does have affairs?

betas[1] + betas[2] * 35 # log of the mean number of affairs 
exp(betas[1] + betas[2] * 35) # mean number of affairs

predict(M_6_zip, newdata = new_df, type = 'count')


# Over ALL people aged 35, what is the average number of affairs
predict(M_6_zip, newdata = new_df, type = 'response')


# zero inflated negative binomial version of the above
M_6_zinb <- zeroinfl(affairs ~ age, dist = 'negbin', data = affairs_df)


# do the above analysis using e.g. a Poisson
M_6_p <- glm(affairs ~ age, data = affairs_df, family = poisson(link = 'log'))

vuong(M_6_zip, M_6_p)
