library(tidyverse)
library(AER)

biochem_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/biochemist.csv")

summarise(biochem_df, 
          avg = mean(publications),
          var = var(publications))

set.seed(101)
x <- rpois(1e4, lambda = 1.69)
mean(x)
var(x)

# Poisson regression predicting `publications` from `married`
M_5_1 <- glm(publications ~ married + children + prestige,
             data = biochem_df,
             family = poisson(link='log'))


dispersiontest(M_5_1) # null hypothesis test on equi-dispersion


# Prob of a value of exactly 3 in a Poisson distribution with lambda = 1.75
dpois(x = 3, lambda = 1.75)
# Prob of a value of exactly 3 in a neg bin distribution with mean of 1.75
# and a dispersion parameter of 0.85
dnbinom(x = 3, mu = 1.75, size = 0.85)

library(MASS) # load up glm.nb etc

M_5_2 <- glm.nb(publications ~ married + children + prestige,
                data = biochem_df)
summary(M_5_2)$coef

# the factor by which the mean (number of pubs) changes
# for every unit increase in `children`
exp(-0.12214788) # e to the power of the coef for "children"

exp(-0.19977104) # e to the power of the coef for `Married`

# predictions -------------------------------------------------------------

library(modelr)
new_df <- tibble(married = 'Married', 
                 children = c(0, 1, 2, 3, 4, 5), 
                 prestige = 3)

add_predictions(new_df, M_5_2) # predicted log of the mean 
add_predictions(new_df, M_5_2, type = 'response') # predicted mean 

b <- coef(M_5_2)
# predicted log mean if children=0 and prestige=3 and married='Married'
b[1] + (b[2] * 0) + (b[3] * 0) + (b[4] * 3)

# predicted mean if children=0 and prestige=3 and married='Married'
exp(b[1] + (b[2] * 0) + (b[3] * 0) + (b[4] * 3))


# model's log likelihood x -2 => that's the deviance
logLik(M_5_2) * -2


M_5_3 <- glm.nb(publications ~ married + children + prestige + mentor + gender,
                data = biochem_df)

logLik(M_5_3) * -2 # deviance of model M_5_3
logLik(M_5_2) * -2 # deviance of model M_5_2

anova(M_5_2, M_5_3)
