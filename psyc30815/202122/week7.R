# sample some values from a Poisson distribution
set.seed(1001)
x <- rpois(100, lambda = 5)
mean(x)
var(x)
var(x)/mean(x)

library(tidyverse)
biochem_df <- read_csv("http://data.ntupsychology.net/biochemist.csv")

x <- biochem_df$publications
mean(x)
var(x)
var(x)/mean(x)

library(MASS)


M <- glm.nb(publications ~ prestige, data = biochem_df)

summary(M)
deviance(M) # does not work; is meaningless; ignore
D <- (-2 * logLik(M))[[1]]

# Poisson regression counterpart of M
Mp <- glm(publications ~ prestige, 
          data = biochem_df, 
          family = poisson(link = 'log'))

# To test the null-hypothesis of equidispersion
library(AER)
dispersiontest(Mp, trafo = 1)

nbreg_deviance <- function(M) (-2 * logLik(M))[[1]]


summary(M)

# extract the coefficients
estimates <- coef(M)

# predicted log of the mean of the number of pubs 
# as a function of prestige from 1 to 5
estimates[1] + estimates[2] * seq(5)

# predicted mean of the number of pubs 
# as a function of prestige from 1 to 5
exp(estimates[1] + estimates[2] * seq(5))

exp(estimates[2])

M0 <- glm.nb(publications ~ 1, data = biochem_df)
anova(M0, M)



# Zero inflated count models ----------------------------------------------

smoking_df <- read_csv("http://data.ntupsychology.net/smoking.csv")

barplot(table(smoking_df$cigs))

library(pscl)

Mzip <- zeroinfl(cigs ~ educ, data = smoking_df)
Mzip_p <- glm(cigs ~ educ, data = smoking_df, 
              family = poisson(link = 'log'))
summary(Mzip_p)$coefficients


coef(Mzip)
summary(Mzip)

estimates <- coef(Mzip)

# predicted log odds of being a smoker if educ = 5, 10, 15, 20
estimates[3] + estimates[4] * c(5, 10, 15, 20)

# predicted probability of being a smoker if educ = 5, 10, 15, 20
plogis(estimates[3] + estimates[4] * c(5, 10, 15, 20))

# predicted log of the average no of cigs
# smoked by a smoker with 5, 10, 15, or 20 years of educ
estimates[1] + estimates[2] * c(5, 10, 15, 20)

# predicted average no of cigs
# smoked by a smoker with 5, 10, 15, or 20 years of educ
exp(estimates[1] + estimates[2] * c(5, 10, 15, 20))


smoking_df_2 <- tibble(educ = seq(20))
library(modelr)

# predicted probability of being a non-smoker
# as a function of educ = 1 ... 20
add_predictions(smoking_df_2, Mzip, type = 'zero')

# predicted average number of cigs smoked by smokers
# as a function of educ = 1 .... 20
add_predictions(smoking_df_2, Mzip, type = 'count')

# predicted average number of cigs smoked by smokers AND non-smokers
# as a function of educ = 1 .... 20
add_predictions(smoking_df_2, Mzip, type = 'response')

# null hypothesis test comparing zero inflated to Poisson
vuong(Mzip, Mzip_p)
vuong(Mzip_p, Mzip) # same as previous, just numbers reversed
