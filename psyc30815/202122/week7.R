library(tidyverse)

affairs_df <- read_csv(
  "https://vincentarelbundock.github.io/Rdatasets/csv/AER/Affairs.csv") %>% 
  select(-1) %>%
  mutate(had_affair = affairs > 0)

M1 <- glm(had_affair ~ age, 
          data = affairs_df, family = binomial()
)

M2 <- glm(had_affair ~ age + yearsmarried,
          data = affairs_df, family = binomial()
)

M0 <- glm(had_affair ~ 1, 
          data = affairs_df, family = binomial()
)

M3 <- glm(had_affair ~ yearsmarried, 
          data = affairs_df, family = binomial()
)


D0 <- deviance(M0)
D1 <- deviance(M1)
D2 <- deviance(M2)
D3 <- deviance(M3)

# Probability of getting a result greater than D0-D1
# in a Chi square distribution with 1 df
1 - pchisq(D0 - D1, df = 1)
pchisq(D0 - D1, df = 1, lower.tail = F)

pchisq(D0 - D2, df = 2, lower.tail = F)


# Is there a sig predictive performance of yearsmarried
# over and above the predictive performance of age
pchisq(D1 - D2, df = 1, lower.tail = F)

# Is there a sig predictive performance of age
# over and above the predictive performance of yearsmarried

pchisq(D3 - D2, df = 1, lower.tail = F)


# yearsmarried * age 
# yearsmarried + age + yearsmarried:age


anova(M3, M2, test = 'Chisq')

M4 <- glm(had_affair ~ 
            gender + age + yearsmarried + 
            children + religiousness + education + occupation + rating,
          family = binomial(link = 'logit'),
          data = affairs_df)

M5 <- glm(had_affair ~ age + yearsmarried + religiousness + rating,
          family = binomial(link = 'logit'),
          data = affairs_df)

anova(M4, M5, test='Chisq')



# Count models ------------------------------------------------------------

doctor_df <- read_csv("http://data.ntupsychology.net/DoctorAUS.csv")

# model number of visits to doctor as Poisson dist
# that is function of age
M6 <- glm(doctorco ~ age, 
          data = doctor_df, 
          family = poisson(link = 'log'))

summary(M6)

estimates <- coef(M6)

# What is log of the average number of visits 
# for a person of median age, i.e. age = 0.32
estimates[1] + estimates[2] * 0.32

# What is the average number of visits 
# for a person of median age, i.e. age = 0.32
exp(estimates[1] + estimates[2] * 0.32)


# some random samples from a Poisson with lambda = 0.25
rpois(50, lambda = 0.25)



# What is the average number of doctor visits
# for a person of age = .75 (presumably, 75 years)
exp(estimates[1] + estimates[2] * 0.75)

# some random samples from a Poisson with lambda = 0.25
rpois(50, lambda = 0.48)


# model number of visits to doctor as Poisson dist
# that is function of sex (whose values are 0 and 1)
M7 <- glm(doctorco ~ sex, 
          data = doctor_df, 
          family = poisson(link = 'log'))

summary(M7)$coefficients

estimates <- coef(M7)

# what is log of average number of doctor visits 
# for when sex = 0
estimates[1] + estimates[2] * 0

# what is log of average number of doctor visits 
# for when sex = 1
estimates[1] + estimates[2] * 1


# difference in the log of the average between the two cases
# i.e. difference between log of average when sex = 0 and when sex = 1
# the following is identical to estimates[2]
(estimates[1] + estimates[2] * 1) - (estimates[1] + estimates[2] * 0)

# what is this?
# this is the factor by which the average increases
# as we go from sex = 0 to sex = 1
exp(estimates[2])


# what is average number of doctor visits 
# for when sex = 0
lambda_sex_0 <- exp(estimates[1] + estimates[2] * 0)

# what is average number of doctor visits 
# for when sex = 1
lambda_sex_1 <- exp(estimates[1] + estimates[2] * 1)

c(lambda_sex_1, lambda_sex_0, lambda_sex_1 / lambda_sex_0)


M8 <- glm(doctorco ~ sex + age + income + insurance,
          data = doctor_df,
          family = poisson(link = 'log'))
M9 <- glm(doctorco ~ insurance,
          data = doctor_df,
          family = poisson(link = 'log'))

c(deviance(M8), deviance(M9))

anova(M9, M8, test = 'Chisq')
