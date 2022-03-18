library(tidyverse)
smoking_df <- read_csv("http://data.ntupsychology.net/smoking.csv")

smoking_df <- mutate(smoking_df, smoker = cigs > 0)

M <- glm(smoker ~ educ + age,
         data = smoking_df,
         family = binomial(link = 'logit')
)

summary(M)$coefficients

coefs <- coef(M)
exp(coefs['educ']) # e to the power of the coefficient, which is the **odds ratio**
exp(coefs['age'])

summary(M)

logLik(M) # log likelihood of the model
logLik(M) * -2 # deviance, also in summary()
deviance(M)

M_1 <- glm(smoker ~ educ,
           data = smoking_df,
           family = binomial(link = 'logit')
)

deviance(M)
deviance(M_1)

deviance(M) - deviance(M_1)
 
# the easy way
anova(M_1, M, test = 'Chisq')

# the longer way
delta <- deviance(M_1) - deviance(M)
pchisq(delta, df = 1, lower.tail = F)  


M_2 <- glm(smoker ~ educ + age + cigpric + income,
           data = smoking_df,
           family = binomial(link = 'logit')
)
anova(M_1, M_2, test = 'Chisq')
