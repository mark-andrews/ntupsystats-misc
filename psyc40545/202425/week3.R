library(tidyverse)

smoking_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/smoking.csv")

smoking_df <- mutate(smoking_df, is_smoker = cigs > 0)

M_3_1 <- glm(is_smoker ~ educ + age + lincome,
             data = smoking_df,
             family = binomial(link='logit')
)

b <- coef(M_3_1)
exp(b['educ']) # this is e to the power of the coef for educ
# this value i.e. e to the power of the coef, is the *odds ratio* 

# confidence interval of coef
confint.default(M_3_1)

exp(confint.default(M_3_1)) # 95% CI on odds ratios


# Model comparison --------------------------------------------------------

# log likelihood
# deviance
# log likelihood ratio test; often Chi^2 tests

deviance(M_3_1)


M_3_2 <- glm(is_smoker ~ age,
             data = smoking_df,
             family = binomial(link='logit')
)

summary(M_3_2)

deviance(M_3_2) # deviance of model M_3_2
deviance(M_3_1) # deviance of model M_3_1

logLik(M_3_1) # log likelihood of model M_3_1
-2 * logLik(M_3_1)

deviance(M_3_2) - deviance(M_3_1)

# deviance model comparison
anova(M_3_2, M_3_1, test = 'Chisq')

# DIY p-value
pchisq(19.021, df = 2, lower.tail = F)



M_3_3 <- glm(is_smoker ~ age + educ,
             data = smoking_df,
             family = binomial(link='logit')
)

deviance(M_3_1)
deviance(M_3_3)
deviance(M_3_3) - deviance(M_3_1)

anova(M_3_3, M_3_1, test = 'Chisq')

# nullest of the nulls
M_3_4 <- glm(is_smoker ~ 1,
             data = smoking_df,
             family = binomial(link='logit')
)

deviance(M_3_4) # deviance of the nullest of the nulls
# compare to null deviance in e.g. M_3_1 etc 
summary(M_3_1)

anova(M_3_4, M_3_1, test = 'Chisq') # model null hypothesis test
