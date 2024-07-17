library(tidyverse)

smoking <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/smoking.csv")

smoking <- mutate(smoking, is_smoker = cigs > 0)

M_3_1 <- glm(is_smoker ~ educ + age + lincome, data = smoking,
             family = binomial(link = 'logit')
)

round(summary(M_3_1)$coefficients, 3)

exp(-0.114) # e ^ -0.114

b <- coef(M_3_1)
exp(b[2:4])


# confidence intervals for coefficients
confint.default(M_3_1)

exp(confint.default(M_3_1))



M_3_2 <- glm(is_smoker ~ educ, 
             data = smoking,
             family = binomial(link = 'logit')
)

deviance(M_3_1)
deviance(M_3_2)

deviance(M_3_2) - deviance(M_3_1)

-2 * logLik(M_3_1)

pchisq(deviance(M_3_2) - deviance(M_3_1), df = 2, lower.tail = F)

anova(M_3_2, M_3_1, test = 'Chisq')


# the nullest of the null
M_3_null <- glm(is_smoker ~ 1, 
                data = smoking,
                family = binomial(link = 'logit')
)
deviance(M_3_null)

anova(M_3_null, M_3_1, test = 'Chisq')