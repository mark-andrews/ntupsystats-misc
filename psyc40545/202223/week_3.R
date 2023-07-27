library(tidyverse)

smoking_df <- read_csv("http://data.ntupsychology.net/smoking.csv")

smoking_df <- mutate(smoking_df, smoker = cigs > 0)

M_3_1 <- glm(smoker ~ educ + age + lincome,
             data = smoking_df,
             family = binomial(link = 'logit')
)

summary(M_3_1)

# Odds ratios -------------------------------------------------------------

# for example
p <- 0.6
q <- 0.4

odds_p <- p/(1-p)
odds_q <- q/(1-q)

odds_ratio <- odds_p / odds_q


# Calculate odds ratio of coefficients ------------------------------------

betas <- coef(M_3_1)
betas <- M_3_1$coefficients

# e to the power of the 3 predictor coefficients
exp(betas)[2:4]


# Confidence intervals ----------------------------------------------------

summary(M_3_1)

confint.default(M_3_1)

# confidence interval for odds ratio
# for example, for `educ`
exp(confint.default(M_3_1, parm = 'educ'))



# Model comparison --------------------------------------------------------

summary(M_3_1)

deviance(M_3_1)
M_3_1$deviance



# Null hypothesis test ----------------------------------------------------

# null model
M_3_2 <- glm(smoker ~ 1,
             data = smoking_df,
             family = binomial(link = 'logit')
)

deviance(M_3_2)

# * calculate difference of deviance
# * calculate difference in number of coefficients
# * calculate AUC in Chi sq distribution with df from step 2
anova(M_3_2, M_3_1, test = 'Chisq')
