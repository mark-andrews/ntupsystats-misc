library(tidyverse)

# get happy2019
happy_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/happy2019.csv")


M_6_1 <- lm(happiness ~ 1 + gdp + support + hle + freedom + generosity + corrupt,
            data = happy_df)


summary(M_6_1)
summary(M_6_1)$r.sq
summary(M_6_1)$adj.r.sq


# Calculate Adjusted R^2 manually
RSS <- sum(M_6_1$residuals^2)
n <- nrow(happy_df)
K <- 6
TSS <- var(happy_df$happiness) * (n-1)

penalty <- (n-1)/(n-K-1)

1 - (RSS/TSS) * penalty # this is adjusted R^2
1 - (RSS/TSS) * ((n-1)/(n-K-1))

# The null model: no predictors ....
M_6_null <- lm(happiness ~ 1, data = happy_df)
summary(M_6_null)$r.sq

# model comparison F test
anova(M_6_null, M_6_1) # null hypothesis 


# Model comparison --------------------------------------------------------

# happiness ~ gdp + support + hle + freedom + generosity + corrupt,
M_6_2 <- lm(happiness ~ gdp + support, data = happy_df)

anova(M_6_2, M_6_1) # null hypothesis that hle, freedom, generosity, corrupt are NULL


# Confidence --------------------------------------------------------------

confint(M_6_1)
confint(M_6_1, level = 0.99)
confint(M_6_1, level = 0.99, parm = 'gdp')



# Predictions -------------------------------------------------------------

happy_df_2 <- tibble(gdp = c(1, 2),
                     support = c(2, 0))

predict(M_6_2, newdata = happy_df_2)

# using add_predictions
library(modelr)

add_predictions(happy_df_2, M_6_2)

# prediction confidence intervals
predict(M_6_2, newdata = happy_df_2, interval = 'confidence')
predict(M_6_2, newdata = happy_df_2, interval = 'confidence', level = 0.99) # 99% CI