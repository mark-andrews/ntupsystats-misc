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
