library(tidyverse)
library(psyntur)

# Get the data
happy_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/happy2019.csv")

# multiple linear regression predicting
# happines from gdp and support
M_2_1 <- lm(happiness ~ gdp + support, data = happy_df)

# look at results
summary(M_2_1)

coef(M_2_1) # same as Estimate column
coef(M_2_1)[2:3]
  w
# confidence intervals
confint(M_2_1)
  

M_2_2 <- lm(happiness ~ gdp + support + hle, data = happy_df)
