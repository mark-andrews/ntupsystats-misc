library(tidyverse)
library(psyntur)

income_df <- read_csv("http://data.ntupsychology.net/income.data.csv")

histogram(x = income, 
          data = income_df, 
          bins = 25)

histogram(x = happiness, 
          data = income_df, 
          bins = 25)

scatterplot(x = income,
            y = happiness,
            data = income_df)


# linear model ------------------------------------------------------------

M_11_1 <- lm(happiness ~ income, data = income_df)

summary(M_11_1)
coef(M_11_1)  # coeficients
sigma(M_11_1) # residual standard deviation
