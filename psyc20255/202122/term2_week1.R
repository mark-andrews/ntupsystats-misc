library(tidyverse)
library(psyntur)

data_df <- read_csv("http://data.ntupsychology.net/income.data.csv") #

scatterplot(x = income, y = happiness, data = data_df,
            best_fit_line = T)

M <- lm(happiness ~ income, data = data_df)
summary(M)
