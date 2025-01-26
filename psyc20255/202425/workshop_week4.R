library(tidyverse)
library(psyntur)

rwas_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/rwas.csv")

# multiple regression
M_4_1 <- lm(rightwing ~ extraverted +  critical + dependable + anxious + open, data = rwas_df)

summary(M_4_1)

# get residuals
rwas_df <- mutate(rwas_df, residuals = residuals(M_4_1))

histogram(residuals, bins = 50, data = rwas_df)

# QQplot
plot(M_4_1, which = 2)

plot(M_4_1, which = 1)
plot(M_4_1, which = 3)

summary(M_4_1)
