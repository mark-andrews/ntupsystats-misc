library(tidyverse)
library(lavaan)
library(lavaanPlot)

# get data
lupien22 <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/lupien22.csv")

model_1 <- '
stress ~ anxiety + resilience
resilience ~ anxiety
'

M_5_1 <- sem(model_1, data = lupien22)

summary(M_5_1)

lavaanPlot(model = M_5_1, coefs = TRUE)

model_2 <- '
stress ~ c_prime * anxiety + b * resilience
resilience ~ a * anxiety

indirect := a*b
direct := c_prime
total := direct + indirect
'

M_5_2 <- sem(model_2, data = lupien22)
