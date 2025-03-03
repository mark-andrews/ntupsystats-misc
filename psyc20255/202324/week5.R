library(tidyverse)
library(lavaan) # we use this for the mediation analysis
library(lavaanPlot) # this is used for making a network diagram

# read in data
lupien_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/lupien22.csv")

# write the mediation regression formulas (two different formulas)

mediation_model_formula <- '
stress ~ anxiety + resilience
resilience ~ anxiety
'

# put the formulas into sem 
M_5_1 <- sem(mediation_model_formula, data = lupien_df)

# view the output
summary(M_5_1)

# produce a path diagram
lavaanPlot(model = M_5_1, coefs = TRUE)

# collect more information
mediation_model_formula2 <- '
stress ~ c_prime * anxiety + b * resilience
resilience ~ a * anxiety

direct := c_prime
indirect := a * b
total := direct + indirect
'

M_5_2 <- sem(mediation_model_formula2, data = lupien_df)

summary(M_5_2)
