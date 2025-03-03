
# load packages -----------------------------------------------------------

library(tidyverse)
library(lavaan)      # sem 
library(lavaanPlot)  # plotting of sem model


# load up some data
lupien_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/lupien22.csv")

# pairwise correlations
cor(lupien_df, use = 'complete.obs')

# specify a mediation model
mediation_model_1 <- '
stress ~ anxiety + resilience
resilience ~ anxiety
'
# do the mediation analysis using `sem` from `lavaan`
mediation_model_1_results <- sem(mediation_model_1, data = lupien_df)

# look at the results using `summary`
summary(mediation_model_1_results)

# plot the model
lavaanPlot(model = mediation_model_1_results, coef = TRUE)

# specify a mediation model; collect more info
mediation_model_2 <- '
stress ~ c_prime * anxiety + b * resilience
resilience ~ a * anxiety

# direct, indirect, total effects
direct := c_prime
indirect := a * b
total := direct + indirect
'

# do the mediation analysis using `sem` from `lavaan`
mediation_model_2_results <- sem(mediation_model_2, data = lupien_df)

summary(mediation_model_2_results)
