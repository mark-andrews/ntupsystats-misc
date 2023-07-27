# Multiple grouping variables
# Deviance of multilevel model
# Deviance based model comparison
# Probability distributions of random effects
# Multiple grouping variables with crossed effects

# Load up our package and data
library(lme4)
library(tidyverse)

classroom_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/classroom.csv")
blp_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/blp_subset.csv")

M_8_1 <- lmer(mathscore ~ ses + (ses|schoolid), data = classroom_df)
summary(M_8_1)



M_8_2 <- lmer(mathscore ~ ses + (ses|schoolid) + (ses|classid), data = classroom_df)
summary(M_8_2)


M_8_3 <- lmer(mathscore ~ ses + (ses|schoolid) + (1|classid), data = classroom_df)
summary(M_8_3)


# Deviance of a multilevel
deviance(M_8_2)

# To do the maximum likelihood version of the model, state REML = FALSE
M_8_3a <- lmer(mathscore ~ ses + (ses|schoolid) + (1|classid), REML = FALSE, data = classroom_df)
deviance(M_8_3) # warning here

# deviance of model `mathscore ~ ses + (ses|schoolid) + (1|classid)`
deviance(M_8_3a) # no warning :)


M_8_1a <- lmer(mathscore ~ ses + (ses|schoolid), REML = FALSE, data = classroom_df)
# deviance of model `mathscore ~ ses + (ses|schoolid)`
deviance(M_8_1a)

# model comparison of M_8_1a and M_8_3a 
anova(M_8_1a, M_8_3a, test = 'Chisq')
anova(M_8_1, M_8_3, test = 'Chisq')

# What is the interval within which lies 95% of the random intercepts in M_8_1?
stdev <- 16.180 
mu <- 522.946 

# use qnorm, for inverse of area under normal distributions
qnorm(0.975, mean = mu, sd = stdev)
qnorm(0.025, mean = mu, sd = stdev)




# What is the interval within which lies 99% of the random intercepts in M_8_1?

# use qnorm, for inverse of area under normal distributions
qnorm(0.995, mean = mu, sd = stdev)
qnorm(0.005, mean = mu, sd = stdev)


# Multilevel linear model with outcome latency, and predictor LOG of frequency
# random intercepts by subject AND random intercepts by item
M_8_4 <- lmer(latency ~ log(frequency) + (1|subject) + (1|item), data = blp_df)
