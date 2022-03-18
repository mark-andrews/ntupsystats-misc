# load up lme4
library(lme4)
library(tidyverse)

head(sleepstudy, 15)

ggplot(sleepstudy,
       aes(x = Days, y = Reaction)
) + geom_point() + facet_wrap(~Subject)



# Multilevel linear model -------------------------------------------------

M <- lmer(Reaction ~ Days + (Days|Subject),
          data = sleepstudy)

summary(M)

# the intercept of the average individual
# which is the mean of the normal distribution over populations intercepts
population_mean <- 251.405

# this is the sd of the normal distribution over the population intercepts
population_sd <- 24.741 

# this is 95% interval of the population
c(population_mean - 1.96 * population_sd,
  population_mean + 1.96 * population_sd)


# 95% interval for the distribution of slope effects ----------------------

# the intercept of the average individual
# which is the mean of the normal distribution over populations intercepts
population_mean <- 10.467

# this is the sd of the normal distribution over the population intercepts
population_sd <- 5.922

# this is 95% interval of the population
c(population_mean - 1.96 * population_sd,
  population_mean + 1.96 * population_sd)


