# Load packages -----------------------------------------------------------

library(tidyverse)
library(psyntur)
library(afex) # for all types of anova
library(emmeans) # for multiple comparisons


# data vis and exploration ------------------------------------------------

tukeyboxplot(y = overall, x = talk, data = laptop_urry)

describe(data = laptop_urry,
         avg = mean(overall),
         std = sd(overall),
         by = talk)

# one way anova
M_7_1 <- aov(overall ~ talk, data = laptop_urry)

summary(M_7_1)

# post hoc pairwise comparisons
emmeans(M_7_1, specs = pairwise ~ talk, adjust = 'bonf')

# Factorial anova ---------------------------------------------------------

# visualize 
tukeyboxplot(y = overall, x = talk,
             by = condition,
             data = laptop_urry)

describe(data = laptop_urry,
         avg = mean(overall),
         std = sd(overall),
         by = c(talk, condition))

# factorial anova
M_7_2 <- aov(overall ~ talk * condition, data = laptop_urry)
M_7_2a <- aov(overall ~ talk + condition + talk:condition, 
              data = laptop_urry)

summary(M_7_2)

# post hoc
emmeans(M_7_2, specs = pairwise ~ talk + condition, adjust = 'bonf')
