
# Load up packages --------------------------------------------------------

library(tidyverse)
library(psyntur)
library(emmeans)
library(afex)


# Visualize the overall score by talk -------------------------------------

tukeyboxplot(y = overall,
             x = talk,
             data = laptop_urry)

describe(data = laptop_urry,
         by = talk,
         mean = mean(overall),
         stdev = sd(overall))


# Oneway ANOVA ------------------------------------------------------------

M_7_1 <- aov(overall ~ talk, data = laptop_urry)
# look at result
summary(M_7_1)

# pairwise comparison
emmeans(M_7_1, specs = pairwise ~ talk, adjust = 'bonf')


# Visualizing the effect of TWO independent variables ---------------------

tukeyboxplot(y = overall,
             x = talk,
             by = condition,
             data = laptop_urry)


# Factorial (two-way) ANOVA -----------------------------------------------

M_7_2 <- aov(overall ~ condition * talk, data = laptop_urry)
summary(M_7_2)

# pairwise comparison
emmeans(M_7_2, specs = pairwise ~ talk | condition, adjust = 'bonf')

