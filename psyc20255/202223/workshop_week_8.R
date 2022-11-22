# one way repeated measures anova


# Load up the packages ----------------------------------------------------

library(psyntur)
library(tidyverse)
library(afex) # for rm anova
library(emmeans) # post hoc comparisons


# Load up data ------------------------------------------------------------

esteem_df <- read_csv("http://data.ntupsychology.net/selfesteem_long.csv")

# some visualization
tukeyboxplot(y = esteem, x = time, data = esteem_df)

# descriptives
describe(esteem_df, by = time,
         avg = mean(esteem),
         stdev = sd(esteem))


# Repeated measures one way anova -----------------------------------------

M_8_1 <- aov_car(esteem ~ Error(id/time), data = esteem_df)
summary(M_8_1)
nice(M_8_1) # just the things I need to know please ....


# Post-hoc comparisons
emmeans(M_8_1, specs = ~ time)
emmeans(M_8_1, specs = pairwise ~ time, adjust = 'bonf')
