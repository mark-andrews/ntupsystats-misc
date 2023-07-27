# Two within-subjects factor repeated measures anova

library(tidyverse)
library(psyntur)
library(afex)
library(emmeans)

esteem_df <- selfesteem2_long


# Visualization -----------------------------------------------------------

tukeyboxplot(y = score, x = time, by = treatment,
             data = esteem_df)


# Summary stats -----------------------------------------------------------

describe(data = esteem_df,
         by = c(treatment, time),
         avg = mean(score),
         stdev = sd(score))


# Factorial repeated measures anova ---------------------------------------

M_9_1 <- aov_car(score ~ Error(id/time * treatment), data = esteem_df)
summary(M_9_1)
nice(M_9_1)


# Post-hoc pairwise comparisons -------------------------------------------

emmeans(M_9_1, specs = pairwise ~ treatment|time, adjust = 'bonf')
emmeans(M_9_1, specs = pairwise ~ time|treatment, adjust = 'bonf')
