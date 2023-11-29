library(tidyverse)
library(psyntur)
library(afex)
library(emmeans)

# factorial repeated measures ANOVA

# plot the data
tukeyboxplot(y = score, 
             x = time, 
             by = treatment,
             data = selfesteem2_long)

# boxplots for each time
tukeyboxplot(y = score, 
             x = time, 
             data = selfesteem2_long)

# boxplots for each treatment
tukeyboxplot(y = score, 
             x = treatment, 
             data = selfesteem2_long)

# factorial repeated measures anova ---------------------------------------

M_9_1 <- aov_car(score ~ Error(id/time * treatment),
                 data = selfesteem2_long)

summary(M_9_1)
M_9_1

# pairwise comparisons
emmeans(M_9_1, 
        specs = pairwise ~ time | treatment,
        adjust = 'bonf')
        