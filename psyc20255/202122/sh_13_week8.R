library(tidyverse)
library(psyntur)
library(emmeans)

# Convert `dose` to a factor
ToothGrowth$dose <- factor(ToothGrowth$dose)

# one way anova; outcome is `len`, predictor is `dose`
oneway <- aov(len ~ dose, data = ToothGrowth)

summary(oneway)

# pairwise comparison of means, with a Bonferroni correction
emmeans(oneway, specs = pairwise ~ dose, adjust = 'bonf')

# repeated measures analysis
selfesteem

selfesteem_long <- pivot_longer(selfesteem,
                                cols = starts_with('t'),
                                names_to = 'time',
                                values_to = 'esteem')
                                

oneway_rm <- aov(esteem ~ time + Error(id/time), 
                 data = selfesteem_long)

summary(oneway_rm)

emmeans(oneway_rm, specs = pairwise ~ time, adjust = 'bonf')
