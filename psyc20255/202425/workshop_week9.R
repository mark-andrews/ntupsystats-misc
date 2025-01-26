library(tidyverse)
library(psyntur)

library(afex) # experimental analysis using ANOVA
library(emmeans) # post-hoc analysis

selfesteem <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/selfesteem_long.csv")

tukeyboxplot(y = esteem, x = time, data = selfesteem)

# one way repeated measures ANOVA
M_8_1 <- aov_car(esteem ~ Error(id/time), data=selfesteem)

summary(M_8_1)

# post-hoc pairwise comparisons
emmeans(M_8_1, specs = pairwise ~ time, adjust='bonf')
