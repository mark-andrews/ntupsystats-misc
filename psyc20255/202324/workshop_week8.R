library(tidyverse)
library(psyntur)
library(afex)
library(emmeans)


selfesteem <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/selfesteem_long.csv")

# boxplot of the self esteem data
tukeyboxplot(y = esteem, x = time, data = selfesteem)

# one way repeated measures anova
M_8_1 <- aov_car(esteem ~ Error(id/time), data = selfesteem)

# Note: compare the above to non-repeated measures anova, which would be written something like
# aov(esteem ~ time, data = selfesteem)

summary(M_8_1)
M_8_1


# post-hoc comparisons
emmeans(M_8_1, specs = pairwise ~ time, adjust = 'bonf')
