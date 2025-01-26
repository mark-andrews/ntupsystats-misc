library(tidyverse)
library(psyntur)
library(afex)
library(emmeans)


# One within, one between mixed ANOVA -------------------------------------

catknow_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/catknowledge.csv")

M_10_1 <- aov_car(nc_erp ~ category + Error(id/change), data = catknow_df)
summary(M_10_1)
M_10_1

emmeans(M_10_1, specs = pairwise ~ change | category, adjust = 'bonf')


# Two within subject ANOVA ------------------------------------------------

beh_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/behmercrump.csv")

M_10_2 <- aov_car(log_rt ~ Error(subject/keyboard * condition), data = beh_df)

emmeans(M_10_2, specs = pairwise ~ condition | keyboard, adjust = 'bonf')
