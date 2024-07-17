library(tidyverse) # for general stuff
library(psyntur) # for plots etc
library(afex) # for anova
library(emmeans) # for post-hoc comparisons


# Get data ----------------------------------------------------------------

catknowledge <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/catknowledge.csv")


# Mixed within/between factorial ANOVA ------------------------------------

M_10_1 <- aov_car(nc_erp ~ category + Error(id/change),data = catknowledge)

summary(M_10_1)

M_10_1


# Plotting ----------------------------------------------------------------

tukeyboxplot(y = nc_erp, x = change, by = category,
             data = catknowledge)

# just look at `change`
tukeyboxplot(y = nc_erp, x = change,
             data = catknowledge)

# just look at `category`
tukeyboxplot(y = nc_erp, x = category, 
             data = catknowledge)


# Post hoc, followup, tests -----------------------------------------------

emmeans(M_10_1, specs = pairwise ~ change | category, adjust = 'bonf')
