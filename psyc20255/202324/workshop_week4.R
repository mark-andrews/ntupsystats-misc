# Load packages -----------------------------------------------------------
library(tidyverse)
library(psyntur)

# Load data ---------------------------------------------------------------

rwas_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/rwas.csv")


# Multiple linear regression ----------------------------------------------

# predict `rightwing` from extraverted, critical, dependable, anxious,open
M_4_1 <- lm(rightwing ~ extraverted + critical + dependable + anxious + open,
            data = rwas_df)

# histogram of the outcome variable `rightwing`
histogram(rightwing, data = rwas_df, bins = 25)

# plot the histogram of residuals
hist(M_4_1$residuals, 25)

# QQ plot
plot(M_4_1, which = 2)

# Residuals versus fitted plot
plot(M_4_1, which = 1)

# scale location plot
plot(M_4_1, which = 3)


# Overall model fit -------------------------------------------------------

# R^2 
summary(M_4_1)$r.squared

# Adjusted R^2
summary(M_4_1)$adj.r.squared

# F statistic for the null hypothesis
summary(M_4_1)$fstatistic
# report it like this:
# F(5, 9469) = 298.1953

M_4_2 <- lm(rightwing ~ dependable + anxious + open,
            data = rwas_df)

# null hypothesis test comparing model M_4_1 and M_4_2
anova(M_4_2, M_4_1)
