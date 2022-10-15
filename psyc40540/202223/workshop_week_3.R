
# Load packages -----------------------------------------------------------

library(tidyverse)
library(psyntur)

# Read in data ------------------------------------------------------------

# read in the data from a url
pain_df <- read_csv("http://data.ntupsychology.net/pain.csv")
# pain <- read_csv("~/Downloads/pain.csv")

# Descriptives ------------------------------------------------------------

# calculate mean and standard dev and sample size for each group
describe(pain_df, 
         avg = mean(time), # calculate mean
         stdev = sd(time), # calculate standard dev
         n = length(time), # calculate sample size
         by = group) # for each group


# null hypothesis test on differences of means ----------------------------

M_3_1 <- t.test(time ~ group, data = pain_df, var.equal = TRUE)

# tell R not to show scientific notation
options(scipen = 50)


# What is the probability of get a t statistic lower than the one
# we obtained if the null is true
pt(M_3_1$statistic, df = 38)

# What is the probability of getting a t statistic *greater* than 
# +5.447215 if the null is true
pt(-M_3_1$statistic, df = 38, lower.tail = FALSE)


# what is the probability of getting a t statistic
# *as or more extreme* than the one we obtained if null is true
pt(M_3_1$statistic, df = 38) * 2

# Test a non-null hypothesis ----------------------------------------------

M_3_2 <- t.test(time ~ group, 
                data = pain_df, 
                mu = -10, # the non-null that the diff is -10
                var.equal = TRUE)
