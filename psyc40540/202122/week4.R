library(tidyverse)


# Reading in files from working directory ------------------------------------

# This won't work unless "problem_set_01_dataset_01.csv" is in your working 
# directory.
read_csv("problem_set_01_dataset_01.csv")

# To find out what your working directory is, type `getwd()`. To change your
# working directory, go to the "Session" menu, choose "Set Working Directory",
# and the choose "Choose directory" and choose whichever directory you want as
# your working directory. However, it is better to to use RStudio Projects, and
# then you never have to worry about this matter. See Step 10 of the following
# for more details.
# https://now.ntu.ac.uk/d2l/le/content/794264/viewContent/7184501/View


# Areas under curve in t-distributions, and p-values ----------------------

# Using the cumulative distribution function of the t-distribution, verify
# the probability of getting a result as or more extreme than the observed
# t-statistic, assuming the null hypothesis is true, is exactly the p-value you
# obtained from the t-test.

# Before we proceed, let us use the cumulative distribution function for
# t-distribution. In R, this is the `pt` function. This gives us areas under the
# curve in t-distributions, which is equivalent to the probability of being
# below or above any specified value. For example, ...

# Prob of being BELOW 2 in a 10 df t-dist
pt(2, df = 10)

# Prob of being ABOVE 2 in a 10 df t-dist
1 - pt(2, df = 10)

# Alternatively, prob of being ABOVE 2 in a 10 df t-dist
pt(2, df = 10, lower.tail = FALSE)

# Prob of being BELOW -2 in a 10 df t-dist
pt(-2, df = 10)


# Read in data from URL
data_df <- read_csv(
  "http://data.ntupsychology.net/problem_set_01_dataset_01.csv"
)

# Test null hypothesis using t.test. Save result as `M`
M <- t.test(score ~ group, data = data_df, var.equal = TRUE)

# What is the probability of getting a value LESS THAN the t-statistic?
pt(M$statistic, df = M$parameter)

# Because the t-statistic is negative (-3.713352), the probability of getting a
# value less the t-statistic is the lower tail. Because any t-distribution is
# symmetric, the upper tail is the same size as the lower tail. In this case,
# the upper tail is the probability of getting a value greater than 3.713352.

# The probability of being as or more extreme that -3.713352 is the probability
# of being below -3.713352 plus the probability of being above 3.713352. For
# reasons just mentioned this is just twice the lower tail (or equivalently,
# twice the upper tail), i.e. 
pt(M$statistic, df = M$parameter) * 2

# We can verify that this is indeed the correct p-value by looking at M's
# p-value as follows:
M$p.value

# Non-null hypothesis testing ---------------------------------------------

# By default, t.test test a null hypothesis, i.e. the hypothesis that the
# population mean of the first group minus the population mean of the second
# group is 0.
# We can test any hypothesis, not just the null hypothesis.
# In t.test, we do this with the `mu` argument.
# For example, the hypothesis that the difference between the population mean of
# the first group and that of the second is -5 is specified as follows:
t.test(score ~ group, data = data_df, mu = -5, var.equal = TRUE)

