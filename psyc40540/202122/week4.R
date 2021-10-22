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


# Confidence intervals ----------------------------------------------------

# What is a confidence interval? For the t-test like problem, the 95% confidence
# interval is the range of hypothetical values that we would not reject at the
# 0.05 level of significance were we to test them in a hypothesis test.

# The 95% confidence interval for the problem above is above as follows:
M$conf.int

# If we were to test the hypothesis that the true value of the difference between the population
# means of the two groups is any value in the interval, we would obtain a p-value > 0.05.

# By the way, we wanted the e.g. 99% confidence interval for this t.test
# problem, we could do the following:
t.test(score ~ group, data = data_df, var.equal = TRUE, conf.level = 0.99)

# Where do we calculate the confidence interval manually?
# For reasons explained in the lecture, the confidence interval is calculate by
# some multiple of the standard error above and below the difference of the 
# same means.
# The multiple is based on areas under the curve in t-distributions.
# In particular, it is the value `x` such that (-x, x) contains 95% of the 
# area under a curve in a given t-distribution.
# We can obtain this value using the inverse cumulative distribution function 
# of the t-distribution, which is `qt`.
# The `qt` function tells us the value below which lies a given percentage 
# of the area under the curve in t-distribution.

# For example, the value below which lies 75% of the area under the curve
# in  a t-distribution with 10 degrees of freedom is as follows:
qt(0.75, df = 10)


# For example, the value below which lies 25% of the area under the curve
# in  a t-distribution with 20 degrees of freedom is as follows:
qt(0.25, df = 20)


# For example, the value below which lies 33% of the area under the curve
# in  a t-distribution with 17 degrees of freedom is as follows:
qt(0.33, df = 17)

# As stated above, we need to know the values between which lies 95% of the area under the 
# curve in a t-distribution. We get this with qt(0.975, df = ...).
# For example, the following tells us the value below which lies 95% of the area
# under the curve in a 10 df t-distribution:
qt(0.975, df = 10)

# From this, we know that 95% (not a typo) of the area under the curve lies between
# -2.228 and +2.228. It is therefore this quantity of 2.228 that we would use
# when calculating confidence intervals in 10 df t-distributions.

# Why do we do qt(0.975, ...) to get the 95% confidence interval?
# Because if 97.5% of the area under the curve lies below say 2.228, then 2.5%
# is above 2.228, and 2.5% is below -2.228.

# So, finally, to get the 95% confidence interval manually, we get the standard
# error and we multiply the standard error by the value we get from qt(0.975, ...).
# Then we add and subtract that from the differences in the sample means.

# Do this using the model `M` above.

# differences in sample means
mean_diff <- M$estimate['mean in group group_0'] - M$estimate['mean in group group_1']

# standard error 
se <- M$stderr

# the value from qt(0.975, ...), which we will call the `t_multiplier`
t_multiplier <- qt(0.975, df = M$parameter)

# now, we subtract and add se x t_multiplier to the differences in means
c(mean_diff - se * t_multiplier, mean_diff + se * t_multiplier)

# And we can see that is the same as the reported 95% confidence interval
M$conf.int
