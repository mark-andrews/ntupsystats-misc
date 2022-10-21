
# Load packages -----------------------------------------------------------

library(tidyverse)

# set scipen
options(scipen = 50)

# Load data ---------------------------------------------------------------

pain_df <- read_csv("http://data.ntupsychology.net/pain.csv")
M <- t.test(time ~ group, data = pain_df, var.equal = TRUE)

t.test(time ~ group, mu = -5, data = pain_df, var.equal = TRUE)

# confidence interval different to 95%, e.g 99%
t.test(time ~ group, conf.level = 0.99, data = pain_df, var.equal = TRUE)



# t tests the hard way ----------------------------------------------------

# times for the noswear group
x <- filter(pain_df, group == 'noswear')$time

# times for the swear group
y <- filter(pain_df, group == 'swear')$time

# mean and standard deviation of x and y
xbar <- mean(x) # mean of x
ybar <- mean(y) # mean of y
s_x <- sd(x) # stdev of x
s_y <- sd(y) # stdev of y
n_x <- length(x) # sample size of x 
n_y <- length(y) # sample size of y

# standard error

term_1_numerator <- (n_x - 1) * s_x^2 + (n_y - 1) * s_y^2
term_1_denominator <- n_x + n_y - 2

term_1 <- sqrt(term_1_numerator / term_1_denominator)
term_2 <- sqrt(1/n_x + 1/n_y)
se <- term_1 * term_2


TQ <- qt(0.975, df = 38)
(xbar - ybar) + TQ * se
(xbar - ybar) - TQ * se
