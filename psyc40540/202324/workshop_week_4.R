library(tidyverse)
library(psyntur)

pain_df <- read_csv('data/pain.csv')

options(scipen = 50)

# null hypothesis test
M_4_1 <- t.test(time ~ group, data = pain_df, var.equal = T)


# area under the t distribution (df=38) curve
# below -5.4472
pt(-5.4472, df = 38)

# area under the t distribution (df=38) curve
# below -5.4472 or above 5.4472
pt(-5.4472, df = 38) * 2

t_stat <- -5.4472

# area under the curve ABOVE the absolute value of the t-stat
pt(abs(t_stat), df = 38, lower.tail = F) 

# so the p-value is always 
pt(abs(t_stat), df = 38, lower.tail = F) * 2


describe(pain_df, 
         avg = mean(time), 
         std = sd(time), 
         n = n(), 
         by=group)

xbar <- 31.9
ybar <- 46.8
s_x <- 7.58
s_y <- 9.65
n_x <- 20
n_y <- 20

# standard error
numerator_term_1 <- (n_x - 1) * s_x^2 + (n_y -1 ) * s_y^2
denom_term_1 <- n_x + n_y -2 

# term 1 (left term)
sqrt(numerator_term_1/denom_term_1)

# term 2 (right term)
sqrt(1/n_x + 1/n_y)

# standard error = term_1 x term_2
sqrt(numerator_term_1/denom_term_1) * sqrt(1/n_x + 1/n_y)



# Confidence intervals -----------------------------------------------------

M_4_2 <- t.test(time ~ group, 
                data = pain_df, var.equal = T,
                conf.level = 0.99)

# 
t.test(time ~ group, mu = -8, data = pain_df, var.equal = T)

# 95% CI is this multiplied by se above/below the diff means
qt(0.975, df = 38) 
