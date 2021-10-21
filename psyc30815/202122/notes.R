
# Converting categorical variables to numbers -----------------------------

countries <- c("england", "ireland", "scotland", "wales")

# How can we convert these values to 1, 2, 3, 4?

# First, convert to a factor, then convert the factor to a numeric.
countries <- as.factor(countries) 
as.numeric(countries)

# What about converting variable with two values to 0 and 1?

countries <- c("england", "ireland")

# do the same thing...
countries <- as.factor(countries) 
as.numeric(countries) # here we get back 1 and 2

# so subtract 1 to get 0 and 1
as.numeric(countries) - 1 

# Using binary valued character vector as outcome variable in logistic reg -----

library(tidyverse)

# A data frame with 100 rows, where y is either 'a' or 'b'
# and x is numeric
data_df <- tibble(y = sample(c('a', 'b'),
                             size = 100, 
                             replace = T),
                  x = rnorm(100)
)

# Although `y` has just two values, if you try to do a logistic
# regression with `y` as outcome you get and error

# errors here ....
glm(y ~ x, 
    data = data_df, 
    family = binomial(link = 'logit')
)

# So we convert `y` to a factor, which we can do 
# inside the glm command like this:
glm(factor(y) ~ x, 
    data = data_df, 
    family = binomial(link = 'logit')
)

