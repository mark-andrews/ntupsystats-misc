

# Emmeans -----------------------------------------------------------------


library(emmeans)

# varying slope and varying intercept model
M_9_1 <- lm(len ~ supp * dose, data = ToothGrowth)

# average predicted value of len for each value of 
# supp, averaged over all values of dose
emmeans(M_9_1, specs = ~ supp)

# to do a logistic regression
ToothGrowth2 <- mutate(ToothGrowth, 
                       dose = factor(dose),
                       len2= len > median(len))

# note that this is not a good model for technical reasons
# related to "perfect/complete separation"
# hence VERY large standard errors
M_9_2 <- glm(len2 ~ supp * dose, 
              data = ToothGrowth2, 
              family = binomial(link = 'logit')
)

# predicted probability of len2 for each value of supp
# averaging over each value of dose
emmeans(M_9_2, specs = ~ supp, type = 'response')

# differences between levels of supp, averaged over dose
emmeans(M_9_2, specs = pairwise ~ supp, type = 'response')


# Bayesian analysis with brms ---------------------------------------------
library(brms)
library(lme4)


# Linear mixed effects model
m_lme <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)

# Bayesian linear mixed effects model
m_brms <- brm(Reaction ~ Days + (Days|Subject), data = sleepstudy)

# If you want to see more introductory brm demos, see 
# https://github.com/mark-andrews/bada02/blob/main/scripts/script_day_2.R
# And for more advaned examples, see
# https://github.com/mark-andrews/bada02/blob/main/scripts/script_day_3.R
# But there is a lot more than brms and Stan can do.
# See the vignettes on https://cran.rstudio.com/web/packages/brms/ for more guides. 



# Predictions in logistic regression --------------------------------------

# Generate a data set of random data
set.seed(1010101)

N <- 100

data_df <- tibble(
  y = sample(c('m', 'f'), 
             size = N, replace = T),
  x1 = rnorm(N),
  x2 = sample(c('a', 'b', 'c'),
              size = N, replace = T),
  x3 = rnorm(N))


# If `y` is intended as the outcome variable for a binary logistic regression,
# we need to do one of three things to make it usable as the binary outcome
# variable.

data_df2 <- data_df %>% 
  mutate(
    # either make it a Boolean, such that if `y` is 'f', `y1` is TRUE and FALSE otherwise
    y1 = y == 'f',
    # or else make if 0 or 1, such that if `y` is 'f', `y2` is 1 and 0 otherwise
    y2 = if_else(y == 'f', 1, 0),
    # or else make it factor, and set the base level to be the first level mentioned in `levels`,
    # In this case, 'm' is the base level, so that is effectively equal to zero
    y3 = factor(y, levels = c('m', 'f')))


# this won't work; y must be binary in one of the three ways mentioned above
m_wont_work <- glm(y ~ x1 + x2 + x3, family = binomial(), data = data_df2)

# but these three are identical
m1 <- glm(y1 ~ x1 + x2 + x3, family = binomial(), data = data_df2)
m2 <- glm(y2 ~ x1 + x2 + x3, family = binomial(), data = data_df2)
m3 <- glm(y3 ~ x1 + x2 + x3, family = binomial(), data = data_df2)

# Do prediction using m1 (same thing with m2 or m3)

# predicting prob that the outcome variable is equal to TRUE for different values of x1
# holding x2 constant at 'a' and x3 constant at the median value of x3 in the data
tibble(x1 = seq(-2, 2, length.out = 50),
       x2 = 'a',
       x3 = median(data_df2$x3)) %>% 
  add_predictions(m1, type = 'response') %>% 
  ggplot(aes(x = x1, y = pred)) + geom_point() + geom_line()


# To predict prob that the outcome variable is equal to TRUE for different values of x1
# for each value of 'a' while x3 is held constant at the median value of x3 in the data
# we use `expand_grid` to create a data frame with each combination of the x1 values and the x2 values
expand_grid(x1 = seq(-2, 2, length.out = 50),
            x2 = c('a', 'b', 'c'),
            x3  = median(data_df2$x3)) %>% 
  add_predictions(m1, type = 'response') %>% 
  ggplot(aes(x = x1, y = pred, colour = x2)) + geom_point() + geom_line()
