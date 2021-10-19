# Doing predictions in regression models ----------------------------------

# Remember, when doing predictions in any regression model, you must use all
# predictor variables. In other words, you can only make predictions for
# hypothetical observations that have values for all the predictors. If you are
# just interested in the change of the predicted value of the outcome for
# changes in one or a few variables, you still have to include values for the
# other variables, but you can just hold them constant at arbitrary values.

# we will use a data set from psyntur
data(ansur, package = 'psyntur')

# Model weight as a function of five variables
M <- lm(weight ~ height + age + gender + handedness + race, data = ansur)

# Create a data frame of hypothetical people who differ in height and age, but
# who have same gender, handedness, and race, Even though we are primarily
# looking at changes in the predicted means of weight with changes in height and
# age, we still we must have values for all predictor variables in the model.
new_df <- tidyr::expand_grid(height = seq(150, 200, by = 10),
                             age = seq(20, 50, by = 5),
                             gender = 'male',
                             handedness = 'right',
                             race = 'white'
)

# Do the predictions using the `add_predictions` function.
# These predictions tell us how the predicted value of `weight` changes when we
# change `height` and `age`, assuming that `gender` is `male`, `handedness` is
# `right`, and `race` is `white`.
modelr::add_predictions(new_df, M)


# Convert probabilities to log odds ---------------------------------------

# theta is a vector of probabilities
theta <- c(0.1, 0.5, 0.9)

# we want to get phi = log(theta/(1-theta)), or the log odds, or logits

# first, get the "odds"
odds <- theta / (1 - theta)

# then, the log (to base e) of these odds
log(odds) # these are the log odds of theta


# Binary logistic regression -----------------------------------------------


# Get the affairs data set
affairs_df <- read_csv(
  "https://vincentarelbundock.github.io/Rdatasets/csv/AER/Affairs.csv")

# Remove the meaningless first column
affairs_df <- select(affairs_df, -1)

# Create a binary outcome variable this takes the value of TRUE if the person
# had an extramarital affair.
affairs_df <- mutate(affairs_df, had_affair = affairs > 0)

# A binary logistic regression modelling the probability of having an
# extramarital affair as a function of the number of years the person is married?
M <- glm(had_affair ~ yearsmarried, 
         family = binomial(link = 'logit'),
         data = affairs_df)

# Get the coefficients
estimates <- coef(M)

# What is the log odds of having an affair if the person has been married for 10
# years?
estimates[1] + estimates[2] * 10

# What is the log odds of having an affair if the person has been married for 20
# years?
estimates[1] + estimates[2] * 20

# What is the log odds of having an affair if the person has been married for 30
# years?
estimates[1] + estimates[2] * 30



# What is the probability of having an affair if the person has been married for
# 10 years?
# Here, we use the inverse logit function, which is implemented in R as `plogis`.
plogis(estimates[1] + estimates[2] * 10)

# What is the probability of having an affair if the person has been married for
# 20 years?
plogis(estimates[1] + estimates[2] * 20)

# What is the probability of having an affair if the person has been married for
# 30 years?
plogis(estimates[1] + estimates[2] * 30)

# What is predicted log odds of having an affair for a range of different values
# of yearsmarried? First, make the data frame.
new_df <- tibble(yearsmarried = seq(0, 50))

# Using `predict`.
predict(M, newdata = new_df)

# Alternatively, use `add_predictions` from `modelr`.
# In this case, we immediately pipe to `ggplot`.
add_predictions(new_df, M) %>% 
  ggplot(aes(x = yearsmarried, y = pred)) +
  geom_line()

# We can do the same as the above, but get predicted probabilities, rather than
# predicted log odds.

# The predicted *probability* using `predict`
predict(M, newdata = new_df, type = 'response')

# Predicted probabilities, immediately piped to ggplot.
add_predictions(new_df, M, type = 'response') %>% 
  ggplot(aes(x = yearsmarried, y = pred)) +
  geom_line()


# Odds ratios -------------------------------------------------------------

# The odds ratio for a given predictor is the factor by which the odds changes
# for every unit increase in that predictor.

# In logistic regression, the maths works out such that the odds ratio for a
# given predictor is just e to the power of that predictor's coefficient.
exp(estimates[2])

# We can verify that e to the power of the coefficient is correct as follows.

# First, what is the *odds* of having an affair if the person has been married
# for 10 years.
p <- plogis(estimates[1] + estimates[2] * 10)
p / (1 - p) # the odds

odds_10 <- exp(estimates[1] + estimates[2] * 10)

# Now, what is the *odds* of having an affair if the person has been married for
# 11 years.
p <- plogis(estimates[1] + estimates[2] * 11)
p / (1 - p) # the odds

odds_11 <- exp(estimates[1] + estimates[2] * 11)

# The odds ratio corresponding to a unit increase is as follows:
odds_11 / odds_10

# Verify that that is e to the power of the coefficient.
exp(estimates[2])


# Confidence intervals ----------------------------------------------------

# We get the 95% confidence intervals on all coefficient as follows:
confint.default(M)

# 95% confidence interval on the coefficient for yearsmarried, rounded
confint.default(M, parm = 'yearsmarried') %>%
  round(3)

# 95% confidence interval on the odds ratio for yearsmarried, rounded
confint.default(M, parm = 'yearsmarried') %>% 
  exp() %>% 
  round(3)


# Controlling for covariates ----------------------------------------------

# What is the relationship between, for example, age and the probability of
# having an affair? To understand this, we must control for other variables that
# covary with both age and the probability of having an affair.

# To see how this works, we will make two models.

# First, we model probability of having affair as function of age alone.
M1 <- glm(had_affair ~ age, 
          data = affairs_df, family = binomial()
)

# Second, we model probability of having affair as function of age and
# yearsmarried.
M2 <- glm(had_affair ~ age + yearsmarried,
          data = affairs_df, family = binomial()
)

# looks at coefficients table in both M1 and M2
summary(M1)$coefficients

summary(M2)$coefficients

# In the case of `M1`, we have a highly significant positive effect of age.
# In the case of `M2`, we have a significant negative effect of age.

# Why the change from a significant positive effect of age to a significant
# negative effect of age? Model M1 tells us that as age increases, so too does
# the probability of having an affair. However, as age increases, so to do other
# variables, such as yearsmarried; older married people tend to married longer
# than younger married people. If people who have been married for a long time
# tend to have a higher probability of having an affair, by just looking at the
# relationship between age and probability of having an affair, it will look
# like there is a positive relationship. However, if we control for the number
# of years the person is married, then we can isolate the effect of age on the
# probability of having an affair. In the model M2 above, we interpret the
# coefficient of the age variable as follows: it is the change in the log-odds
# of having an affair with a unit change in age *assuming yearsmarried is held
# constant*. In other words, we can imagine a set of people who range in their
# ages from younger to older, but who have been married for the exact same
# number of years. For example, we could have three people whose ages are 25,
# 35, and 45, respectively, but have all been married for the same number of
# years (it does not matter what that number is; 1 year, 5 years, etc). In this
# case, model M2 tells us that as we increase the age, the probability of having
# an affair goes down, i.e. there is a negative effect of age on probability of
# having an affair.



