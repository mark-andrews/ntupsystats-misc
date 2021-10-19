library(tidyverse)
library(modelr)

weight_df <- read_csv("http://data.ntupsychology.net/weight.csv")

weight_male_df <- read_csv("http://data.ntupsychology.net/weight.csv") %>% 
  filter(gender == 'male')

M1 <- lm(weight ~ height + age, data = weight_male_df)

confint(M1)


# predictions -------------------------------------------------------------

hypo_df <- tibble(height = 185, age = 45)

predict(M1, newdata = hypo_df)

add_predictions(hypo_df, M1)

estimates <- coef(M1)
estimates[1] + estimates[2] * 185 + estimates[3] * 45

predict(M1, newdata = hypo_df, interval = 'confidence')

hypo_df_2 <- tibble(height = seq(150, 200), age = 45)

predict(M1, newdata = hypo_df_2)

pred_df <- add_predictions(hypo_df_2, M1)

# make a plot of predicted means
ggplot(pred_df, 
       aes(x = height, y = pred)) + geom_line()


# model weight as a function of height and age and gender -----------------

M2 <- lm(weight ~ height + age + gender, 
         data = weight_df)

summary(M2)$coefficients

estimates <- coef(M2)

# what is the predicted mean for a
# gender = female, age = 35, height = 160

estimates[1] + estimates[2] * 160 + estimates[3] * 35 + estimates[4] * 0


# what is the predicted mean for a
# gender = male, age = 35, height = 160

estimates[1] + estimates[2] * 160 + estimates[3] * 35 + estimates[4] * 1




hypo_df_3 <- expand_grid(gender = c("male", "female"),
                         height = seq(150, 200),
                         age = 35)

pred_df_2 <- add_predictions(hypo_df_3, M2)

# to get the `pred`, we can also do this.....
predict(M2, newdata = hypo_df_3)

# and we can use `predict` to get prediction confidence intervals
predict(M2, newdata = hypo_df_3, interval = 'confidence')

ggplot(pred_df_2,
       aes(x = height, y = pred, colour = gender)
) + geom_line()

# prediction confidence interval



# interaction model, i.e. varying slope, varying intercept ----------------


M3 <- lm(weight ~ height * gender + age, 
         data = weight_df)

summary(M3)$coefficients

pred_df_3 <- add_predictions(hypo_df_3, M3)

ggplot(pred_df_3,
       aes(x = height, y = pred, colour = gender)
) + geom_line()



# filter the data to three races only -------------------------------------

weight_df2 <- filter(weight_df,
                     race %in% c('white', 'black', 'hispanic')
)

# M4 <- lm(weight ~ height + gender, 
#          data = weight_df2)

M4 <- lm(weight ~ height + race, data = weight_df2)
# weight ~ race

summary(M4)$coefficients

# varying intercept and varying slope model
M5 <- lm(weight ~ height * race, data = weight_df2)
#M5 <- lm(weight ~ height + race + height:race, data = weight_df2)
summary(M5)$coefficients

# look at their R squares
summary(M4)$r.squared
summary(M5)$r.squared

anova(M4, M5) # nested model comparison function

# just for fun, calculate RSS of M4 and M5
sum(residuals(M4)^2)
sum(residuals(M5)^2)


