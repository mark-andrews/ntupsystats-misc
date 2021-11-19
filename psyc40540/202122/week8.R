library(tidyverse)
library(modelr)

# Get the data ------------------------------------------------------------

beauty_df <- read_csv("http://data.ntupsychology.net/beautyeval.csv")


ggplot(beauty_df,
       aes(x = beauty, y = eval, colour = sex)
) + geom_point() + stat_smooth(method = 'lm', se = F)


# linear regression with beauty and sex, and interaction, ----------------
# as predictor --------

# varying intercept only model
M_vi <- lm(eval ~ beauty + sex, 
           data = beauty_df)

coef(M_vi)
estimates <- coef(M_vi)

# what is the predicted mean of `eval` if beauty = 1, and sex = male?
estimates[1] + estimates[2] * 1 + estimates[3] * 1

# what is the predicted mean of `eval` if beauty = 2, and sex = female?
estimates[1] + estimates[2] * 2 + estimates[3] * 0

# what is the predicted mean of `eval` if beauty = 2, and sex = male?
estimates[1] + estimates[2] * 2 + estimates[3] * 1


# Varying slope and intercept model ---------------------------------------

M_vivs <- lm(eval ~ beauty * sex, 
             data = beauty_df)
M_vivs_2 <- lm(eval ~ beauty + sex + beauty:sex, 
               data = beauty_df)

estimates <- coef(M_vivs)
# what is the predicted mean of `eval` if beauty = 1, and sex = male?
estimates[1] + estimates[2] * 1 + estimates[3] * 1 + estimates[4] * 1 * 1

# what is the predicted mean of `eval` if beauty = 2, and sex = female?
estimates[1] + estimates[2] * 2 + estimates[3] * 0 + estimates[4] * 2 * 0

# what is the predicted mean of `eval` if beauty = 2, and sex = male?
estimates[1] + estimates[2] * 2 + estimates[3] * 1 + estimates[4] * 2 * 1

# confirm the above using `modelr::add_predictions`
new_df <- tibble(beauty = c(1, 2, 2),
                 sex = c('male', 'female', 'male'))

modelr::add_predictions(new_df, M_vivs)

# varying slope only, no varying intercept
M_vs <- lm(eval ~ beauty + 1 + beauty:sex, 
           data = beauty_df)
