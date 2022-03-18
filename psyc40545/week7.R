library(tidyverse)
affairs_df <- read_csv("http://data.ntupsychology.net/affairs.csv")

table(affairs_df$affairs)
# sample from a Poiss with mean 1.45
# calc prop of zeros
mean(rpois(1000, lambda = 1.45) == 0)

# what proportion of our data is zero?
mean(affairs_df$affairs == 0)

# Let's a Poisson regression
# outcome: affairs
# predictor: yearsmarried

Mp <- glm(affairs ~ yearsmarried, 
          data = affairs_df,
          family = poisson(link = 'log'))

summary(Mp)$coefficients
confint(Mp)

library(pscl)
Mzip <- zeroinfl(affairs ~ yearsmarried, 
                 data = affairs_df)

summary(Mzip)

# create a data frame with range of values of yearsmarried
affairs_df_new <- tibble(yearsmarried = seq(1, 25, by = 5))

library(modelr)
# predicted probability that the latent variable is 1
# predicted probability that the data is from the zero component 
add_predictions(affairs_df_new, Mzip, type = 'zero')

# predicted avg number of affairs as function of yearsmarried
# for individuals who do have affairs (non-monogamous types)
add_predictions(affairs_df_new, Mzip, type = 'count')

# predicted avg number of affairs as function of yearsmarried
# for BOTH those individuals who do and do not have affairs
add_predictions(affairs_df_new, Mzip, type = 'response')

vuong(Mp, Mzip)
