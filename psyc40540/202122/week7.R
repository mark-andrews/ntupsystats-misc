library(tidyverse)
library(modelr)

# convert `dose` from numeric to categorical
ToothGrowth <- mutate(ToothGrowth, dose = factor(dose))

# general linear model
# with outcome `len` and predictor `supp`
M_1 <- lm(len ~ supp, data = ToothGrowth)
summary(M_1)$coefficients

estimates <- coef(M_1)
# mean of outcome when supp = OJ
estimates[1] + estimates[2] * 0

# mean of outcome when supp = VC
estimates[1] + estimates[2] * 1

# same as above, but using `add_predictions` from `modelr`
new_df <- tibble(supp = c('OJ', 'VC'))
add_predictions(new_df, M_1)

# show all results to six significant figures
options(pillar.sigfig = 6)

# general linear model
# with outcome `len` and predictor `dose`
M_2 <- lm(len ~ dose, data = ToothGrowth)
summary(M_2)$coefficients


new_df2 <- tibble(dose = c('0.5', '1', '2'))
add_predictions(new_df2, M_2)
