library(tidyverse)
library(psyntur)


# Get some data -----------------------------------------------------------

anorexia_df <- read_csv("http://data.ntupsychology.net/anorexia.csv")

# scatterplot of this data
scatterplot(x = Prewt, 
            y = Postwt, 
            by = Treat,
            best_fit_line = TRUE,
            data = anorexia_df)


# Four different linear models --------------------------------------------

# Model 1: Postwt varies by Prewt score only
M_8_1 <- lm(Postwt ~ Prewt, data = anorexia_df)

# Model 2: Postwt varies by Treat only
M_8_2 <- lm(Postwt ~ Treat, data = anorexia_df)

# Model 3: Postwt varies by Prewt, with a different intercept for each Treat 
# this is known as a varying intercept linear model
# this is an ANCOVA
M_8_3 <- lm(Postwt ~ Prewt + Treat, data = anorexia_df)

# Model 4: Postwt varies by Prewt, & different intercepts AND slopes for each Treat
# this is varying slope and varying intercept linear model
M_8_4 <- lm(Postwt ~ Prewt * Treat, data = anorexia_df)
M_8_4b <- lm(Postwt ~ Prewt + Treat + Prewt:Treat, data = anorexia_df)



# Look at the R^2 ---------------------------------------------------------

summary(M_8_1)$r.sq
summary(M_8_2)$r.sq
summary(M_8_3)$r.sq
summary(M_8_4)$r.sq


# Interpret the coefficients for each model
summary(M_8_1)$coefficients
# If a person had a Prewt score of 75, what is the predicted Postwt?
42.7005802 + 0.5153804 * 75
betas <- coef(M_8_1)
betas[1] + betas[2] * 75
betas['(Intercept)'] + betas['Prewt'] * 75

# Interpret coefficient of model M_8_2
summary(M_8_2)$coefficients

# What is the predicted value of Postwt when Treat = `FT`?
b <- coef(M_8_2)
b['(Intercept)'] + b['TreatCont'] * 0 + b['TreatFT'] * 1
# or just ....
b['(Intercept)'] + b['TreatFT'] 

# Interpret coefficnets of model M_8_3
summary(M_8_3)$coefficients

# What is predicted value of Postwt if Prewt is 75 and Treat = `Cont`?
b <- coef(M_8_3)
(b['(Intercept)'] + b['TreatCont']) + b['Prewt'] * 75


# check your work...
new_df <- tibble(Prewt = 75, Treat = 'Cont')
predict(M_8_3, newdata = new_df)


# interpret coefficients of M_8_4
summary(M_8_4)$coefficients
# What is predicted value of Postwt if Prewt is 75 and Treat = `Cont`?
b <- coef(M_8_4)

(b['(Intercept)'] + b['TreatCont']) + (b['Prewt'] + b['Prewt:TreatCont']) * 75

# check your work...
predict(M_8_4, newdata = new_df)


# Check for an interaction, or equivalently if slopes vary by Treat
anova(M_8_3, M_8_4)

# What is predicted value of Postwt if Prewt is 65 and Treat = `FT`?
new_df <- tibble(Prewt = 65, Treat = 'FT')
predict(M_8_4, newdata = new_df)
