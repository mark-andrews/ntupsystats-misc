library(tidyverse)
library(psyntur)
library(modelr)

anorexia <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/anorexia.csv")

scatterplot(x = Prewt, 
            y = Postwt, 
            #by = Treat, 
            best_fit_line = T,
            data = anorexia)


# simple linear regression; one continuous predictor
M_8_1 <- lm(Postwt ~ Prewt, data = anorexia)            
summary(M_8_1)

# GLM identical to a one way Anova
M_8_2 <- lm(Postwt ~ Treat, data = anorexia)
summary(M_8_2)

# GLM with two predictor, one continuous, one categorical
# this is an ANCOVA
M_8_3 <- lm(Postwt ~ Prewt + Treat, data = anorexia)
summary(M_8_3)

# If Prewt is 80 and Treat = CBT, what is predicted value of Postwt?
b <- coef(M_8_3)

b[1] + b[2] * 80 + b[3] * 0 + b[4] * 0

# If Prewt is 80 and Treat = Cont, what is predicted value of Postwt?
b[1] + b[2] * 80 + b[3] * 1 + b[4] * 0

# If Prewt is 80 and Treat = FT, what is predicted value of Postwt?
b[1] + b[2] * 80 + b[3] * 0 + b[4] * 1

(b[1] + b[3] * 0 + b[4] * 1) + b[2] * 80 # FT 
(b[1] + b[3] * 1 + b[4] * 0) + b[2] * 80 # Cont
(b[1] + b[3] * 0 + b[4] * 0) + b[2] * 80 # CBT


anova(M_8_1, M_8_3)

M_8_4 <- lm(Postwt ~ Prewt * Treat, data = anorexia)
M_8_4a <- lm(Postwt ~ Prewt + Treat + Prewt:Treat, data = anorexia)


summary(M_8_4)$coefficients

# If Prewt is 80 and Treat = FT, what is predicted value of Postwt?
b <- coef(M_8_4)
b[1] + (b[2] * 80) + (b[3] * 0) + (b[4] * 1) + (b[5] * 80 * 0) + (b[6] * 80 * 1)

anorexia2 <- tibble(Prewt = 80,
                    Treat = c('CBT', 'Cont', 'FT')
)

options(pillar.sigfig = 5)
add_predictions(anorexia2, M_8_4)


# If Prewt is 80 and Treat = CBT, what is predicted value of Postwt?
b[1] + (b[2] * 80) + (b[3] * 0) + (b[4] * 0) + (b[5] * 80 * 0) + (b[6] * 80 * 0)

# If Prewt is 80 and Treat = Cont, what is predicted value of Postwt?
b[1] + (b[2] * 80) + (b[3] * 1) + (b[4] * 0) + (b[5] * 80 * 1) + (b[6] * 80 * 0)

# is there a significant interaction?
anova(M_8_3, M_8_4)

b[1] + (b[2] * 80) + (b[3] * 1) + (b[4] * 0) + (b[5] * 80 * 1) + (b[6] * 80 * 0)
(b[1] + (b[3] * 1) + (b[4] * 0)) + (b[2] + b[5]) * 80 
