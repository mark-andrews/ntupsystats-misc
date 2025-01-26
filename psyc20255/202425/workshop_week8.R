library(tidyverse)
library(modelr)
library(psyntur)

npi_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/npi.csv")

# make a scatterplot
scatterplot(x = age, 
            y = narcissism, 
            data = npi_df, 
            best_fit_line = TRUE,
            by = gender)

# general linear model
M_8_1 <- lm(narcissism ~ age + gender, data = npi_df)

summary(M_8_1)

# re-scale age 
M_8_2 <- lm(narcissism ~ I(age - mean(age)) + gender, data = npi_df)
summary(M_8_2)

# what is the predicted avg narcissism score of a female aged 20?

b <- coef(M_8_1)
b[1] + (b[2] * 20) + (b[3] * 0)

# what is the predicted avg narcissism score of a male aged 20?
b[1] + (b[2] * 20) + (b[3] * 1)

# what is the predicted avg narcissism score of a female aged 50?
b[1] + (b[2] * 50) + (b[3] * 0)

# what is the predicted avg narcissism score of a male aged 50?
b[1] + (b[2] * 50) + (b[3] * 1)


# same predictions using add_predictions

npi_df2 <- tibble(gender = c("female", "male", "female", "male"),
                  age = c(20, 20, 50, 50))

add_predictions(npi_df2, M_8_1)

# test significance of categorical variable: ANCOVA
M_8_3 <- lm(narcissism ~ age, data = npi_df)
anova(M_8_3, M_8_1)
