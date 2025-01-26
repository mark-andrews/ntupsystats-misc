library(tidyverse)

psymetr_total <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/psymetr_df_total.csv")

M_3_1 <- lm(stress ~ anxiety + depression + efficacy + sociability, data = psymetr_total)
summary(M_3_1)
confint(M_3_1)

new_data <- tibble(anxiety = 2, depression = 2, efficacy = 3, sociability = 3)
predict(M_3_1, newdata = new_data)
library(modelr)
add_predictions(new_data, M_3_1)

new_data2 <- tibble(anxiety = 5, depression = 2, efficacy = 3, sociability = 3)
add_predictions(new_data2, M_3_1)
