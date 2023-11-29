library(tidyverse)
library(psyntur)

pain_df <- read_csv('pain.csv')

describe(pain_df, 
         avg = mean(time),
         sd = sd(time),
         by = group)

M_3_1 <- t.test(time ~ group, data = pain_df, var.equal = TRUE)

