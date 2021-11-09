library(tidyverse)
library(psyntur)

# look at the first few rows
head(ToothGrowth, 10)

tukeyboxplot(y = len, x = supp,
             jitter = TRUE,
             data = ToothGrowth)

describe(ToothGrowth,
         by = supp,
         average = mean(len),
         stdev = sd(len))

t_test(len ~ supp, data = ToothGrowth)


tukeyboxplot(y = len, x = dose,
             jitter = TRUE,
             data = ToothGrowth)

describe(ToothGrowth,
         average = mean(len),
         stdev = sd(len),
         by = dose)

oneway <- aov(len ~ factor(dose), data = ToothGrowth)

summary(oneway)
