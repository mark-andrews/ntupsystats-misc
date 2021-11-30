library(tidyverse)
library(psyntur)
library(moments)
library(ez) # optional
library(emmeans)

# Use ToothGrowth; make dose categorical
ToothGrowth$dose <- factor(ToothGrowth$dose)

# make a boxplot
tukeyboxplot(y = len, 
             x = dose, 
             by = supp, 
             data = ToothGrowth)

# calculate the mean, sd, skewness, kurtosis
# of each combo of the two IVs
describe(data = ToothGrowth,
         by = c(dose, supp),
         avg = mean(len),
         stdev = sd(len),
         skew = skewness(len),
         kurtosis = kurtosis(len))


tukeyboxplot(data = selfesteem2_long,
             y = score,
             x = time, 
             jitter = TRUE,
             by = treatment)


describe(data = selfesteem2_long,
         by = c(time, treatment),
         avg = mean(score),
         stdev = sd(score),
         skew = skewness(score),
         kurtosis = kurtosis(score))

# Do a two by two within subjects anova
# using ez_anova

ez_anova(data = selfesteem2_long,
         dv = score, 
         wid = id,
         within = c(time, treatment),
         type = 3,
         detailed = TRUE,
         return_aov = TRUE)
