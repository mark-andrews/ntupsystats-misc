library(tidyverse)
library(psyntur)

head(ToothGrowth)

tukeyboxplot(y = len,
             x = dose,
             by = supp,
             data = ToothGrowth)

# factorial anova
M <- aov(len ~ supp * factor(dose), 
         data = ToothGrowth)

summary(M)

M1 <- aov(score ~ treatment * time + Error(id/(treatment*time)),
          data = selfesteem2_long)


