library(tidyverse)

ToothGrowth
ToothGrowth$dose <- factor(ToothGrowth$dose)
summary(ToothGrowth)

group_by(ToothGrowth, supp, dose) %>% 
  summarise(sample_size = n(),
            the_average = mean(len))

summarise(group_by(ToothGrowth, supp, dose),
          n = n(),
          avg = mean(len))

summarise(ToothGrowth, 
          avg = mean(len),
          n = n())

# oneway anova
M1 <- aov(len ~ dose, data = ToothGrowth)
summary(M1)
anova(M1)

# Total variation
var(ToothGrowth$len) * (nrow(ToothGrowth) - 1)

sum(anova(M1)[['Sum Sq']])

# prob being above f stat in J-1, n-J F dist
pf(67.416, 2, 57, lower.tail = F)


M2 <- aov(len ~ dose * supp, data = ToothGrowth)
summary(M2)
anova(M2)

anova(M2)[['Sum Sq']] %>% sum()

# prob being above f stat for interaction in appropriate F dist
pf(4.107, 2, 54, lower.tail = F)
