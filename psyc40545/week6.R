library(mixtools)
library(tidyverse)

ggplot(faithful,
       aes(x = eruptions, y = waiting)
) + geom_point()

ggplot(faithful,
       aes(x = eruptions)
) + geom_histogram(binwidth = 0.1, 
                   colour = 'white')

eruptions <- pull(faithful, eruptions)
M_faithful <- normalmixEM(eruptions, k = 2)

M_faithful$mu
M_faithful$sigma
M_faithful$lambda
M_faithful$posterior
round(head(M_faithful$posterior), 2)
M_faithful$posterior[seq(1, 272, by = 10),]

data(CO2data)
y <- CO2data$CO2
x <- CO2data$GNP

M_reg <- regmixEM(y, x, k = 2)
M_reg <- regmixEM(y, x, k =2)

M_reg$beta
set.seed(10101)
