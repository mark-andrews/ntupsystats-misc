
# Load packages -----------------------------------------------------------

library(tidyverse)
library(psyntur)


# Load the data -----------------------------------------------------------

happy_df <- read_csv("http://data.ntupsychology.net/happy2019.csv")

# Visualization -----------------------------------------------------------

histogram(happiness, data = happy_df)

# create new variables 
happy_df <- mutate(happy_df, 
                   gdp_tercile = ntile(gdp, 3),
                   generosity_tercile = ntile(generosity, 3)
)


histogram(happiness, data = happy_df, 
          facet = c(gdp_tercile, generosity_tercile))


# Analysis ----------------------------------------------------------------

# multiple linear regression predicting happiness from gdp and generosity 
M_2_1 <- lm(happiness ~ gdp + generosity, data = happy_df)

# look at the summary of the results
summary(M_2_1)

# focus in on the coefficients
coef(M_2_1)[-1]
