
# Load packages -----------------------------------------------------------

library(tidyverse)
library(psyntur)


# Set some global settings ------------------------------------------------

# set the scipen to show more decimal numbers
options(scipen = 50)

# Read in the data --------------------------------------------------------

happy_df <- read_csv("http://data.ntupsychology.net/happy2019.csv")


# Multiple regression analysis --------------------------------------------

M_3_1 <- lm(happiness ~ gdp + support + hle + freedom + generosity + corrupt, data = happy_df)

# get the summary of the main results
summary(M_3_1)

# get the 95% confidence interval for the coefficients
confint(M_3_1)

# Predictions in multiple regression --------------------------------------

hypothetical_country <- tibble(gdp = 2.0,
                               support = 1.0,
                               hle = 1.0,
                               freedom = 1.0,
                               generosity = 1.0,
                               corrupt = 0.0)

predict(M_3_1, newdata = hypothetical_country)
