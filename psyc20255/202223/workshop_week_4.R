# workshop on evaluating models


# Load packages -----------------------------------------------------------

library(tidyverse)
library(psyntur)

# Load data ---------------------------------------------------------------

rwas_df <- read_csv("http://data.ntupsychology.net/rwas.csv")

# look at the data
glimpse(rwas_df)


# Multiple regression -----------------------------------------------------

# predicting `rightwing` from `extraverted`, `anxious`, `critical`, `dependable`, `open`

M_4_1 <- lm(rightwing ~ extraverted + anxious + critical + open + dependable,
            data = rwas_df)
summary(M_4_1)


# Model diagnostics -------------------------------------------------------

# histogram of residuals
M_4_1_resid <- tibble(residuals = residuals(M_4_1))
histogram(residuals, data = M_4_1_resid, bins = 50)

# QQ plot
plot(M_4_1, which = 2)
# residual veruss fitted
plot(M_4_1, which = 1)
# scale location plots
plot(M_4_1, which = 3)



# Overall model fit -------------------------------------------------------

summary(M_4_1)$r.sq
