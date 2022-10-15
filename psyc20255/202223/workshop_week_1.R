
# Load up packages --------------------------------------------------------

library(tidyverse)
library(psyntur)


# Make a scatterplot ------------------------------------------------------

scatterplot(attractive, faithful, data = faithfulfaces)

# put in the line of best fit of the scatterplot
scatterplot(attractive, faithful, 
            data = faithfulfaces, 
            best_fit_line = TRUE)
