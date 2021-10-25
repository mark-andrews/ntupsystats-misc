
# load up packages --------------------------------------------------------

library(tidyverse)
library(psyntur)


# Get some demo data ------------------------------------------------------

data_df <- read_csv("http://data.ntupsychology.net/psychometrics_demo_data.csv")

glimpse(data_df)


# Recode negatively coded items -------------------------------------------

# e.g. anxiety_6

# use `re_code` (recode)

data_df_fix <- mutate(data_df,
                      anxiety_6 = re_code(anxiety_6, 0:4, 4:0),
                      anxiety_7 = re_code(anxiety_7, 0:4, 4:0),
                      anxiety_8 = re_code(anxiety_8, 0:4, 4:0),
                      anxiety_9 = re_code(anxiety_9, 0:4, 4:0),
                      anxiety_10 = re_code(anxiety_10, 0:4, 4:0),
                      depression_8 = re_code(depression_8, 1:5, 5:1),
                      depression_9 = re_code(depression_9, 1:5, 5:1),
                      depression_10 = re_code(depression_10, 1:5, 5:1),
                      efficacy_7 = re_code(efficacy_7, 1:5, 5:1),
                      efficacy_8 = re_code(efficacy_8, 1:5, 5:1),
                      efficacy_9 = re_code(efficacy_9, 1:5, 5:1),
                      efficacy_10 = re_code(efficacy_10, 1:5, 5:1),
                      sociability_4 = re_code(sociability_4, 1:5, 5:1),
                      sociability_5 = re_code(sociability_5, 1:5, 5:1),
                      sociability_6 = re_code(sociability_6, 1:5, 5:1),
                      sociability_7 = re_code(sociability_7, 1:5, 5:1),
                      sociability_8 = re_code(sociability_8, 1:5, 5:1),
                      sociability_9 = re_code(sociability_9, 1:5, 5:1), 
                      sociability_10 = re_code(sociability_10, 1:5, 5:1),
                      stress_4 = re_code(stress_4, 0:4, 4:0),
                      stress_5 = re_code(stress_5, 0:4, 4:0),
                      stress_7 = re_code(stress_7, 0:4, 4:0),
                      stress_8 = re_code(stress_8, 0:4, 4:0)
)



# Cronbach's alpha --------------------------------------------------------

cronbach(data_df_fix,
         anxiety = starts_with('anxiety_'),
         depression = starts_with('depression_'),
         efficacy = starts_with('efficacy_')
)



# aggregate scorees -------------------------------------------------------


# based on mean
total_scores(data_df_fix,
             anxiety = starts_with('anxiety_'),
             depression = starts_with('depression_')
)

total_scores(data_df_fix,
             anxiety = starts_with('anxiety_'),
             depression = starts_with('depression_'),
             .method = 'sum'
)

