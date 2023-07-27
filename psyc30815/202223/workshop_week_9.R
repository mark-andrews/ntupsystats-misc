# Multilevel model --------------------------------------------------------

#library(lme4)
library(lmerTest)
library(tidyverse)

# Varying slope and varying intercept and possible correlations thereof
M_9_1 <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)
#M_9_1b <- lmer(Reaction ~ 1 + Days + (1 + Days|Subject), data = sleepstudy)
summary(M_9_1)

coef(M_9_1)


# Varying intercept only
M_9_2 <- lmer(Reaction ~ Days + (1|Subject), data = sleepstudy)
summary(M_9_2)

# Varying slope only
M_9_3 <- lmer(Reaction ~ Days + (0 + Days|Subject), data = sleepstudy)
summary(M_9_3)


# Varying intercept and varying slope, but no correlation between random slopes and intercepts
M_9_4 <- lmer(Reaction ~ Days + (1|Subject) + (0 + Days|Subject), 
              data = sleepstudy)
M_9_4b <- lmer(Reaction ~ Days + (Days || Subject), data = sleepstudy)
# compare with
# lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)

summary(M_9_4)
summary(M_9_4b)

# ML versus REML ----------------------------------------------------------

M_9_1_reml <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)
M_9_1_ml <- lmer(Reaction ~ Days + (Days|Subject), REML = FALSE, data = sleepstudy)

# Model comparison of the four models -------------------------------------

anova(M_9_1, M_9_2, test = 'Chisq') 
anova(M_9_1, M_9_2, M_9_3, M_9_4, test = 'Chisq')

anova(M_9_1, M_9_3, test = 'Chisq')
anova(M_9_1, M_9_4, test = 'Chisq')

anova(M_9_2, M_9_3, test = 'Chisq')


# Load in new data sets ---------------------------------------------------

classroom_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr06/main/data/classroom.csv")
blp_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/immr06/main/data/blp-short2.csv")

# look at the number of schools 
table(classroom_df$schoolid)
table(classroom_df$classid)


# Varying slope and varying intercept linear model ------------------------

M_9_5 <- lmer(mathscore ~ ses + (ses|schoolid), data = classroom_df)
summary(M_9_5)

M_9_6 <- lmer(mathscore ~ ses + (ses|schoolid) + (ses|classid), 
              data = classroom_df)
summary(M_9_6)

M_9_7 <- lmer(mathscore ~ ses + (ses|schoolid) + (ses||classid), 
              data = classroom_df)

M_9_8 <- lmer(mathscore ~ ses + (ses|schoolid) + (1|classid), 
              data = classroom_df)

# Crossed structures ------------------------------------------------------

blp_df <- mutate(blp_df, freq = scale(freq)[,1])

M_9_9 <- lmer(log(rt) ~ freq + (freq|participant) + (1|spelling),
              data = blp_df)

summary(M_9_9)     
