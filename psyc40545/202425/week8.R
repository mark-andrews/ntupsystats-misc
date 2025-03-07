library(lme4)
# random slope, random intercept, and correlation
M_8_1 <- lmer(Reaction ~ Days + (Days|Subject), data=sleepstudy)
# equivalent to
M_8_1a <- lmer(Reaction ~ 1 + Days + (1 + Days|Subject), data=sleepstudy)

# random intercept only
M_8_2 <- lmer(Reaction ~ Days + (1|Subject), data=sleepstudy)

# simple linear regression
M_8_3 <- lm(Reaction ~ Days, data = sleepstudy)

# simple linear regression
M_8_4 <- lm(Reaction ~ 1 + Days, data = sleepstudy)

# simple linear regression with no intercept
M_8_5 <- lm(Reaction ~ 0 + Days, data = sleepstudy)


# Model comparison --------------------------------------------------------

anova(M_8_2, M_8_1)
logLik(M_8_1) # note this is different to loglik in the anova output
deviance(M_8_1) # won't work at all

M_8_1_ml <- lmer(Reaction ~ Days + (Days|Subject), 
                 REML = FALSE,
                 data=sleepstudy)
logLik(M_8_1_ml) 
deviance(M_8_1_ml) 

# random slopes only
M_8_6 <- lmer(Reaction ~ Days + (0 + Days|Subject), data=sleepstudy)

anova(M_8_6, M_8_1)

# random slopes, random intercepts, but no correlation
M_8_7 <- lmer(Reaction ~ Days + (1|Subject) + (0 + Days|Subject), data=sleepstudy)
M_8_8 <- lmer(Reaction ~ Days + (Days||Subject), data=sleepstudy)

anova(M_8_8, M_8_1)

classroom_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/classroom.csv")
science_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/science.csv")


M_8_9 <- lmer(mathscore ~ ses + (ses|schoolid), data = classroom_df)
M_8_10 <- lmer(mathscore ~ ses + (ses|schoolid) + (1|classid), 
               data = classroom_df)

anova(M_8_10, M_8_9)

science_df <- drop_na(science_df) # drop missing values
M_8_11 <- lmer(like ~ PrivPub + sex + (1|school), data = science_df)
M_8_12 <- lmer(like ~ PrivPub + (1|school), data = science_df)

anova(M_8_12, M_8_11)

