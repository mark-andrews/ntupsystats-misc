library(lme4)
library(tidyverse)

sleepstudy_df <- tibble(sleepstudy)


# Subject 308 -------------------------------------------------------------

subject308_df <- filter(sleepstudy_df, Subject == "308")

M_7_1 <- lm(Reaction ~ 1, data = subject308_df)
coef(M_7_1) # estimated mean
sigma(M_7_1) # estimated SD

# Subject 309 -------------------------------------------------------------

subject309_df <- filter(sleepstudy_df, Subject == "309")

M_7_2 <- lm(Reaction ~ 1, data = subject309_df)
coef(M_7_2) # estimated mean
sigma(M_7_2) # estimated SD


# Multilevel normal model -------------------------------------------------

M_7_3 <- lmer(Reaction ~ 1 + (1|Subject), data = sleepstudy_df)
summary(M_7_3)

# estimate of the mu's
coef(M_7_3)


# Multilevel linear regression models -------------------------------------

ggplot(sleepstudy_df,
       aes(x = Days, y = Reaction, colour = Subject)
) + geom_point() + 
  stat_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~Subject) 


M_7_4 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy_df)
M_7_9 <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy_df)
# same as M_7_4
M_7_4a <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), data = sleepstudy_df)

#lm(Reaction ~ 0 + Days, data = sleepstudy_df)

summary(M_7_4)

c(10.467 - qnorm(0.975) * 5.922, 10.467 + qnorm(0.975) * 5.922)

10.467 + c(-1, 1) * qnorm(0.975) * 5.922# shorter version of above

10.467 + c(-1, 1) * qnorm(0.995) * 5.922 # 99% interval

c(10.467 - qnorm(0.995) * 5.922, 
  10.467 + qnorm(0.995) * 5.922)


# Week 8 ------------------------------------------------------------------

# same as M_7_4
M_7_5 <- lmer(Reaction ~ 1 + Days + (1 | Subject), data = sleepstudy_df)

summary(M_7_5)

deviance(M_7_4) # -2 log likelihood of M_7_4

# Random intercepts & random slopes: varying intercepts & slopes and a model of the variability
M_7_4b <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), 
               REML = FALSE,
               data = sleepstudy_df)

# Random intercepts: varying intercepts and a model of the variability
M_7_5b <- lmer(Reaction ~ 1 + Days + (1 | Subject), 
              REML = FALSE,
              data = sleepstudy_df)

# Random slopes only: varying slopes and a model of the variability
M_7_6b <- lmer(Reaction ~ 1 + Days + (0 + Days | Subject),
               REML = FALSE,
               data = sleepstudy_df)


# Random intercepts & slopes only: varying slopes and a model of the variability
# but NO correlation
M_7_7b <- lmer(Reaction ~ 1 + Days + (1 + Days || Subject),
               REML = FALSE,
               data = sleepstudy_df)


anova(M_7_5b, M_7_4b)

anova(M_7_6b, M_7_4b)

anova(M_7_7b, M_7_4b)


science_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/science.csv")

M_7_8 <- lmer(like ~ 1 + PrivPub + (1 |school), data = science_df)

library(lmerTest)


M_7_8 <- lmer(like ~ 1 + PrivPub + (1 |school), data = science_df)

#M_7_9 <- lmer(like ~ 1 + PrivPub + (1 |school) + (1|Class), data = science_df)

classroom_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/ntupsychology-data/main/data-sets/classroom.csv")

M_7_9 <- lmer(mathscore ~ ses + (ses|schoolid), data = classroom_df)
summary(M_7_9)

M_7_10 <- lmer(mathscore ~ ses + (ses|schoolid) + (ses|classid), 
               data = classroom_df)


M_7_11 <- lmer(mathscore ~ ses + (ses|schoolid) + (1|classid), 
               data = classroom_df)


anova(M_7_11, M_7_10)
