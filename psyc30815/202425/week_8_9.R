library(tidyverse)
library(lme4)
# sleepstudy

# facet plot of all 18 subjects
ggplot(sleepstudy, 
       aes(x = Days, y = Reaction, colour = Subject)) + 
  geom_point() + 
  geom_smooth(method='lm', se = FALSE) +
  facet_wrap(~Subject) +
  theme_minimal() + 
  theme(legend.position = 'none') 
  
# collapsed across all 18; aka pooled analysis
ggplot(sleepstudy, 
       aes(x = Days, y = Reaction)) + 
  geom_point() + 
  geom_smooth(method='lm', se = FALSE) +
  theme_minimal() 
  

# lmer model  -------------------------------------------------------------

M_8_1 <- lmer(Reaction ~ Days + (Days|Subject), 
              data = sleepstudy)
summary(M_8_1)

