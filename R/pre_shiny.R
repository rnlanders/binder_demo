# R Studio API
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(psych)

# Dataset Import

# Garcia, D. M., Schmitt, M. T., Branscombe, N. R., & Ellemers, N. (2010). 
# Women's reactions to ingroup members who protest discriminatory treatment: 
# The importance of beliefs about inequality and response appropriateness. 
# European Journal of Social Psychology, 40(5), 733–745.
#
# Abstract
# Our goal was to identify factors that shape women's responses to ingroup 
# members who protest gender discrimination. We predicted and found that women 
# who perceived gender discrimination as pervasive regarded a protest response 
# as being more appropriate than a no protest response and expressed greater 
# liking and less anger towards a female lawyer who protested rather than did 
# not protest an unfair promotion decision.
#
# Method: vingette study, protest is between-subjects experimental IV (3 levels), 
# sexism is moderating beliefs about sexism, DVs are anger towards target, likeability
# of target, appropriateness of response, and unfairnes of the promotion decision

data(GSBE)
working_tbl <- Garcia %>%
  select(-prot2) %>%
  mutate(protest = as.factor(protest))

# Generate Data File Ready for Shiny App

# Let's say the shiny goals are to:
# 1) Let the user see the moderation of IV on DV by sexism
# 2) Let the user choose which DV they are looking at
#
# To do this, we need a dataset that contains all needed variables and nothing
# more, and we need to save it to RDS format, so that we are confident it will be
# imported correctly
#
# Since DVs could be anger, liking, or respappr, we need to generate predicted values for each

anger_pred_vals <- lm(anger ~ protest * sexism, data=working_tbl)$fitted.values
liking_pred_vals <- lm(liking  ~ protest * sexism, data=working_tbl)$fitted.values
resp_pred_values <- lm(respappr  ~ protest * sexism, data=working_tbl)$fitted.values

for_shiny_tbl <- working_tbl %>%
  transmute(
    protest,
    sexism,
    anger_pred_vals,
    liking_pred_vals,
    resp_pred_values
  )
  
saveRDS(for_shiny_tbl, "../shiny/for_shiny.rds")

# Generate Prototype of Figures that Shiny Needs to Draw

ggplot(for_shiny_tbl, aes(x=sexism, y=anger_pred_vals, group=protest)) +  # y could be any of the three
  geom_smooth(method="lm", se=F) +
  labs(x="Sexism", y="Anger")