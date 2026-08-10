#--created 6 aug 2026
#--do stats on total fall biomass taken at end of season
#--make emmeans summaries to make figure

library(tidyverse)
library(prye)

library(lme4)
library(lmerTest)
library(broom)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(car)
library(performance)
library(multcomp)

rm(list = ls())

# 1. fallbio data ---------------------------------------------------------

f1 <- 
  sexy1_fallbio |> 
  mutate(plot = as.character(plot))

#--get last biomass sampling
f2 <- 
  f1 |> 
  ungroup() |> 
  filter(sampledate_ymd2 == max(sampledate_ymd2)) 


# 2. weeds -------------------------------------------------------------------

w <- 
  f2 |> 
  filter(biomass_cat == "weeds")

w |> 
  ggplot(aes(block, value)) +
  geom_col() +
  facet_grid(.~trt_name)


# weeds model -------------------------------------------------------------------

m2 <- glmmTMB(value ~ trt_name + 
                (1 | block), 
              REML = T, 
              data = w)

summary(m2)
sim_rest2 <- simulateResiduals(m2)
plot(sim_rest2)

#--letters
em2 <- emmeans(m2, specs = ~trt_name)

cld2 <- 
  multcomp::cld(em2, Letters = letters) |> 
  as_tibble() |> 
  mutate(sig_letter = str_squish(.group),
         data_type = "fallbio - weeds")

cld2 |> 
  dplyr::select(data_type, everything(), -.group) |> 
  write_csv("data/stats/figs_emmeans/emmeans_fallbio-weeds.csv")



#--more complicated model including herbicdie, cover crop, crop

w2 <- 
  w %>%
  fxn_SeparateTrt(.)

w2 |> write_csv("data/tidy_fallbio-last.csv")

m3 <- glmmTMB(value ~ herb * cctrt * crop + 
                (1 | block), 
              REML = T, 
              data = w2)

summary(m3)
sim_rest3 <- simulateResiduals(m3)
plot(sim_rest3)

Anova(m3)

em1 <- emmeans(m3, specs = ~herb)
pairs(em1)

em3 <- emmeans(m3, specs = ~herb|cctrt)
pairs(em3)
