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

#--combine biomass_cats (pha, weeds, etc.)
f3 <- 
  f2 |>  
  group_by(field_id, sea_name, trt_name, block, plot, sampledate_ymd2, data_type) |> 
  summarise(value = sum(value))

#--double check it
f3 |> 
  ggplot(aes(block, value)) +
  geom_col() +
  facet_grid(.~trt_name)

f <- f3


# fallbio model -------------------------------------------------------------------

m1 <- glmmTMB(value ~ trt_name + 
                (1 | block), 
              REML = T, 
              data = f)

summary(m1)
sim_rest1 <- simulateResiduals(m1)
plot(sim_rest1)

#--letters
em1 <- emmeans(m1, specs = ~trt_name)

cld1 <- 
  multcomp::cld(em1, Letters = letters) |> 
  as_tibble() |> 
  mutate(sig_letter = str_squish(.group),
         data_type = "fallbio") 

cld1 |>
  dplyr::select(data_type, everything(), -.group) |> 
  write_csv("data/stats/figs_emmeans/emmeans_fallbio.csv")


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



