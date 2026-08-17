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

source("code/utils.R")

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
  summarise(.by = c(field_id, sea_name, trt_name, block, plot, sampledate_ymd2, data_type),
            value = sum(value))

#--double check it
f3 |> 
  ggplot(aes(block, value)) +
  geom_col() +
  facet_grid(.~trt_name)

f <- 
  f3 %>%
  fxn_SeparateTrt(.)


f |> 
  filter(trt_name %in% c("p", "pcc")) |> 
  group_by(trt_name) |> 
  summarise(value = mean(value))

# 1. by treatment -------------------------------------------------------------------

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
  multcomp::cld(em1, Letters = letters, reverse = TRUE) |> 
  as_tibble() |> 
  mutate(sig_letter = str_squish(.group),
         data_type = "fallbio") 

cld1 |>
  dplyr::select(data_type, everything(), -.group) |> 
  write_csv("data/stats/figs_emmeans/emmeans_fallbio.csv")


# 2. by herb, cctrt, crop -------------------------------------------------------------------

m2 <- glmmTMB(value ~ herb * cctrt * crop + 
                (1 | block), 
              REML = T, 
              data = f)

sim_rest2 <- simulateResiduals(m2)
plot(sim_rest2)


# cctrt*crop interaction
# herb is not sig
Anova(m2)

em_all <- emmeans(m2, specs = ~ herb)

#--the reduction in biomass from NOT using a cover crop is 3x larger in a compared to p
em2 <- emmeans(m2, specs = ~ cctrt|crop)
em2

pairs(em2)

#--the difference between a and p is only sig when using a cover crop
em3 <- emmeans(m2, specs = ~ crop|cctrt)
em3

pairs(em3)

em4 <- emmeans(m2, specs = ~ crop*cctrt)

cld2 <- 
  multcomp::cld(em4, Letters = letters, reversed = TRUE) |> 
  as_tibble() |> 
  mutate(sig_letter = str_squish(.group),
         data_type = "fallbio") 


#--a+cc is highest, all others (p, pcc, a) are the 'same'
cld2

cld2 |>
  dplyr::select(data_type, everything(), -.group) |> 
  write_csv("data/stats/figs_emmeans/emmeans_fallbio-cropxcctrt.csv")

