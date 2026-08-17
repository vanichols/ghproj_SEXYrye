#--created 6 aug 2026
#--do stats on grain yields
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

# 1. yield data ---------------------------------------------------------

f1 <- 
  sexy1_harvest |> 
  mutate(plot = as.character(plot)) |> 
  filter(data_type == "straw_Mgha")


f <- 
  f1 |> 
  fxn_SeparateTrt()


# 1. by crop and herbicide bc cover crop is not a thing during the season -------------------------------------------------------------------

m1 <- glmmTMB(value ~ crop*herb + 
                (1 | block), 
              REML = T, 
              data = f)

sim_rest1 <- simulateResiduals(m1)
plot(sim_rest1)

Anova(m1)

#--letters
em1 <- emmeans(m1, specs = ~crop*herb)

cld1 <- 
  multcomp::cld(em1, Letters = letters,  decreasing = TRUE) |> 
  as_tibble() |> 
  mutate(sig_letter = str_squish(.group),
         data_type = "straw") 

cld1 |>
  dplyr::select(data_type, everything(), -.group) |> 
  write_csv("data/stats/figs_emmeans/emmeans_straw.csv")


