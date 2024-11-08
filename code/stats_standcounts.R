# created: 8 Nov 2024
# purpose: analyze stand counts
# notes:


library(tidyverse)
library(SEXYrye)
library(lme4)
library(lmerTest)
library(plotly)
library(ggthemes)

theme_set(theme_igray())

# data --------------------------------------------------------------------

d <- 
  sexy1_eukey %>% 
  select(-eu_id, -plothalf_id) %>% 
  distinct() %>% 
  left_join(sexy1_standcount)


# stats -------------------------------------------------------------------

#--first look at it
#--there are some mistakes, p has too many in block 2, xaprows has too many in block 1

ggplotly(
  d %>% 
  ggplot(aes(croptrt_id, plants_m2)) +
  geom_jitter(aes(color = block_id), size = 3, width = 0.1)
)

#--maybe remove the xacc block 3

  d %>% 
    ggplot(aes(croptrt_id, plants_m2)) +
    geom_jitter(aes(color = block_id), size = 4, width = 0.1) + 
    facet_grid(.~crop_cat, scales = "free_x")
