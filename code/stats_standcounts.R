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
  sexy1_standcounts %>% 
  left_join(sexy1_eukey) %>% 
  left_join(sexy1_plotkey) %>% 
  left_join(sexy1_trtkey) 

d %>% 
  write_csv("data/standcounts-for-emmas-records.csv")
# stats? -------------------------------------------------------------------

#--first look at it

d %>% 
  ggplot(aes(block, plants_m2)) +
  geom_point(aes(color = as.factor(date)), size = 3) +
  facet_grid(.~cropcat, scales = "free")
  
ggsave("figs/trial1_standcounts.png")

d %>% 
  mutate(year = year(date)) %>% 
  group_by(year, croptrt_id) %>% 
  summarise(plants_m2 = mean(plants_m2)) %>% 
  filter(croptrt_id %in% c("a", "p"))

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
