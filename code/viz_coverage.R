# created: 12 feb 2025
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
  left_join(sexy1_coverage)



# viz ---------------------------------------------------------------------

d %>% 
  ggplot(aes(crop_cat, mean)) +
           geom_jitter(aes(color = block_id), width = 0.2)


d %>% 
  ggplot(aes(crop_cat, mean)) +
  geom_jitter(aes(color = block_id), 
              width = 0.2, 
              show.legend = F) +
  facet_wrap(~ plot_id, nrow = 2, 
             scale = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(caption = "Data from Tim and his drone")

ggsave("figs/2025-02_coverage.png",
       width = 11, height = 6)
