# created: 12 feb 2025
# purpose: analyze stand counts
# notes: 302b and 309b are missing


library(tidyverse)
library(SEXYrye)
library(ggthemes)

theme_set(theme_igray())

# data --------------------------------------------------------------------

d <- 
  sexy1_eukey %>% 
  #select(-eu_id, -plothalf_id) %>% 
  distinct() %>% 
  left_join(sexy1_coverage)



# viz ---------------------------------------------------------------------

d %>% 
  ggplot(aes(crop_cat, coverage_ex_gr)) +
           geom_jitter(aes(color = block_id), width = 0.2)

tst <-
  d %>% 
  filter(plot_id == 309)

d %>% 
  mutate(coverage_ex_gr = coverage_ex_gr *100) %>% 
  group_by(plot_id) %>% 
  mutate(max_id = ifelse(coverage_ex_gr == max(coverage_ex_gr, na.rm = T), 
                         "max", "not" )) %>% 
  ggplot(aes(plothalf_id, coverage_ex_gr)) +
  geom_point(aes(color = max_id), 
              show.legend = F) +
  facet_wrap(~ plot_id + crop_cat, nrow = 2, 
             scale = "free_x") +
  scale_color_manual(values = c("red", "gray")) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(caption = "Coverage data from Tim and his drone",
       title = "Picking a plot side for yields")

ggsave("figs/2025-02_coverage-pick-a-side.png",
       width = 11, height = 6)
