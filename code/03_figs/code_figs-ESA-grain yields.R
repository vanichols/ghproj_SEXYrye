#--purpose:   create figs for ESA presentation
#--created:   july 2026
#--notes:     I need to find better colors

rm(list = ls())
source("code/00_palettes and legends.R")
source("code/utils.R")

library(tidyverse)
library(prye)

th_esa <- 
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        strip.text = element_text(size = rel(1.75)),
        panel.grid.major.x = element_blank() )


# grain -------------------------------------------------------------------

graw <- 
  sexy1_grain |> 
  fxn_SeparateTrt() |>
  fxn_MakeNice() |> 
  filter(data_type == "grain_Mgha")

g1 <- 
  read_csv("data/stats/figs_emmeans/emmeans_grainyields.csv") |> 
  mutate(cctrt = NA) |> 
  fxn_MakeNice()



ggplot() +
  geom_col(data = g1, color = "black",
           aes(x = cropNice, y = emmean, fill = cropNice), width = 0.8, show.legend = F) +
  geom_jitter(data = graw, color = "gray80", 
              aes(x = cropNice, y = value, shape = cropNice), width = 0.1, show.legend = F) +
  geom_text(data = g1, 
            aes(x = cropNice, y = emmean + 1, label = round(emmean, 1)), size = 11, fontface = "italic", show.legend = F) +
  geom_text(data = g1, 
            aes(x = cropNice, y = 1, label = sig_letter), color = "gray50", size = 11, show.legend = F) +
  labs(x = NULL,
       y = myyieldlab) +
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_fill_manual(values = c("Annual" = p_annual, 
                               "Perennial" = p_perennial, 
                               "A/P mix" = p_mix)) +
  th_esa +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5))) +
  facet_grid(. ~ herbNice)

ggsave("figs/pres_grain-yields.png", width = 8.5, height = 7)

