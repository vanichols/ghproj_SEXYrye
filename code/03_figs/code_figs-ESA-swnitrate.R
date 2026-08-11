# purpose: make fig of soil water nitrate
# created: aug 2026
# notes:

rm(list = ls())

source("code/utils.R")

source("code/00_palettes and legends.R")


library(tidyverse)
library(prye)

d.slopes <- read_csv("data/stats/figs_emmeans/emmeans_swnitrate-slopes.csv")
d.preds <- read_csv("data/stats/figs_emmeans/emmeans_swnitrate-preds.csv")

d1 <- 
  sexy1_swnitrate |> 
  left_join(sexy1_plotkey) |> 
  select(field_id, sea_name, data_type, block, plot, 
         sampledate_ymd, days_after_harvest, dah,
         trt_name, value, nitrate_sensored) %>%
  fxn_SeparateTrt()



ggplot(d1, aes(dah, value, color = crop, group = trt_name)) +
  geom_point(alpha = 0.4, aes(shape = cctrt)) +
  geom_ribbon(
    data = d.preds,
    aes(y = emmean, ymin = lower.CL, ymax = upper.CL, fill = crop),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(
    data = d.preds,
    aes(y = emmean, linetype = cctrt)
  ) +
  scale_color_manual(values = c("Annual rye" = p_red3,
                                "Perennial rye" = p_pur5,
                                "P/A mixture" = p_gre2,
                                "P/A rows" = p_gre2
                                
  )) +
  scale_fill_manual(values = c("Annual rye" = p_red3,
                                "Perennial rye" = p_pur5,
                                "P/A mixture" = p_gre2,
                                "P/A rows" = p_gre2
  )) +
  theme_classic() +
  facet_grid(herb~crop)
