#--make fig of soil water nitrate

d.slopes <- read_csv("data/stats/figs_emmeans/emmeans_swnitrate-slopes.csv")
d.preds <- read_csv("data/stats/figs_emmeans/emmeans_swnitrate-preds.csv")

d1 <- 
  sexy1_swnitrate |> 
  left_join(sexy1_plotkey) |> 
  select(field_id, sea_name, data_type, block, plot, 
         sampledate_ymd, days_after_harvest, dah,
         trt_name, value, nitrate_sensored) %>%
  fxn_SeparateTrt()



ggplot(d1, aes(dah, value, color = trt_name)) +
  geom_point(alpha = 0.4) +
  geom_ribbon(
    data = pred,
    aes(y = emmean, ymin = lower.CL, ymax = upper.CL, fill = trt_name),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(
    data = pred,
    aes(y = emmean)
  ) +
  theme_classic() +
  facet_grid(.~crop)
