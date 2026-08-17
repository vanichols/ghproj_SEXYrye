#--purpose:   create figs for ESA presentation
#--created:   july 2026
#--notes:     I need to find better colors

rm(list = ls())
source("code/00_palettes and legends.R")
source("code/utils.R")

library(tidyverse)
library(prye)
library(patchwork)
library(scales)

th_esa <- 
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        strip.text = element_text(size = rel(1.75)),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "tan"))



# harvest -------------------------------------------------------------------

g1 <- 
  read_csv("data/stats/figs_emmeans/emmeans_grain.csv") |>
  mutate(cctrt = NA) |> 
  fxn_MakeNice() |> 
  filter(crop != "mix", herb == "herb")

#--letters are nonsense
s1 <- 
  read_csv("data/stats/figs_emmeans/emmeans_straw.csv") |> 
  mutate(cctrt = NA) |> 
  fxn_MakeNice() |> 
  filter(crop != "mix", herb == "herb") 

t1 <- 
  read_csv("data/stats/figs_emmeans/emmeans_totbio.csv") |> 
  mutate(cctrt = NA) |> 
  fxn_MakeNice() |> 
  filter(crop != "mix", herb == "herb") |> 
  mutate(sig_letter = ifelse(crop == "a", "a", "b"))


#--the circle thing isn't working

#--make circle thing
h1 <-
  g1 |>
  dplyr::select(cropNice, data_type, emmean) |>
  bind_rows(s1 |> dplyr::select(cropNice, data_type, emmean)) 

h1 |> 
  arrange(cropNice) |> 
  mutate(ypos = lead(emmean)) |> 
  mutate(combo = paste(cropNice, data_type),
         ypos2 = case_when(
           data_type == "grain" ~ ypos + emmean/2,
           .default = emmean/2
         )) |> 
  ggplot(aes(cropNice, emmean)) +
  geom_col(aes(fill = combo), color = "black", show.legend = F, width = 0.8) +
  geom_linerange(data = t1, aes(x = cropNice, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_linerange(data = t1, aes(x = cropNice, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_text(data = t1, 
            aes(cropNice, asymp.UCL + 1, label = paste0(round(emmean, 1), sig_letter)),
            size = 8, fontface = "italic") +
  geom_text(aes(cropNice, ypos2, label = round(emmean, 1)), 
            size = 6, color = "white") +
  scale_fill_manual(values = c("Annual grain" = p_annual,
                               "Perennial grain" = p_perennial,
                               "Annual straw" = "tan",
                               "Perennial straw" = "tan")) +
  labs(x = NULL,
       y = mytotbiolab,
       fill = NULL) +
  th_esa


ggsave("figs/ESA_drybiomass.png", width = 6, height = 3.6)


# nitrate -----------------------------------------------------------------

#--only acc slope is different
n.slopes <- read_csv("data/stats/figs_emmeans/emmeans_swnitrate-slopes.csv") |> 
  fxn_SeparateTrt() |> 
  fxn_MakeNice() |> 
  filter(herb == "herb") |> 
  filter(crop != "mix")


n.preds <- read_csv("data/stats/figs_emmeans/emmeans_swnitrate-preds.csv") |> 
  fxn_MakeNice() |> 
  filter(herb == "herb") |> 
  filter(crop != "mix") |> 
  mutate(TrtNice = case_when(
    trt_name == "p" ~ "Perennial",
    trt_name == "pcc" ~ "Perennial + CC",
    trt_name == "a" ~ "Annual",
    trt_name == "acc" ~ "Annual + CC",
    TRUE ~ "JUICE"
  )) |> 
  filter(dah < 150)


#--raw data
n1 <- 
  sexy1_swnitrate |> 
  left_join(sexy1_plotkey) |> 
  fxn_SeparateTrt() |> 
  fxn_MakeNice() |> 
  filter(herb == "herb") |> 
  filter(crop != "mix") |> 
  mutate(TrtNice = case_when(
    trt_name == "p" ~ "Perennial",
    trt_name == "pcc" ~ "Perennial + CC",
    trt_name == "a" ~ "Annual",
    trt_name == "acc" ~ "Annual + CC",
    TRUE ~ "JUICE"
  )) |> 
  filter(dah < 150)


n_star <- tibble(dah = 75, 
                 value = 2, 
                 text = "*",
                 cropNice = "Annual")

ggplot() +
  geom_point(data = n1, aes(dah, value, color = TrtNice, shape = TrtNice, group = trt_name),
             alpha = 0.4) +
  geom_ribbon(
    data = n.preds,
    aes(x = dah, y = emmean, ymin = lower.CL, ymax = upper.CL, group = trt_name),
    fill = "gray80",
    alpha = 0.35,
    color = NA
  ) +
  geom_line(
    data = n.preds,
    aes(x = dah, y = emmean, color = TrtNice),
    linewidth = 2
  ) +
  labs(x = "Days after harvest",
       y = mynitratelab,
       color = NULL,
       shape = NULL) +
  geom_text(data = n_star, aes(x = dah, y = value, label = text), 
            size = 18) +
  scale_color_manual(values = c("Annual" = p_annual,
                                "Perennial" = p_perennial,
                                "Annual + CC" = p_acc,
                                "Perennial + CC" = p_pcc
  )) +
  guides(
    color = guide_legend(override.aes = list(size = 4))
  ) +
  theme_classic() +
  th_esa +
  facet_grid(.~cropNice)

ggsave("figs/ESA_swnitrate-trends.png", width = 10, height = 5)



# biomass -----------------------------------------------------------------

b0 <- read_csv("data/stats/figs_emmeans/emmeans_fallbio.csv")

b1 <-
  b0 |> 
  fxn_SeparateTrt() |> 
  fxn_MakeNice() |> 
  filter(crop != "mix", herb == "herb") 
  
b1 |> 
  ggplot(aes(cctrtNice, emmean)) +
  geom_col(aes(fill = cropNice, alpha = cctrtNice), color = "black", show.legend = F) +
  geom_linerange(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_text(aes(y = 100, label = sig_letter), size = 8) +
  geom_text(aes(y = asymp.UCL + 50, label = round(emmean, 0)),
            fontface = "italic", size = 6) +
  scale_fill_manual(values = c("Annual" = p_annual,
                               "Perennial" = p_perennial)) +
  scale_alpha_manual(values = c(1, 0.5)) +
  labs(x = NULL,
       y = myfallbiolab,
       fill = NULL,
       alpha = NULL) +
  facet_grid(.~cropNice) +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  th_esa

ggsave("figs/ESA_fallbio.png", width = 8, height = 5)

# weather -----------------------------------------------------------------



w1 <- 
  flak_weatot |> 
  mutate(exp = ifelse(year == 2025, "2025", "1993-2023")) |> 
  ggplot(aes(tot_heatunits, tot_p)) +
  geom_point(aes(color = exp, shape = exp), size = 5) +

  geom_vline(xintercept = mean(flak_weatot$tot_heatunits)) +
  geom_hline(yintercept = mean(flak_weatot$tot_p)) +
  
  geom_text(aes(x = 3000, y = 400, label = "Cool and dry"), 
            check_overlap = T, hjust = 0.5, 
            fontface = "italic",  color = "gray35") +
  geom_text(aes(x = 3600, y = 400, label = "Warm and dry"), 
            check_overlap = T, hjust = 0.5, 
            fontface = "italic", color = "gray35") +
  geom_text(aes(x = 3000, y = 950, label = "Cool and wet"), 
            check_overlap = T, hjust = 0.5, 
            fontface = "italic", color = "gray35") +
  geom_text(aes(x = 3600, y = 950, label = "Warm and wet"), 
            check_overlap = T, hjust = 0.5, 
            fontface = "italic", color = "gray35") +
  labs(x = "Cumulative degree-days", 
       y = "Cumulative\nprecipitation (mm)",
       shape = NULL,
       color = NULL) +
  scale_color_manual(values = c("gray", p_gre3)) +
  th_esa +
  theme(legend.position = "top",
        legend.text = element_text(size = rel(1.5)))

w1

ggsave("figs/ESA-wea-summary.png",
       width = 6, height = 4)

#--lont term cum precip

sexy1_mgmt |> 
  mutate(doy = yday(date_ymd))

w2 <- 
  flak_wea |> 
  filter(year == 2025) |> 
  left_join(flak_weacum) |> 
  mutate(dev_psum = psum - psum_lt) |> 
  ggplot() +
  geom_line(aes(x = doy, y = dev_psum), color = "royalblue", linewidth = 3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 220, color = "gray", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 50, y = 100, label = "Wetter", 
            hjust = 0.5, fontface = "italic", color = "gray35") +
  annotate("text", x = 50, y = -50, label = "Drier", 
           hjust = 0.5, fontface = "italic", color = "gray35") +
  annotate("text", x = 223, y = 25, label = "Grain harvest", 
           hjust = 0, fontface = "italic", color = "gray") +
  labs(y = "Deviation\nfrom long-term\nprecipitation",
       x = "Day of year") +
  th_esa

w2

ggsave("figs/ESA-wea-precip.png",
       width = 6, height = 4)

w3 <- 
  flak_wea |> 
  filter(year == 2025) |> 
  left_join(flak_weacum) |> 
  mutate(dev_tsum0 = tsum0 - tsum0_lt) |> 
  ggplot() +
  geom_line(aes(x = doy, y = dev_tsum0), color = "darkred", linewidth = 3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 220, color = "gray", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 50, y = 100, label = "Warmer", 
           hjust = 0.5, fontface = "italic", color = "gray35") +
  annotate("text", x = 50, y = -50, label = "Cooler", 
           hjust = 0.5, fontface = "italic", color = "gray35") +
  annotate("text", x = 223, y = 25, label = "Grain harvest", 
           hjust = 0, fontface = "italic", color = "gray") +
  labs(y = "Deviation\nfrom long-term\ntemperature",
       x = "Day of year") +
  th_esa


w3

ggsave("figs/ESA-wea-te.png",
       width = 6, height = 4)

