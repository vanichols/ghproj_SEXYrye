#--purpose:   create figs for ESA presentation
#--created:   july 2026
#--notes:     

rm(list = ls())
source("code/00_palettes and legends.R")

library(tidyverse)
library(prye)

th_esa <- 
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        panel.grid.major.x = element_blank() )


# grain -------------------------------------------------------------------

# add nice trt_name

g1 <- 
  sexy1_grain |> 
  mutate(trt_nice = case_when(
    trt_name %in% c("a", "acc", "xa", "xacc") ~ "Annual",
    trt_name %in% c("p", "pcc", "xp", "xpcc") ~ "Perennial",
    trt_name %in% c("apmix", "xapmix") ~ "Annual/Perennial Mix",
    trt_name %in% c("aprows", "xaprows") ~ "Annual/Perennial Mix",
  ))

g2 <- 
  g1 |> 
  filter(data_type == "grain_Mgha") 

g2_means <- 
  g2 |> 
  group_by(trt_nice) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  arrange(-value) |> 
  mutate(sigletter = c("A", "B", "C"))


ggplot() +
  geom_col(data = g2_means, color = "black",
           aes(x = trt_nice, y = value, fill = trt_nice), width = 0.8, show.legend = F) +
  geom_jitter(data = g2, color = "gray80", 
              aes(x = trt_nice, y = value, shape = trt_nice), width = 0.1, show.legend = F) +
  geom_text(data = g2_means, 
            aes(x = trt_nice, y = value + 1, label = round(value, 0)), size = 11, fontface = "italic", show.legend = F) +
  geom_text(data = g2_means, 
            aes(x = trt_nice, y = 1, label = sigletter, color = trt_nice), size = 11, show.legend = F) +
  labs(x = NULL,
       y = myyieldlab) +
  scale_y_continuous(limits = c(0, 10)) +
  scale_fill_manual(values = c("Annual" = p_pur5, "Perennial" = p_yel1, "Annual/Perennial Mix" = p_red3)) +
  scale_color_manual(values = c("Annual" = "white", "Perennial" = "black", "Annual/Perennial Mix" = "white")) +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)))

ggsave("figs/pres_grain-yields.png", width = 8.6, height = 9.2)


# nitrate leaching --------------------------------------------------------

n1 <- 
  sexy1_swnitrate

n2 <- 
  n1 |> 
  left_join(sexy1_plotkey) |> 
  mutate(trt_nice = case_when(
    trt_name %in% c("a", "acc", "xa", "xacc") ~ "Annual",
    trt_name %in% c("p", "pcc", "xp", "xpcc") ~ "Perennial",
    trt_name %in% c("apmix", "xapmix") ~ "Annual/Perennial Mix",
    trt_name %in% c("aprows", "xaprows") ~ "Annual/Perennial Mix",
  ))

#--no idea if these are significant...
n2_means <- 
  n2 |> 
  group_by(trt_nice) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  arrange(-value) |> 
  mutate(sigletter = c(" ", " ", " "))


ggplot() +
  geom_boxplot(data = n2, color = "gray80",
              aes(x = trt_nice, y = value, fill = trt_nice), width = 0.5, show.legend = F) +
  geom_jitter(data = n2, color = "gray80",
              aes(x = trt_nice, y = value, shape = trt_nice), width = 0.1, show.legend = F) +
  geom_text(data = n2_means, 
            aes(x = trt_nice, y = value + 4, label = round(value, 0), color = trt_nice), size = 11, fontface = "italic", show.legend = F) +
  geom_text(data = n2_means, 
            aes(x = trt_nice, y = 0.2, label = sigletter), size = 11, show.legend = F) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  labs(x = NULL,
       y = mynitratelab) +
  #scale_y_continuous(limits = c(0, 10)) +
  scale_fill_manual(values = c("Annual" = p_pur5, "Perennial" = p_yel1, "Annual/Perennial Mix" = p_red3)) +
  scale_color_manual(values = c("Annual" = "black", "Perennial" = "black", "Annual/Perennial Mix" = "black")) +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)))

ggsave("figs/pres_soil-nitrate.png", width = 8.6, height = 9.2)

# 
# library(ggiraph)
# 
# girafe(
#   
#   ggplot() +
#     # geom_boxplot(data = n2_means, color = "black",
#     #          aes(x = trt_nice, y = value, fill = trt_nice), width = 0.8, show.legend = F) +
#     geom_jitter_interactive(data = n2, color = "gray80",
#                 aes(x = trt_nice, y = value, shape = trt_nice, tooltip = block), width = 0.5, show.legend = F) +
#     geom_boxplot(data = n2, color = "gray80",
#                  aes(x = trt_nice, y = value, fill = trt_nice), width = 0.5, show.legend = F) +
#     geom_text(data = n2_means, 
#               aes(x = trt_nice, y = 0.1, label = round(value, 0), color = trt_nice), size = 11, fontface = "italic", show.legend = F) +
#     geom_text(data = n2_means, 
#               aes(x = trt_nice, y = value + 1, label = sigletter), size = 11, show.legend = F) +
#     labs(x = NULL,
#          y = mynitratelab) +
#     #scale_y_continuous(limits = c(0, 10)) +
#     scale_fill_manual(values = c("Annual" = p_pur5, "Perennial" = p_yel1, "Annual/Perennial Mix" = p_red3)) +
#     scale_color_manual(values = c("Annual" = "black", "Perennial" = "black", "Annual/Perennial Mix" = "black")) +
#     theme_bw() +
#     theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
#           axis.text.y = element_text(size = rel(1.5)),
#           axis.text.x = element_text(size = rel(1.5)))
#   
# )
# 
# ggplot() +
#   geom_jitter_interactive(data = n2 ,
#                           aes(x = block, color = trt_nice, y = value, shape = trt_nice, tooltip = block), width = 0.2, size = 4) +
#   labs(x = NULL,
#        y = mynitratelab) +
#   #scale_y_continuous(limits = c(0, 10)) +
#   scale_fill_manual(values = c("Annual" = p_pur5, "Perennial" = p_yel1, "Annual/Perennial Mix" = p_red3)) +
#   #scale_color_manual(values = c("Annual" = "black", "Perennial" = "black", "Annual/Perennial Mix" = "black")) +
#   theme_bw() +
#   theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
#         axis.text.y = element_text(size = rel(1.5)),
#         axis.text.x = element_text(size = rel(1.5))) 

# fall biomass emmeans------------------------------------------------------------

#--don't have the mixes in this data
f_emm <- read_csv("data/stats/figs_emmeans/emmeans_fallbio.csv")

f_emm2 <- 
  f_emm |> 
  mutate(trt_nice = case_when(
    trt_name %in% c("a") ~ "Annual",
    trt_name %in% c("acc") ~ "Annual+CC",
    trt_name %in% c("p") ~ "Perennial",
    trt_name %in% c("pcc") ~ "Perennial+CC",
    #trt_name %in% c("apmix") ~ "Annual/Perennial Mix",
    #trt_name %in% c("aprows") ~ "Annual/Perennial Mix",
    TRUE ~ "no herbicides"
  )) |> 
  filter(trt_nice != "no herbicides")


ggplot(data = f_emm2, aes(trt_nice, emmean)) +
  geom_col(aes(fill = trt_nice), 
           color = "gray80",
           width = 0.5, 
           show.legend = F) +
  geom_linerange(aes(ymin = asymp.LCL,
                     ymax = asymp.UCL), 
                 color = "gray80") +
  geom_text(aes(y = emmean + 50, label = paste0(round(emmean, 0))), 
            size = 8, fontface = "italic", show.legend = F) +
  geom_text(aes(y = 50, label = sig_letter, color = trt_nice), 
            size = 8, show.legend = F) +
  labs(x = NULL,
       y = myfallbiolab) +
  scale_y_continuous(limits = c(0, 1000)) +
  scale_fill_manual(values = c("Annual" = p_pur4,
                               "Annual+CC" = p_pur1,
                               "Perennial" = p_yel1, 
                               "Perennial+CC" = p_yel2)) +
  scale_color_manual(values = c("Annual" = "white",
                               "Annual+CC" = "black",
                               "Perennial" = "black", 
                               "Perennial+CC" = "black")) +
  th_esa

ggsave("figs/pres_fallbio.png", width = 8.6, height = 9.2)



# fall biomass categories -------------------------------------------------

#--weed sig letters
w_sig <- 
  read_csv("data/stats/figs_emmeans/emmeans_fallbio-weeds.csv") |> 
  mutate(trt_nice = case_when(
    trt_name %in% c("a") ~ "Annual",
    trt_name %in% c("acc") ~ "Annual+CC",
    trt_name %in% c("p") ~ "Perennial",
    trt_name %in% c("pcc") ~ "Perennial+CC",
    #trt_name %in% c("apmix") ~ "Annual/Perennial Mix",
    #trt_name %in% c("aprows") ~ "Annual/Perennial Mix",
    TRUE ~ "no herbicides"
  )) |> 
  filter(trt_nice != "no herbicides") |> 
  mutate(ypos = emmean/2) |> 
  #--because we don't have herbicides, make the bc a b
  mutate(sig_letter = ifelse(sig_letter == "bc", "b", sig_letter))

#--raw data with categories
f1 <- 
  sexy1_fallbio |> 
  mutate(plot = as.character(plot))

#--get total at last biomass sampling
f2 <- 
  f1 |> 
  ungroup() |> 
  filter(sampledate_ymd2 == max(sampledate_ymd2)) |> 
  mutate(biomass_cat = ifelse(biomass_cat %in% c("oil", "pha"), "cc", biomass_cat)) |> 
  group_by(trt_name, biomass_cat) |> 
  summarise(value = mean(value, na.rm = T))


f3 <- 
  f2 |> 
  mutate(trt_nice = case_when(
    trt_name %in% c("a") ~ "Annual",
    trt_name %in% c("acc") ~ "Annual+CC",
    trt_name %in% c("p") ~ "Perennial",
    trt_name %in% c("pcc") ~ "Perennial+CC",
    #trt_name %in% c("apmix") ~ "Annual/Perennial Mix",
    #trt_name %in% c("aprows") ~ "Annual/Perennial Mix",
    TRUE ~ "no herbicides"
  )) 

#--make categories nice
f4 <- 
  f3 |> 
  filter(trt_nice != "no herbicides") |> 
  mutate(biomass_nice = case_when(
    biomass_cat %in% c("cc") ~ "Cover crops",
    biomass_cat %in% c("rye re") ~ "Perennial rye regrowth",
    biomass_cat %in% c("rye vo") ~ "Rye volunteers",
    biomass_cat %in% c("weeds") ~ "Weeds",
    TRUE ~ "uh oh"
  )) 

#--there is something wrong bc these totals don't match the emmeans totals
#--should I get average percents, then take percentages of the totals?
ggplot() +
  geom_col(data = f4, color = "black",
           aes(x = trt_nice, y = value, fill = biomass_nice), 
           width = 0.5) +
  geom_text(data = w_sig, color = "black",
           aes(x = trt_nice, y = ypos, label = sig_letter)) +
  scale_fill_manual(values = c("Cover crops" = p_pur5,
                               "Perennial rye regrowth" = p_yel4,
                               "Rye volunteers" = p_yel1, 
                               "Weeds" = p_red3)) +
  labs(x = NULL,
       y = myfallbiolab,
       fill = "Biomass category") +
  th_esa
