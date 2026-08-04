#--purpose:   create figs for ESA presentation
#--created:   july 2026
#--notes:     

source("code/00_palettes and legends.R")

library(tidyverse)
library(prye)




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

# fall biomass ------------------------------------------------------------

#--don't have the mixes

f1 <- 
  sexy1_fallbio |> 
  mutate(plot = as.character(plot))

#--get total at last biomass sampling
f2 <- 
  f1 |> 
  ungroup() |> 
  filter(sampledate_ymd2 == max(sampledate_ymd2)) |> 
  group_by(field_id, sea_name, trt_name, block, plot, sampledate_ymd2, data_type) |> 
  summarise(value = sum(value))

f3 <- 
  f2 |> 
  left_join(sexy1_plotkey) 

dummy_mix <- 
  f3 |> 
  ungroup() |> 
  select(field_id, sea_name, block, plot, sampledate_ymd2, data_type) |> 
  mutate(trt_name = "apmix",
         value = NA)


f4 <- 
  f3 |> 
  bind_rows(dummy_mix) |> 
  mutate(trt_nice = case_when(
    trt_name %in% c("a") ~ "Annual",
    trt_name %in% c("acc") ~ "Annual+CC",
    trt_name %in% c("p") ~ "Perennial",
    trt_name %in% c("pcc") ~ "Perennial+CC",
    #trt_name %in% c("apmix") ~ "Annual/Perennial Mix",
    #trt_name %in% c("aprows") ~ "Annual/Perennial Mix",
    TRUE ~ "no herbicides"
  )) 


f5 <- 
  f4 |> 
  filter(trt_nice != "no herbicides")

#--no idea if these are significant...need to do stats
f5_means <- 
  f5 |> 
  group_by(trt_nice) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  arrange(-value) 


ggplot() +
  geom_col(data = f5_means, color = "gray80",
               aes(x = trt_nice, y = value, fill = trt_nice), width = 0.5, show.legend = F) +
  geom_jitter(data = f5, color = "gray80", size = 4,
              aes(x = trt_nice, y = value, shape = trt_nice), width = 0.1, show.legend = F) +
  geom_text(data = f5_means, 
            aes(x = trt_nice, y = value + 25, label = round(value, 0)), size = 11, fontface = "italic", show.legend = F) +
  labs(x = NULL,
       y = myfallbiolab) +
  scale_y_continuous(limits = c(0, 1000)) +
  scale_fill_manual(values = c("Annual" = p_pur5,
                               "Annual+CC" = p_pur1,
                               "Perennial" = p_yel1, 
                               "Perennial+CC" = p_yel2)) +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)))

ggsave("figs/pres_fallbio.png", width = 8.6, height = 9.2)


