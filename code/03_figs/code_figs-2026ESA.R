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
            aes(x = trt_nice, y = 1, label = round(value, 0), color = trt_nice), size = 11, fontface = "italic", show.legend = F) +
  geom_text(data = g2_means, 
            aes(x = trt_nice, y = value + 1, label = sigletter), size = 11, show.legend = F) +
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


n1 <- sexy1_swnitrate 

n2 <- 
  n1 |> 
  left_join(sexy1_plotkey) 

#--no idea if these are significant...
n2_means <- 
  n2 |> 
  group_by(trt_nice) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  arrange(-value) |> 
  mutate(sigletter = c("A", "B", "C"))


ggplot() +
  geom_col(data = n2_means, color = "black",
           aes(x = trt_nice, y = value, fill = trt_nice), width = 0.8, show.legend = F) +
  geom_jitter(data = n2, color = "gray80", 
              aes(x = trt_nice, y = value, shape = trt_nice), width = 0.1, show.legend = F) +
  geom_text(data = n2_means, 
            aes(x = trt_nice, y = 1, label = round(value, 0), color = trt_nice), size = 11, fontface = "italic", show.legend = F) +
  geom_text(data = n2_means, 
            aes(x = trt_nice, y = value + 1, label = sigletter), size = 11, show.legend = F) +
  labs(x = NULL,
       y = myyieldlab) +
  scale_y_continuous(limits = c(0, 10)) +
  scale_fill_manual(values = c("Annual" = p_pur5, "Perennial" = p_yel1, "Annual/Perennial Mix" = p_red3)) +
  scale_color_manual(values = c("Annual" = "white", "Perennial" = "black", "Annual/Perennial Mix" = "white")) +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)))
