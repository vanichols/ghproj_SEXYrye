#--purpose:   do stats on soil water nitrate concentrations
#--created:   july 2026
#--notes:

library(tidyverse)
library(prye)



# 1. data -----------------------------------------------------------------

d1 <- 
  sexy1_swnitrate |> 
  left_join(sexy1_plotkey) |> 
  select(field_id, sea_name, data_type, block, plot, sampledate_ymd, trt_name, value)

d2 <- 
  d1 |> 
  left_join(sexy1_trtkey |> 
              separate(trt_desc, into = c("crop", "planting_season", "row_width", "cover_crop", "herbicide"),
                       sep = ",")) |> 
  select(-planting_season, -row_width, -crop) 

#--four blocks
d2 |> pull(block) |> unique()
#--nine sample points, but the last two have a lot of blanks where no water was pulled
#--seven functional sample points

d1 |> 
  ggplot(aes(trt_name, value)) +
  geom_boxplot(aes(color = trt_name)) +
  facet_wrap(~sampledate_ymd)

d2
#--four crop treatments (p, a, apmix, aprows) or three if you pool the mixes together (p, a, mix)
d2 |> pull(trt_nice) |> unique()

#--two cover crop treatments: no cover, or cover crop
d2 |> pull(cover_crop) |> unique()

#--not a completely balanced design w/regard to cover crop - the mixes don't have a cover crop treatment

#--all crop treatments have both with and without herbicide
d2 |> pull(herbicide) |> unique()

#--no real patterns over time, maybe a general trend upwards
d1 |> 
  group_by(trt_name, sampledate_ymd) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  ggplot(aes(sampledate_ymd, value, group = trt_name)) +
  geom_line(aes(color = trt_name), linewidth = 2)


#--there is a cover crop (not in the mixes)
d1 |> 
  left_join(sexy1_trtkey |> 
              separate(trt_desc, into = c("crop", "planting_season", "row_width", "cover_crop", "herbicide"),
                       sep = ",")) |> 
  group_by(trt_nice, herbicide, crop, cover_crop, trt_name, sampledate_ymd) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  ggplot(aes(sampledate_ymd, value, group = trt_name)) +
  geom_line(aes(color = cover_crop, linetype = herbicide)) +
  facet_grid(.~trt_nice)

#--cover crop versus herbicide versus crop
d2 |> 
  group_by(trt_nice, herbicide, crop, cover_crop, trt_name, sampledate_ymd) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  filter(trt_nice != "Annual/Perennial Mix") |> 
  ggplot(aes(sampledate_ymd, value, group = trt_name)) +
  geom_line(aes(color = cover_crop, linetype = herbicide), size = 2) +
  facet_grid(.~trt_nice)

#--time is not that interesting to me, could use it as a 'replicate'?
#--the mixes are also a bit odd because of some issues, removing them to get at cover crop would be worth it (they don't have cover crop treatments)
d2 |> 
  group_by(trt_nice, herbicide, crop, cover_crop, trt_name, sampledate_ymd) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  ggplot(aes(reorder(trt_name, -value), value)) +
  geom_col(aes(fill = trt_name)) +
  facet_grid(.~trt_nice, scales = "free_x", space = "free_x")


# my quetsions ------------------------------------------------------------


#--Q1: the difference between a and p without cover crop use - is it significant?
#--Q2: Within an herbicide treatment, is there an effect of cover crop?

