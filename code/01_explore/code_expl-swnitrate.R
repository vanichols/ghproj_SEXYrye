#--explore data

library(prye)
library(tidyverse)

d1 <- sexy1_plotkey |> as_tibble()
d0 <- sexy1_trtkey |> 
  separate(trt_desc, into = c("crop", "planting_season", "row_width", "cover_crop", "herbicide"),
           sep = ",")

d2 <- sexy1_swnitrate |> 
  as_tibble() 

#--merge
d3 <- 
  d2 |> 
  left_join(d1, by = c("field_id", "sea_name", "plot"), relationship = "many-to-many") |> 
  left_join(d0)



# viz ---------------------------------------------------------------------

#--not useful except to see last two samplig points are NAs
d3 |> 
  ggplot(aes(trt_name, value)) +
  geom_boxplot(aes(color = trt_name)) +
  facet_wrap(~sampledate_ymd)

d3 |> 
  group_by(trt_name, sampledate_ymd) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  ggplot(aes(sampledate_ymd, value, group = trt_name)) +
  geom_line(aes(color = trt_name), linewidth = 2)

d3 |> 
  group_by(herbicide, crop, cover_crop, trt_name, sampledate_ymd) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  ggplot(aes(sampledate_ymd, value, group = trt_name)) +
  geom_line(aes(color = cover_crop, linetype = herbicide)) +
  facet_grid(.~crop)

d3 |> 
  group_by(trt_name) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  ggplot(aes(reorder(trt_name, -value), value)) +
  geom_col(aes(fill = trt_name))

d3 |>
  ggplot(aes(fct_reorder(trt_name, value, .fun = mean, .desc = TRUE), value)) +
  stat_summary()
