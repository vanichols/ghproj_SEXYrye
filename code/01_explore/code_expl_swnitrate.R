#--explore data

library(prye)
library(tidyverse)

d1 <- sexy1_plotkey |> as_tibble()
d0 <- sexy1_trtkey |> 
  separate(trt_desc, into = c("crop", "planting_season", "row_width", "cover_crop", "herbicide"),
           sep = ",")

d2 <- sexy1_swnitrate |> 
  as_tibble() 

#--below detectible limit becomes NA, then 0
d3 <- 
  d2 |> 
  mutate(nitraten_mgl2 = parse_number(nitraten_mgl),
         nitraten_mgl3 = ifelse(is.na(nitraten_mgl2), 0, nitraten_mgl2))

#--merge
d4 <- 
  d3 |> 
  left_join(d1, by = c("field_id", "sea_name", "plot"), relationship = "many-to-many") |> 
  left_join(d0)



# viz ---------------------------------------------------------------------

d4 |> 
  ggplot(aes(trt_name, nitraten_mgl3)) +
  geom_boxplot(aes(color = trt_name)) +
  facet_wrap(~sample_date_ymd)

d4 |> 
  group_by(trt_name, sample_date_ymd) |> 
  summarise(nitraten_mgl3 = mean(nitraten_mgl3)) |> 
  ggplot(aes(sample_date_ymd, nitraten_mgl3, group = trt_name)) +
  geom_line(aes(color = trt_name), linewidth = 2)

d4 |> 
  group_by(herbicide, crop, cover_crop, trt_name, sample_date_ymd) |> 
  summarise(nitraten_mgl3 = mean(nitraten_mgl3)) |> 
  ggplot(aes(sample_date_ymd, nitraten_mgl3, group = trt_name)) +
  geom_line(aes(color = cover_crop, linetype = herbicide)) +
  facet_grid(.~crop)


d4 |> 
  select(sample_date_ymd) |> 
  distinct()

d4 |> 
  group_by(trt_name) |> 
  summarise(nitraten_mgl3 = mean(nitraten_mgl3)) |> 
  ggplot(aes(reorder(trt_name, -nitraten_mgl3), nitraten_mgl3)) +
  geom_col(aes(fill = trt_name))
