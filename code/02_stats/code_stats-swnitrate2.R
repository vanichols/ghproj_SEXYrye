#--purpose:   do stats on soil water nitrate concentrations
#--created:   july 2026
#--notes:

#--from simon:
# First figure out the sensoring - how many data points is it? Does it matter?
# could possibly use a bayesian approach if it matters
# include time (do a regression), see if there are sig slopes
# can just use 'trt' as factor in model, then do contrasts to get:
# cover crop x crop x herbicide, for example
#--use lme...see attached pictures of notes

#--note the minimum detection level is 0.05 mg/l


library(tidyverse)
library(prye)

# 1. data -----------------------------------------------------------------

#--don't need analyzer_id right now
d1 <- 
  sexy1_swnitrate |> 
  left_join(sexy1_plotkey) |> 
  select(field_id, sea_name, data_type, block, plot, sampledate_ymd, trt_name, value, nitrate_sensored)

# 2. how many undetectible values are there? ---------------------------------

#--9 total, or 2%
d1 %>%
  summarise(
    nitrate_sensored = sum(nitrate_sensored),
    nitrate_sensored_pct = sum(nitrate_sensored)/nrow(.))

#--are they randomly distributed?
#--over time, more or less
d1 |> 
  group_by(plot, sampledate_ymd) |> 
  summarise(nitrate_sensored = sum(nitrate_sensored)) |> 
  ggplot(aes(plot, nitrate_sensored)) +
  geom_col() +
  facet_grid(.~sampledate_ymd)

#--the xacc will be overly biased to a low value, potentially, xpcc as well, when using 0
d1 |> 
  group_by(trt_name) |> 
  summarise(nitrate_sensored = sum(nitrate_sensored)) |> 
  ggplot(aes(trt_name, nitrate_sensored)) +
  geom_col() 

#--I chose to believe it is more or less random, will proceed with assigning them as 0s for now (waiting on lowest detectible limit)


# 3. time -----------------------------------------------------------------

#--use models simon suggests

