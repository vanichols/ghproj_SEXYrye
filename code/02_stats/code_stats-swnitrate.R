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

rm(list = ls())

library(tidyverse)
library(prye)

library(lme4)
library(lmerTest)
library(emmeans)
library(ggResidpanel)


source("code/utils.R")

# 1. data -----------------------------------------------------------------

#--don't need analyzer_id right now
d1 <- 
  sexy1_swnitrate |> 
  left_join(sexy1_plotkey) |> 
  fxn_SeparateTrt()

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

#--is there a pattern in where we couldn't get water?
#--how to distinguish between where we couldn't get water and where it is a 'failed plot'....go to prye
d1 |> 
  filter(is.na(value)) |> 
  filter(dah < 150) |> #--still waiting on later samples
  group_by(trt_name, dah) |> 
  summarise(n = n()) |> 
  ggplot(aes(trt_name, n)) +
  geom_col() +
  facet_wrap(~dah)

# 3. time -----------------------------------------------------------------


m1 <- lmer(value ~ dah*trt_name + (1|block),
           data = d1)

summary(m1)
ggResidpanel::resid_panel(m1)
# 
# m2 <- lmer(value ~ trt_name + (1|block),
#            data = d1)
# 
# summary(m2)
# ggResidpanel::resid_panel(m2)
# 
# anova(m1, m2)
# 
# #--seems like model 1 is better, should include days after harvest
# anova(m1)

em_trends1 <- 
  emtrends(m1, ~ trt_name, var = "dah") 

em_trends1 |> 
  as_tibble() |> 
  ggplot(aes(trt_name, dah.trend)) +
  geom_point() +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL)) +
  geom_hline(yintercept = 0)

#--this is a good figure
#--can I get letters for them

cld1 <- 
  multcomp::cld(em_trends1, Letters = letters,  decreasing = TRUE) |> 
  as_tibble() |> 
  mutate(sig_letter = str_squish(.group),
         data_type = "swnitrate slopes") 


cld1 |> 
  write_csv("data/stats/figs_emmeans/emmeans_swnitrate-slopes.csv")


emmeans(m1, ~ trt_name|dah)

#--compare the slopes, too many, should make a contrast vector
pairs(emtrends(m1, ~ trt_name, var = "dah")) |> 
  as_tibble() |> 
  arrange(p.value)
#--seems like it is just acc that is different from everything else
#--this is consistent with the biomass

#-- this is the order:
em_trends1

#--help from simon needed

#--get a visual 
# pred <- expand.grid(
#   dah = seq(min(d1$dah, na.rm = TRUE),
#                 max(d1$dah, na.rm = TRUE),
#                 length.out = 100),
#   trt_name = levels(d1$trt_name)
# )
# 
# pred$value <- predict(m1, newdata = pred, re.form = NA)


#--or
pred <- emmeans(
  m1,
  ~ trt_name | dah,
  at = list(dah = seq(min(d1$dah),
                          max(d1$dah),
                          length.out = 100))
) |>
  as_tibble() |> 
  fxn_SeparateTrt()

pred |> 
  write_csv("data/stats/figs_emmeans/emmeans_swnitrate-preds.csv")

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
  facet_grid(herb~crop)
