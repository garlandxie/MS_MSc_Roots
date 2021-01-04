# libraries --------------------------------------------------------------------
library(here)
library(ggplot2)
library(dplyr)
library(forcats)

# import -----------------------------------------------------------------------

traits_EF <- readRDS(
  here("data/final",
       "traits_EF_clean_df.rds")
)

# check packaging -------------------------------------------------------------

str(traits_EF)

# linear models ----------------------------------------------------------------

lm1 <- lm(
  total_water_loss ~ total_water_capture + treatment, 
  data = traits_EF
)


# plot -------------------------------------------------------------------------

# set up custom annotation for the figure
p_rsq_lab <- paste(
  "Adjusted R-squared:",
  round(summary(lm1)$adj.r.squared, digits = 2)
)

p_cap_lab <- paste(
  "Stormwater capture: p",
  ifelse(summary(lm1)$coefficients[2,4] < 0.001, "<0.001")
)

p_trt_lab <- paste(
  "Watering regime: p",
  ifelse(summary(lm1)$coefficients[3,4] < 0.001, "<0.001")
)

ann_text <- data.frame(
  total_water_loss = c(1200, 1000, 800),
  total_water_capture = c(5500, 5500, 5500),
  treatment = factor(
    c("Well-watered", "Well-watered", "Well-watered"),
    levels = c("Well-watered", "Water-deficit")
    ),
  lab = c(p_cap_lab, p_trt_lab, p_rsq_lab)
)

# make the figure
(gg1 <- traits_EF %>%
  mutate(log_water_loss = log(total_water_loss),
         treatment = case_when(
           treatment == "WW" ~ "Well-watered", 
           treatment == "WD" ~ "Water-deficit", 
           TRUE ~ treatment)
         )%>%
  ggplot(aes(x = total_water_capture, y = total_water_loss)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~treatment) + 
  geom_text(data = ann_text, aes(label = lab), size = 3) + 
  labs(x = "Total stormwater capture (g)",
       y = "Total evaporative loss (g)") + 
  theme_bw() 
)

# save the data ----------------------------------------------------------------

ggsave(
  plot = gg1,
  here("output/figures/supp", "fig-supp-water_vs_loss.png"),
  device = "png",
  width = 6, 
  height = 5
)
