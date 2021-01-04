################################################################################
# Accompanying code for the paper: 
#   Root traits influence storm-water performance in a green roof microcosm
#
# Authorship:
#   Garland Xie (1)
#   Jeremy Lundholm (2)
#
# Corresponding for this script: 
#   Garland Xie (1)
#
# Institutional affiliations:
# (1) Department of Biological Sciences
#     University of Toronto Scarborough,
#     1265 Military Trail, Toronto, ON, M1C 1A4, Canada
#     email: garlandxie@gmail.com
# (2) Department of Biology
#     Saint Mary's University
#     923 Robie St., Halifax, NS, B3H 3C3, Canada
#
# Purpose of this R script: to conduct ANOVA tests

# libraries --------------------------------------------------------------------
library(car)
library(here)
library(emmeans)
library(tidyverse)
library(lme4)

# import -----------------------------------------------------------------------
traits <- readRDS(
  here(
  "data/final",
  "traits_EF_clean_df.rds")
)

# clean ------------------------------------------------------------------------

# get stormwater data on plants that survived at the end of expt
traits1 <- traits %>%
  ungroup() %>%
  drop_na() %>%
  select(block, 
         spp, 
         treatment, 
         ind, 
         total_water_capture, 
         total_water_loss
         )

# get stormwater data on soil-only treatments
traits2 <- traits %>%
  ungroup() %>%
  filter(spp == "SC") %>%
  select(block, 
         spp, 
         treatment, 
         ind, 
         total_water_capture, 
         total_water_loss
         )

# clean
clean_df <- traits1 %>%
  rbind(traits2) %>%
  mutate(
    spp = case_when(
      spp  == "SOBI" ~ "Solidago bicolor",
      spp  == "SYNO" ~ "Symphyotrichum novae-angliae",
      spp  == "PLMA" ~ "Plantago maritima",
      spp  == "SEAC" ~ "Sedum acre",
      spp  == "SEAL" ~ "Sedum album",
      spp  == "SESE" ~ "Sedum sexangulare",
      spp  == "VAMA" ~ "Vaccinium macrocarpon",
      spp  == "EMNI" ~ "Empetrum nigrum",
      spp  == "DASP" ~ "Danthonia spicata",
      spp  == "SITR" ~ "Sibbaldiopsis tridentata",
      spp  == "SC"   ~ "Soil-only",
      spp  == "FERU" ~ "Festuca rubra",
      spp  == "DEFL" ~ "Deschampsia flexuosa",
      TRUE ~ spp),
    
    spp = fct_reorder(spp, total_water_capture),
    spp = fct_reorder(spp, total_water_loss)) %>%
  
  mutate(
    treatment = case_when(
      treatment == "WD" ~ "Water-Deficit",
      treatment == "WW" ~ "Well-Watered",
      TRUE ~ treatment
    )
  )
    
# ANOVA: total water capture ---------------------------------------------------

# fit a hierarchical linear mixed model
# fixed effects: species identity + watering regime
# random effects: block
total_ret_lmer <- 
  lmer(data = clean_df, 
       log(total_water_capture) ~ treatment + spp + (1|block)
  )

# variance component of block is approximately zero 
# drop the block effect and use a more simple approach
summary(total_ret_lmer)

# two-way ANOVA with an unbalanced design
# factors: species identity (eg. SOBI) and watering regime treatments (WW, WD)
# log-transformation on dependent variable to stabilize variance
total_ret_aov <- 
  lm(data = clean_df, 
       log(total_water_capture) ~ treatment + spp 
  )

# diagnostic plots: 
# (1) residuals vs fitted
# (2) normal Q-Q
# (3) scale-location
# (4) constant leverage: residuals vs factor levels
plot(total_ret_aov, c(1))
plot(total_ret_aov, c(2))
plot(total_ret_aov, c(3))
plot(total_ret_aov, c(4))

# reference grid: ref_grid_avg_ET
ref_grid(total_ret_aov)@grid

# type II sum of squares to account for unbalanced design: avg_ET_aov
Anova(total_ret_aov, type = "II")

# estimated marginal means (least square means): total_ret_lsm
total_ret_lsm <- 
  lsmeans(
    total_ret_aov, 
    specs = "spp", 
    by = "treatment"
  )

# multiple pairwise comparisons using Tukey HSD test
# shows letters for plotting purposes 
cmp_RET <- 
  plot(
     total_ret_lsm, 
     comparison = TRUE, 
     type = "response",
     alpha = 0.05,
     adjust = "tukey"
     )  +
  
  labs(x = "Estimated Least-Squares Mean (Total Stormwater Capture (g))", 
       y = NULL
       ) + 
  
  theme_bw() + 
  theme(axis.text.y = element_text(face = "italic"))

# ANOVA: total water loss -------------------------------------------------------

# fit a hierarchical linear mixed model
# fixed effects: species identity + watering regime
# random effects: block
total_ET_lmer <- 
  lmer(data = clean_df, 
       log(total_water_loss) ~ treatment + spp + (1|block)
  )

# variance component of block is approximately zero 
# drop the block effect and use a more simple approach
summary(total_ret_lmer)

# two-way ANOVA with an unbalanced design: total_ET_aov
# factors: species identity (eg. SOBI) and watering regime treatments (WW, WD)
# log-transformation on dependent variable to stabilize variance

total_ET_aov <- 
  lm(
    data = clean_df, 
    log(total_water_loss) ~ treatment + spp
    )

# diagnostic plots: 
# (1) residuals vs fitted
# (2) normal Q-Q
# (3) scale-location
# (4) constant leverage: residuals vs factor levels
plot(total_ET_aov, c(1))
plot(total_ET_aov, c(2))
plot(total_ET_aov, c(3))
plot(total_ET_aov, c(5))

# reference grid: ref_grid_avg_ET
ref_grid(total_ET_aov)@grid

# type II sum of squares to account for unbalanced design: avg_ET_aov
Anova(total_ET_aov, type = "II")

# estimated marginal means (least square means): avg_ET_lsm
total_ET_lsm <- 
  lsmeans(
    total_ET_aov, 
    specs = "spp", 
    by = "treatment"
    )

# multiple pairwise comparisons using Tukey HSD test
pairs(total_ET_lsm)

# shows letters for plotting purposes 
cmp_ET <- 
  plot(
    total_ET_lsm, 
    comparison = TRUE, 
    type = "response", 
    alpha = 0.05, 
    adjust = "tukey"
    ) + 
  
  labs(x = "Estimated Least-Squares Mean (Total Evapotranspiration (g))", 
       y = NULL
       ) + 
  
  theme_bw() + 
  theme(axis.text.y = element_text(face = "italic"))

# save to disk -----------------------------------------------------------------

ggsave(
  plot = cmp_ET, 
  filename = 
    here(
    "output/figures/main", 
    "cmp_ET.png"
    ), 
  device = "png",
  height = 5, 
  width  = 7
)

ggsave(
  plot = cmp_RET, 
  filename = 
    here(
      "output/figures/main", 
      "cmp_RET.png"
    ), 
  device = "png",
  height = 5, 
  width  = 7
)