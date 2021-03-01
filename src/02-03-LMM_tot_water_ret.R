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
# Purpose of this R script: to conduct statistical analyses on the relationship
# between root traits and ecosystem functions

# libraries --------------------------------------------------------------------
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(broom)
library(ggstatsplot)
library(flextable)
library(lme4)

# import -----------------------------------------------------------------------
traits_EF <- readRDS(
  here("data/final",
       "traits_EF_clean_df.rds")
  )

# check packaging --------------------------------------------------------------
str(traits_EF)

# split data set: wet  ----------------------------------------------------------
EF_WW <- traits_EF %>% 
  filter(treatment == "WW") %>%
  drop_na() %>%
  ungroup() %>%
  as.data.frame()
  
# split data set: dry -----------------------------------------------------------
EF_WD <- traits_EF %>% 
  filter(treatment == "WD") %>%
  drop_na() %>%
  ungroup() %>%
  as.data.frame() 

# linear mixed effect models: water capture (dry treatment) --------------------

lmm_total_ret_WD <- lmer(
  total_water_capture ~ 
  
    # fixed vars
    scale(srl) +
    scale(mean_radius_mm) + 
    scale(rld) + 
    scale(rmf) + 
    scale(max_root_depth_cm) + 
    
    # covariate var 
    scale(plant_size) +  
  
    #bloc random effect
    (1|block),
  
  data = EF_WD)


# check the variance component of the random effect
# five levels for block effect
summary(lmm_total_ret_WD)

# result: singularity and zero variance component
# drop the block variable and opt for a more simple model

# linear mixed effect models: water capture (wet treatment) --------------------

lmm_total_ret_WW <- lmer(
  total_water_capture ~ 
    
    # fixed vars
    scale(srl) +
    scale(mean_radius_mm) + 
    scale(rld) + 
    scale(rmf) + 
    scale(max_root_depth_cm) + 
    
    # covariate var 
    scale(plant_size) +  
    
    #bloc random effect
    (1|block),
  
  data = EF_WW)

# check the variance component of the random effect
# five levels for block effect
summary(lmm_total_ret_WW)

# multiple regression: water capture for wet treatment -------------------------

# model fitting
lm_total_ret_WW <- lm(
  formula = 
    
    # response var
    total_water_capture ~ 
    
    # fixed vars
    scale(srl) +
    scale(mean_radius_mm) + 
    scale(rld) + 
    scale(rmf) + 
    scale(max_root_depth_cm) + 
    
    # covariate var 
    scale(plant_size), 
    
  data = EF_WW)

# get coefficients, p-values, R-squared values, degrees of freedom
summary(lm_total_ret_WW)

# check for model diagnostics
plot(lm_total_ret_WW, c(1)) # check for linearity
plot(lm_total_ret_WW, c(2)) # check for normality
plot(lm_total_ret_WW, c(3)) # check for homogeneity of variance
plot(lm_total_ret_WW, c(5)) # check for influential outliers

# check for multicollinearity
vif(lm_total_ret_WW)

# multiple regression: water capture for dry treatment -------------------------

# model fitting
lm_total_ret_WD <- lm(
  formula = 
    
    # response var
    total_water_capture ~ 
    
    # fixed vars
    scale(srl) +
    scale(mean_radius_mm) + 
    scale(rld) + 
    scale(rmf) + 
    scale(max_root_depth_cm) + 
    
    # covariate var 
    scale(plant_size), 
  
  data = EF_WD)

# get coefficients, p-values, R-squared values, degrees of freedom
summary(lm_total_ret_WD)

# check for model diagnostics
plot(lm_total_ret_WD, c(1)) # check for linearity
plot(lm_total_ret_WD, c(2)) # check for normality
plot(lm_total_ret_WD, c(3)) # check for homogeneity of variance
plot(lm_total_ret_WD, c(5)) # check for influential outliers

# check for multicollinearity
vif(lm_total_ret_WD)

# save to disk -----------------------------------------------------------------

ggsave(plot = plot_WD,
       here(
         "output/figures/main", 
         "fig1-RET_WD.png"
       ),
       width = 7.5, height = 5.4, 
       device = "png")

ggsave(plot = plot_WW,
       here(
         "output/figures/main", 
         "fig1-RET_WW.png"
       ),
       width = 7.5, height = 5.4, 
       device = "png")

