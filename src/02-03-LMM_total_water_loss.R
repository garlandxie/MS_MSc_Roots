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
library(patchwork)
library(lme4)

# import -----------------------------------------------------------------------
traits_EF <- readRDS(
  here("data/final",
       "traits_EF_clean_df.rds")
)

# split data set: wet  ----------------------------------------------------------
EF_WW <- traits_EF %>% 
  filter(treatment == "WW",) %>%
  drop_na() %>%
  ungroup() %>%
  as.data.frame()

# remove influential outlier
EF_WW_O <- EF_WW[-86, ]

# split data set: dry -----------------------------------------------------------
EF_WD <- traits_EF %>% 
  filter(treatment == "WD") %>%
  drop_na()

# linear mixed effect models: water loss (wet treatment) -----------------------

lmm_total_ET_WW <- lmer(
  formula = 
    
    # response var
    total_water_loss ~ 
    
    # fixed vars
    scale(srl) +
    scale(mean_radius_mm) + 
    scale(rld) + 
    scale(rmf) + 
    scale(max_root_depth_cm) + 
    
    # covariate var 
    scale(plant_size) +
    
    # random effect
    (1|block), 
  
  data = EF_WW)

# check the variance component of the random effect
# five levels for block effect
summary(lmm_total_ET_WW)

# result: singularity and zero variance component
# drop the block variable and opt for a more simple model

# linear mixed effect models: water loss (dry treatment) -----------------------

lmm_total_ET_WD <- lmer(
  formula = 
    
    # response var
    total_water_loss ~ 
    
    # fixed vars
    scale(srl) +
    scale(mean_radius_mm) + 
    scale(rld) + 
    scale(rmf) + 
    scale(max_root_depth_cm) + 
    
    # covariate var 
    scale(plant_size) +
    
    # random effect
    (1|block), 
  
  data = EF_WD)

# check the variance component of the random effect
# five levels for block effect
summary(lmm_total_ET_WD)

# result: singularity and zero variance component
# drop the block variable and opt for a more simple model

# multiple regression: water loss for wet treatment (without outliers) ---------

# model fitting
lm_total_ET_WW <- lm(
  formula = 
    
    # response var
    total_water_loss ~ 
    
    # fixed vars
    scale(srl) +
    scale(mean_radius_mm) + 
    scale(rld) + 
    scale(rmf) + 
    scale(max_root_depth_cm) + 
    
    # covariate var 
    scale(plant_size), 
  
  data = EF_WW)

# get coefficients, p-values, R-squared values, degree of freedom
summary(lm_total_ET_WW)

# check for model diagnostics
plot(lm_total_ET_WW, c(1)) # check for linearity
plot(lm_total_ET_WW, c(2)) # check for normality
plot(lm_total_ET_WW, c(3)) # check for homogeneity of variance
plot(lm_total_ET_WW, c(5)) # check for influential outliers

# check for multicollinearity 
vif(lm_total_ET_WW)

# multiple regression: water loss for dry treatment (without outliers) ---------

# model fitting
lm_total_ET_WD <- lm(
  formula =  
    
    # response var
    total_water_loss ~ 
    
    # fixed vars
    scale(srl) + 
    scale(mean_radius_mm) + 
    scale(rld) + 
    scale(rmf) + 
    scale(max_root_depth_cm) + 
    
    # covariate var 
    scale(plant_size),
  
  data = EF_WD)

# get regression coefficients, p-values, R-squared values, degree of freedom
summary(lm_total_ET_WD)

# check for model diagnostics
plot(lm_total_ET_WD, c(1)) # check for linearity
plot(lm_total_ET_WD, c(2)) # check for normality
plot(lm_total_ET_WD, c(3)) # check for homogeneity of variance
plot(lm_total_ET_WD, c(5)) # check for influential outliers

# check for multicollinearity
vif(lm_total_ET_WD)

# multiple regression: water loss for dry treatment (with outliers) ------------

# model fitting
lm_total_ET_WW_O <- lm(
  formula = 
    
    # response var
    total_water_loss ~ 
    
    # fixed vars
    scale(srl) +
    scale(mean_radius_mm) + 
    scale(rld) + 
    scale(rmf) + 
    scale(max_root_depth_cm) + 
    
    # covariate var 
    scale(plant_size), 
  
  data = EF_WW_O) # note: only one outlier was removed here

# get regression coefficients, p-values, R-squared values, degree of freedom
summary(lm_total_ET_WW_O)

# check for model diagnostics
plot(lm_total_ET_WW_O, c(1)) # check for linearity
plot(lm_total_ET_WW_O, c(2)) # check for normality
plot(lm_total_ET_WW_O, c(3)) # check for homogeneity of variance
plot(lm_total_ET_WW_O, c(5)) # check for influential outliers

# check for multicollinearity
vif(lm_total_ET_WW_O)

# plot: total water capture - WD  ----------------------------------------------

plot_WD <- lm_total_ET_WD %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # remove random effect coefficient
  filter(term != "sd__(Intercept)") %>%
  
  # rename 
  mutate(term = case_when(
    term == "scale(rld)" ~ "Root length density",
    term == "scale(srl)" ~ "Specific root length",
    term == "scale(max_root_depth_cm)" ~ "Maximum root depth",
    term == "scale(plant_size)" ~ "Total biomass",
    term == "scale(mean_radius_mm)" ~ "Mean root diameter",
    term == "scale(rmf)" ~ "Root mass fraction",
    TRUE ~ term)) %>%
  
  slice(match(c(
    "Root length density",
    "Root mass fraction",
    "Total biomass", 
    "Maximum root depth",
    "Mean root diameter", 
    "Specific root length"
  ),
  term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              stats.labels = FALSE,
              conf.int = TRUE,
              point.args = c(color = "red")
  )+ 
  
  labs(title = "A) Without outliers",
       x = "Total Evapotranspiration (mL)", 
       y = NULL)

plot_WD_O <- lm_total_ET_WD_O %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # remove random effect coefficient
  filter(term != "sd__(Intercept)") %>%
  
  # rename 
  mutate(term = case_when(
    term == "scale(rld)" ~ "Root length density",
    term == "scale(srl)" ~ "Specific root length",
    term == "scale(max_root_depth_cm)" ~ "Maximum root depth",
    term == "scale(plant_size)" ~ "Total biomass",
    term == "scale(mean_radius_mm)" ~ "Mean root diameter",
    term == "scale(rmf)" ~ "Root mass fraction",
    TRUE ~ term)) %>%
  
  slice(match(c(
    "Root length density",
    "Root mass fraction",
    "Total biomass", 
    "Maximum root depth",
    "Mean root diameter", 
    "Specific root length"
  ),
  term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              stats.labels = FALSE,
              conf.int = TRUE,
              point.args = c(color = "red")
  )+ 
  
  labs(title = "B) With outliers",
       x = "Total Evapotranspiration (mL)", 
       y = NULL) + 
  
  theme(axis.text.y = element_blank())

WD <- plot_WD + plot_WD_O

# plot: total water capture - WW  ----------------------------------------------

plot_WW <- lm_total_ET_WW %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # remove random effect coefficient
  filter(term != "sd__(Intercept)") %>%
  
  # rename 
  mutate(term = case_when(
    term == "scale(rld)" ~ "Root length density",
    term == "scale(srl)" ~ "Specific root length",
    term == "scale(max_root_depth_cm)" ~ "Maximum root depth",
    term == "scale(plant_size)" ~ "Total biomass",
    term == "scale(mean_radius_mm)" ~ "Mean root diameter",
    term == "scale(rmf)" ~ "Root mass fraction",
    TRUE ~ term)) %>%
  
  slice(match(c(
    "Root length density",
    "Root mass fraction",
    "Total biomass", 
    "Maximum root depth",
    "Mean root diameter", 
    "Specific root length"
  ),
  term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              stats.labels = FALSE,
              conf.int = TRUE,
              point.args = c(color = "blue")
  )+ 
  
  labs(title = "A) Without outliers",
       x = "Total Evapotranspiration (mL)", 
       y = NULL)

plot_WW_O <- lm_total_ET_WW_O %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # remove random effect coefficient
  filter(term != "sd__(Intercept)") %>%
  
  # rename 
  mutate(term = case_when(
    term == "scale(rld)" ~ "Root length density",
    term == "scale(srl)" ~ "Specific root length",
    term == "scale(max_root_depth_cm)" ~ "Maximum root depth",
    term == "scale(plant_size)" ~ "Total biomass",
    term == "scale(mean_radius_mm)" ~ "Mean root diameter",
    term == "scale(rmf)" ~ "Root mass fraction",
    TRUE ~ term)) %>%
  
  slice(match(c(
    "Root length density",
    "Root mass fraction",
    "Total biomass", 
    "Maximum root depth",
    "Mean root diameter", 
    "Specific root length"
  ),
  term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              stats.labels = FALSE,
              conf.int = TRUE,
              point.args = c(color = "blue")
  )+ 
  
  labs(title = "B) With outliers",
       x = "Total Evapotranspiration (mL)", 
       y = NULL) + 
  
  theme(axis.text.y = element_blank())

WW <- plot_WW + plot_WW_O

# save to disk -----------------------------------------------------------------

ggsave(plot = WD,
       here(
         "output/figures/main", 
         "fig1-ET_WD.png"
       ),
       width = 7.5, height = 5.4, 
       device = "png")

ggsave(plot = WW,
       here(
         "output/figures/main", 
         "fig1-ET_WW.png"
       ),
       width = 7.5, height = 5.4, 
       device = "png")
