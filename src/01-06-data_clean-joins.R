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
# Purpose of this R script: to clean data to create a data frame 
# for statistical analyses

# libraries --------------------------------------------------------------------
library(here)
library(dplyr)
library(GGally)

# import -----------------------------------------------------------------------
root_depth    <- read.csv(here("data/working", "root_depth_clean.csv"))
pot_weight    <- read.csv(here("data/working", "pots_wide.csv"))
above_biomass <- read.csv(here("data/working", "above_clean.csv"))
below_biomass <- read.csv(here("data/working", "below_clean.csv"))
ij_rhizo      <- read.csv(here("data/working", "ij_rhizo_clean.csv"))

# calculate root mass fraction  ------------------------------------------------

rmf <- 
  left_join(
    above_biomass, 
    below_biomass, 
    by = c("block", "spp", "ind", "treatment")
    ) %>%
  mutate(rmf = total_root_g/(above_dry_g + total_root_g)) %>%
  select(block, spp, ind, treatment, rmf)

# calculate plant size ---------------------------------------------------------

plant_size <- 
  left_join(
    above_biomass, 
    below_biomass, 
    by = c("block", "spp", "ind", "treatment")
    ) %>%
  mutate(plant_size = above_dry_g + total_root_g) %>%
  select(block, spp, ind, treatment, plant_size)

# calculate specific root length  ----------------------------------------------

srl_mrd <- 
  left_join(
    ij_rhizo,
    below_biomass,
    by = c("block", "spp", "ind", "treatment")
    ) %>%
  mutate(srl = raw_total_length_m/fine_root_g) %>%
  select(block, spp, ind, treatment, srl, mean_radius_mm, rld)

# calculate ecosystem functions  -----------------------------------------------

ef <- pot_weight %>%
  
  # calculate green roof ecosystem functions for each module for each session
  mutate(water_capture = T2 - T1,
         water_loss    = T2 - T4) %>%
  
  # calculate green roof EFS for each module across all sessions
  group_by(block, spp, treatment, ind) %>%
  
  summarize(
    total_water_capture = sum(water_capture, na.rm = TRUE),
    total_water_loss    = sum(water_loss, na.rm = TRUE),
    avg_water_capture   = mean(water_capture, na.rm = TRUE),
    avg_water_loss      = mean(water_loss, na.rm = TRUE)
  )

# join -------------------------------------------------------------------------

# recursively perform left join operations
# final data set should have data on traits and ecosystem functions
list_dfs <- list(ef, rmf, srl_mrd, plant_size, root_depth)

traits_EF <- Reduce(left_join, list_dfs)

# remove outliers --------------------------------------------------------------

# SRL value for one of the SEAL specimens is extremely high

# remove outliers by subsetting the trait_EF 
# through the respective row indices (see below)
SEAL_out <- which(traits_EF$srl == max(traits_EF$srl, na.rm = TRUE))
traits_EF <- traits_EF[-c(SEAL_out),]

# correlation matrix -----------------------------------------------------------

# Pearson's product-moment correlation coefficient

cormatrix_WD <- traits_EF %>%
  filter(treatment == "WD") %>%
  rename(RMF = rmf,
         SRL = srl,
         MD  = mean_radius_mm, 
         RLD = rld, 
         MRD = max_root_depth_cm,
         TB  = plant_size) %>%
  ungroup() %>%
  select(RMF, 
         SRL,
         MD, 
         RLD, 
         MRD, 
         TB) %>%
  GGally::ggpairs() + 
  ggtitle("Water-deficit treatment") + 
  theme(axis.text.x = element_text(size = 10))

cormatrix_WW <- traits_EF %>%
  filter(treatment == "WW") %>%
  rename(RMF = rmf,
         SRL = srl,
         MD  = mean_radius_mm, 
         RLD = rld, 
         MRD = max_root_depth_cm,
         TB  = plant_size) %>%
  ungroup() %>%
  select(RMF, 
         SRL,
         MD, 
         RLD, 
         MRD, 
         TB) %>%
  GGally::ggpairs() + 
  ggtitle("Well-watered treatment") + 
  theme(axis.text.x = element_text(size = 10))

# save the data ----------------------------------------------------------------

saveRDS(
  traits_EF, 
  here(
    "data/final", 
    "traits_EF_clean_df.rds")
  )

ggsave(
  plot = cormatrix_WW, 
  filename = here(
    "output/figures",
    "fig-supp-cor-matrix_WW.png"
  ),
  device = "png",
  width  = 7,
  height = 7
)

ggsave(
  plot = cormatrix_WD, 
  filename = here(
    "output/figures",
    "fig-supp-cor-matrix_WD.png"
  ),
  device = "png",
  width  = 7,
  height = 7
)


