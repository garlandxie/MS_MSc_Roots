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
# Purpose of this R script: to clean data from above-ground biomass

# libraries --------------------------------------------------------------------
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

# import -----------------------------------------------------------------------
above_raw <- 
  read.csv(
    here("data/original",
         "above_biomass.csv"
         )
    )

# clean ------------------------------------------------------------------------

# change all "-" values to NA
# is there a tidy version to do this?
above_raw[above_raw == "-"] <- NA

# convert block and treatment into factor class
above_tidy <- above_raw %>%
  mutate(block = as.factor(block),
         treatment = as.factor(treatment)) %>%
  separate(col = pot_ID, into = c("spp", "ind"), sep = "-") %>%
  mutate(above_dry_g = as.numeric(above_dry_g),
         ind = as.numeric(ind)) %>%
  select(block, treatment, spp, ind, above_dry_g)

# exploratory data analysis

above_tidy %>%
  ggplot(aes(x = spp, y = above_dry_g)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  facet_wrap(~treatment) + 
  coord_flip() + 
  labs(x = "Species Identity",
       y = "Aboveground biomass (g)")

above_tidy %>%
  ggplot(aes(x = spp, y = above_dry_g)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  facet_wrap(~block) + 
  coord_flip() + 
  labs(x = "Species Identity",
       y = "Aboveground biomass (g)")

# save the data! ---------------------------------------------------------------
write.csv(
  above_tidy, 
  here(
    "data/working", 
    "above_clean.csv")
  )
