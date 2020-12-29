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
# Purpose of this R script: to clean data for rooting depth

# libraries --------------------------------------------------------------------
library(here)
library(dplyr)
library(tidyr)

# import -----------------------------------------------------------------------
depth_raw <- 
  read.csv(
    here(
      "data/original", 
      "max_root_depth.csv")
    )

# clean ------------------------------------------------------------------------

# change all "-" values to NA
depth_raw[depth_raw == "-"] <- NA

# convert block and treatment into factor class
depth_tidy <- depth_raw %>%
  mutate(block = as.factor(block),
         treatment = as.factor(treatment)) %>%
  separate(col = pot_ID, into = c("spp", "ind"), sep = "-") %>%
  mutate(max_root_depth_cm = as.numeric(max_root_depth_cm),
        ind = as.numeric(ind)) %>%
  select(block, 
         treatment, 
         spp, 
         ind, 
         max_root_depth_cm)

# save the data! ---------------------------------------------------------------

write.csv(
  depth_tidy,
  here(
    "data/working", 
    "root_depth_clean.csv")
  )
