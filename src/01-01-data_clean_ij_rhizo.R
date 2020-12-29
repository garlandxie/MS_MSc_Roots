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
# Purpose of this R script: to clean data from IJ Rhizo output files 

# libraries --------------------------------------------------------------------
library(here)
library(dplyr)
library(janitor)
library(tidyr)
library(ggplot2)

# import -----------------------------------------------------------------------
ij_rhizo_raw <- 
  read.csv(
    here(
      "data/original", "ij_rhizo_output.csv")
    )

# check packaging --------------------------------------------------------------
str(ij_rhizo_raw)

# clean ------------------------------------------------------------------------

# change all "-" values to NA
ij_rhizo_raw[ij_rhizo_raw == "-"] <- NA

# process root traits: root length density, mean root diameter
ij_rhizo_clean <- ij_rhizo_raw %>%
  clean_names() %>%
  separate(col = pot_id, into = c("spp", "ind"), sep = "-") %>%
  mutate(block = as.factor(block),
         trt   = as.factor(trt)) %>%
  rename(treatment = trt) %>%
  mutate(mean_radius_mm = as.numeric(mean_radius_mm),
         raw_total_length_mm = as.numeric(raw_total_length_mm),
         volume_cm_cubed  = as.numeric(volume_cm_cubed),
         ind = as.numeric(ind),
         raw_total_length_m = raw_total_length_mm/1000,
         rld = raw_total_length_m/volume_cm_cubed) %>%
  select(block, 
         spp, 
         ind, 
         treatment, 
         raw_total_length_m, 
         mean_radius_mm, 
         rld)

# exploratory data analysis ----------------------------------------------------
  
# root diameter vs species identity by treatment
ij_rhizo_clean %>%
  ggplot(aes(x = spp, y = mean_radius_mm)) +
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  labs(x = "species identity",
       y = "root length (m)") +
  facet_wrap(~treatment) + 
  coord_flip()

# root length density vs species identity by treatment
ij_rhizo_clean %>%
  ggplot(aes(x = spp, y = rld)) +
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  labs(x = "species identity",
       y = "root length density (m per cubic cm)") +
  facet_wrap(~treatment) + 
  coord_flip()

# root length density vs species identity by block
ij_rhizo_clean %>%
  ggplot(aes(x = spp, y = rld)) +
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  labs(x = "species identity",
       y = "root length density (m per cubic cm)") +
  facet_wrap(~block, nrow = 1, ncol = 5) + 
  coord_flip()

# mean root diameter vs species identity by block
ij_rhizo_clean %>%
  ggplot(aes(x = spp, y = rld)) +
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  labs(x = "species identity",
       y = "mean root diameter (mm)") +
  facet_wrap(~block, nrow = 1, ncol = 5) + 
  coord_flip()

# save the data! ---------------------------------------------------------------
write.csv(ij_rhizo_clean, here("data/working", "ij_rhizo_clean.csv"))

