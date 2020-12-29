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
# Purpose of this R script: to clean data for below-ground biomass

# libraries --------------------------------------------------------------------
library(here)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)

# import -----------------------------------------------------------------------
belowground_bm_raw <- 
  read.csv(
    here(
      "data/original",
      "below_biomass.csv")
    )

# check package ----------------------------------------------------------------
str(belowground_bm_raw)

# data clean -------------------------------------------------------------------

# convert all "-" to NA's
belowground_bm_raw[belowground_bm_raw == "-"] <- NA

# calculate dry weight of fine roots below 2 mm
fine_root_g <- belowground_bm_raw %>% 
  separate(col = "pot_ID", into = c("spp", "ind"), sep = "-") %>% 
  rename(root_type = `fine.coarse`) %>% 
  mutate(ind = as.numeric(ind),
         below_dry_g = as.numeric(below_dry_g)) %>%
  filter(root_type == "fine") %>%
  group_by(block, spp, ind, treatment) %>%
  summarize(fine_root_g = sum(below_dry_g, na.rm = TRUE))

# calculate dry weight of below-ground biomass (coarse + fine)
total_root_g <- belowground_bm_raw %>% 
  tidyr::separate(col = "pot_ID", into = c("spp", "ind"), sep = "-") %>% 
  mutate(ind = as.numeric(ind),
         below_dry_g = as.numeric(below_dry_g)) %>%
  group_by(block, spp, ind, treatment) %>%
  summarize(total_root_g = sum(below_dry_g, na.rm = TRUE))

# join operation ---------------------------------------------------------------

# inner join
root_bm_clean <- inner_join(fine_root_g, total_root_g, 
                            by = c("block", 
                                   "spp", 
                                   "treatment",
                                   "ind")
                            ) 

# assign appropriate data types to cols
root_bm_clean <- root_bm_clean %>%
  ungroup() %>%
  mutate(block = factor(block),
         treatment = factor(treatment))


# exploratory data analysis ----------------------------------------------------

# note: might move this section to a different script

# box-plot: species identity vs fine root biomass
# there is an unbalanced design since:
# (1) some specimens died prior to the end of the experiment
# (2) a subset was taken for FERU and DEFL due to logistical constraints
root_bm_clean %>%
  ggplot(aes(x = spp, y = fine_root_g)) + 
  geom_boxplot() +
  geom_point(alpha = 0.2) +
  labs(x = "Species",
       y = "Fine root biomass (g)") + 
  facet_wrap(~treatment) + 
  coord_flip()

# box-plot: species identity vs total biomass
# there is an unbalanced design since:
# (1) some specimens died prior to the end of the experiment
# (2) a subset was taken for FERU and DEFL due to logistical constraints
root_bm_clean %>%
  ggplot(aes(x = spp, y = total_root_g)) + 
  geom_boxplot() +
  geom_point(alpha = 0.2) +
  labs(x = "Species",
       y = "Total root biomass (g)") + 
  facet_wrap(~treatment) + 
  theme_classic()
  
# save data --------------------------------------------------------------------
write.csv(
  root_bm_clean, 
  here(
    "data/working", 
    "below_clean.csv")
  )
