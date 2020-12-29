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
# Purpose of this R script: to clean data for pot weights

# libraries --------------------------------------------------------------------
library(here)
library(tidyverse)

# import -----------------------------------------------------------------------
pots_raw <- 
  read.csv(
    here(
      "data/original", 
      "pot_weight.csv")
    )

# check packaging --------------------------------------------------------------
glimpse(pots_raw)

# clean ------------------------------------------------------------------------

# change all "-" values to NA
pots_raw[pots_raw == "-"] <- NA

# convert block and treatment into factor class
pots_clean <- pots_raw %>%
  mutate(block = as.factor(block),
         treatment = as.factor(treatment)) %>%
  select(session, block, pot_ID, treatment, period, weight_grams) %>%
  separate(col = "pot_ID", into = c("spp", "ind"), sep = "-") %>%
  mutate(ind = as.numeric(ind)) %>%
  arrange(session, block, spp, ind, treatment, period)

# spread -----------------------------------------------------------------------

pot_wide <- pots_clean %>%
  spread(period, weight_grams) %>%
  mutate(across(c(T1, T2, T3, T4), as.numeric))

# exploratory data analysis ----------------------------------------------------

# histograms 

pot_wide %>%
  ggplot(aes(x = T1)) +
  geom_histogram() + 
  facet_wrap(treatment~block, nrow = 2, ncol = 5) + 
  labs(x = "Pre-water (T1)")

pot_wide %>%
  ggplot(aes(x = T2)) +
  geom_histogram() + 
  facet_wrap(treatment~block, nrow = 2, ncol = 5) + 
  labs(x = "Ten minute delay (T2)")

pot_wide %>%
  ggplot(aes(x = T3)) +
  geom_histogram() + 
  facet_wrap(treatment~block, nrow = 2, ncol = 5) +
  labs(x = "Pot Weight 24hr (T3)")

pot_wide %>%
  ggplot(aes(x = T4)) +
  geom_histogram() + 
  facet_wrap(treatment~block, nrow = 2, ncol = 5) + 
  labs(x = " Pot Weight 48hr (T4)")

pot_wide %>%
  mutate(capture = T2 - T1) %>%
  group_by(block, spp, ind, treatment) %>%
  summarize(total_capture = sum(capture, na.rm = TRUE)) %>%
  ggplot(aes(x = spp, y = total_capture)) + 
  geom_boxplot() + 
  facet_wrap(~treatment) + 
  coord_flip()

pot_wide %>%
  mutate(water_loss = T2 - T4) %>%
  group_by(block, spp, ind, treatment) %>%
  summarize(tot_water_loss = sum(water_loss, na.rm = TRUE)) %>%
  ggplot(aes(x = spp, y = tot_water_loss)) + 
  geom_boxplot() + 
  facet_wrap(treatment~block, nrow = 2, ncol = 5) + 
  coord_flip()

# save the data ----------------------------------------------------------------

write.csv(
  pot_wide, 
  here(
    "data/working", 
    "pots_wide.csv")
  )


