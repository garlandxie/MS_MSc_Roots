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
# Purpose of this R script: to conduct a principal component analysis

# libraries --------------------------------------------------------------------
library(here)
library(dplyr)
library(tidyr)
library(factoextra)
library(patchwork)

# import -----------------------------------------------------------------------
traits_EF <- readRDS(
  here("data/final",
       "traits_EF_clean_df.rds")
  )

# data cleaning ----------------------------------------------------------------

traits_EF <- traits_EF %>%
  mutate(
  spp = case_when(
    spp == "DASP" ~ "Danthonia spicata", 
    spp == "DEFL" ~ "Deschampsia flexuosa", 
    spp == "EMNI" ~ "Empetrum nigrum",
    spp == "PLMA" ~ "Plantago major", 
    spp == "SEAC" ~ "Sedum acre", 
    spp == "SEAL" ~ "Sedum album", 
    spp == "SOBI" ~ "Solidago bicolor", 
    spp == "VAMA" ~ "Vaccinium macrocarpon",
    spp == "FERU" ~ "Festuca rubra", 
    spp == "SYNO" ~ "Symphyotrichum novae-angliae", 
    spp == "SITR" ~ "Sibbaldiopsis tridentata", 
    spp == "SESE" ~ "Sedum sexangulae"
  )
)

# pca WD -----------------------------------------------------------------------

EF_WD <- traits_EF %>%
  ungroup() %>%
  filter(treatment == "WD") %>%
  drop_na 

pca_matrix_WD <- EF_WD %>%
  select(rmf, srl, rld, max_root_depth_cm, mean_radius_mm, plant_size) %>%
  prcomp(center = TRUE, scale = TRUE)

summary(pca_matrix_WD)  # info on cumulative proportion

# pca: WW ----------------------------------------------------------------------
EF_WW <- traits_EF %>%
  ungroup() %>%
  filter(treatment == "WW") %>%
  drop_na 

pca_matrix_WW<- EF_WW %>%
  select(rmf, srl, rld, max_root_depth_cm, mean_radius_mm, plant_size) %>%
  prcomp(center = TRUE, scale = TRUE)

summary(pca_matrix_WW)  # info on cumulative proportion 

# plot - pca WD ----------------------------------------------------------------

row.names(pca_matrix_WD$rotation) <- 
  c("Root Mass Fraction", 
    "Specific Root Length", 
    "Root Length Density", 
    "Maximum Rooting Depth", 
    "Mean Root Diameter", 
    "Total Biomass")

pca_WD <- fviz_pca_biplot(
  pca_matrix_WD, 
  repel = TRUE, 
  label = "var",
  col.ind = "blue", 
  col.var = "black", 
  alpha.ind = 0.3,
  habillage = EF_WD$spp,
  geom.var = c("point", "text"), 
  title = "Water-defict treatment") + 

  theme(legend.text = element_text(face = "italic"))

# plot - pca WW ----------------------------------------------------------------

row.names(pca_matrix_WW$rotation) <- 
  c("Root Mass Fraction", 
    "Specific Root Length", 
    "Root Length Density", 
    "Maximum Rooting Depth", 
    "Mean Root Diameter", 
    "Total Biomass")

pca_WW <- fviz_pca_biplot(
  pca_matrix_WW, 
  repel = TRUE, 
  label = "var",
  col.ind = "blue", 
  col.var = "black", 
  alpha.ind = 0.3,
  habillage = EF_WW$spp,
  geom.var = c("point", "text"), 
  title = "Well-watered treatment") +

  theme(legend.text = element_text(face = "italic"))

# save disk --------------------------------------------------------------------

ggsave(
  filename = here(
    "output/figures/supp", 
    "pca_WW.png"
    ),
  plot = pca_WW,
  device = "png",
  height = 5, 
  width  = 7
)

ggsave(
  filename = here(
    "output/figures/supp", 
    "pca_WD.png"
  ),
  plot = pca_WD,
  device = "png",
  height = 5, 
  width  = 7
)
