# Traits - Chap 5
# H. Stevens 
# 2026-0-10-07

library(BIEN)
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: confirm that your traits exist in BIEN ----
## BIEN_trait_list() returns every trait in the database (dozens of rows),
## so we filter it down to the handful we care about. Run this yourself and
## check that the names in the trait-pair table appear exactly as
## written. The set of available traits can change over time.
all_traits <- BIEN_trait_list()
all_traits |>
  filter(grepl("height|leaf area|nitrogen|seed mass", trait_name))

# Step 2: choose your two traits ----
## Copy one pair from the trait-pair table. Pair 1 shown here.
my_traits <- c("whole plant height",
               "leaf nitrogen content per leaf dry mass")

# one species list from the species-list table ----
  ## Type (or copy) the species names from your chosen list inside c(...),
  ## with each name in quotes. 
my_species <- c("Acer saccharum", "Acer rubrum", "Acer negundo",
                #                 "Acer saccharinum", "Aesculus glabra")