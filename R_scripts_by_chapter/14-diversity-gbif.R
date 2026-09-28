# GBIF: Why are there so many species, and where? ----
# Source: 14-diversity-gbif.qmd
# All R code chunks extracted in order of appearance.

# GBIF: Why are there so many species, and where? ----

## setup ----
#| label: setup
#| include: false
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

## Goals ----

## Preparatory Steps ----

## Background ----

### A gold standard, and why we usually can't use it ----

### Three things people mean by "diversity" ----

## Follow the Logic: why richness alone can fool you ----

### Richness depends on how hard you looked ----

### The latitudinal diversity gradient---and an exception ----

## Hypotheses and Predictions ----

## Predict before you compute ----

## Get R ready for work ----

### install ----
#| label: install
#| eval: false
## Run ONCE, then comment out.
install.packages(c("rgbif", "vegan"))

### libraries ----
#| label: libraries
library(rgbif)      # live access to the GBIF biodiversity repository
library(vegan)      # diversity, rarefaction
library(tidyverse)  # wrangling and ggplot2
library(broom)      # tidy model output

### palm-key ----
#| label: palm-key
#| cache: true
palm_key <- name_backbone(name = "Arecaceae")$usageKey
palm_key

## Q1 — Same latitude, different continents ----

### country-richness ----
#| label: country-richness
#| cache: true
countries <- tribble(
  ~country,      ~iso, ~continent,      ~abs_latitude,
  "Colombia",    "CO", "S. America",            4.0,
  "Malaysia",    "MY", "Asia",                  4.0,
  "Cameroon",    "CM", "Africa",                6.0,
  "Madagascar",  "MG", "Africa (island)",      19.0,
  "United States","US","N. America",           38.0,
  "United Kingdom","GB","Europe",              54.0
)

palm_richness <- function(iso) {
  occ_count(facet = "speciesKey", facetLimit = 99999,
            taxonKey = palm_key, country = iso) |> nrow()
}

palm_records <- function(iso) {
  occ_count(taxonKey = palm_key, country = iso)
}

countries <- countries |>
  mutate(richness = map_int(iso, palm_richness),
         records  = map_dbl(iso, palm_records))

countries |> select(country, continent, abs_latitude, richness, records)

## Walk me through this code ----

### fig-country-richness ----
#| label: fig-country-richness
#| fig-cap: "Recorded palm species richness by country. The three left-hand bars sit at nearly the same latitude but on different continents."
countries |>
  mutate(country = fct_reorder(country, richness)) |>
  ggplot(aes(x = country, y = richness, fill = continent)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Recorded palm species (GBIF)", fill = "Region") +
  theme_minimal()

## Follow the Logic: before you trust that African bar ----

## Q3 — How much is effort? Rarefaction ----

### fig-effort ----
#| label: fig-effort
#| fig-cap: "Recorded richness rises with sampling effort (number of records). Raw richness is partly a map of where people have looked."
ggplot(countries, aes(x = records, y = richness, label = country)) +
  geom_point(size = 2) +
  geom_text(vjust = -0.8, size = 3) +
  scale_x_log10() +
  labs(x = "Number of GBIF records (log scale)", y = "Recorded palm species") +
  theme_minimal()

### rarefy-data ----
#| label: rarefy-data
#| cache: true
get_species_counts <- function(iso, n = 3000) {
  occ_search(taxonKey = palm_key, country = iso,
             hasCoordinate = TRUE, limit = n)$data |>
    filter(!is.na(species)) |>
    count(species, name = "n")
}

co <- get_species_counts("CO")   # Colombia
cm <- get_species_counts("CM")   # Cameroon

### rarefy-compute ----
#| label: rarefy-compute
## Build named count vectors and rarefy both to a common sample size.
co_vec <- setNames(co$n, co$species)
cm_vec <- setNames(cm$n, cm$species)

common_n <- min(sum(co_vec), sum(cm_vec))

tibble(
  country        = c("Colombia", "Cameroon"),
  raw_species    = c(length(co_vec), length(cm_vec)),
  rarefied_species = c(rarefy(co_vec, sample = common_n),
                       rarefy(cm_vec, sample = common_n))
)

## Follow the Logic: what rarefaction does and does not fix ----

## Q2 — The gradient, done right ----

### gradient ----
#| label: gradient
#| cache: true
boxes <- tribble(
  ~lon,  ~lat,
  -110,   50,    # W Canada / N US
  -100,   35,    # S US
  -100,   20,    # Mexico
   -75,    5,    # Colombia
   -70,  -10,    # W Brazil / Peru
   -60,  -25     # S Brazil / Paraguay
)

box_wkt <- function(lon, lat, half = 5) {
  sprintf("POLYGON((%f %f,%f %f,%f %f,%f %f,%f %f))",
          lon - half, lat - half,  lon + half, lat - half,
          lon + half, lat + half,  lon - half, lat + half,
          lon - half, lat - half)
}

box_richness <- function(lon, lat) {
  occ_count(facet = "speciesKey", facetLimit = 99999,
            taxonKey = palm_key, geometry = box_wkt(lon, lat)) |> nrow()
}

boxes <- boxes |>
  mutate(abs_latitude = abs(lat),
         richness = map2_int(lon, lat, box_richness))

boxes

### fig-gradient ----
#| label: fig-gradient
#| fig-cap: "Palm species richness in equal-sized 10°×10° boxes vs. absolute latitude, with a fitted line."
ggplot(boxes, aes(x = abs_latitude, y = richness)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "darkorange") +
  labs(x = "Absolute latitude (degrees from equator)",
       y = "Palm species in box") +
  theme_minimal()

### gradient-model ----
#| label: gradient-model
fit <- lm(richness ~ abs_latitude, data = boxes)
tidy(fit)      # slope: species lost per degree of latitude
glance(fit)    # r.squared: how much of the variation latitude explains

## Walk me through the box query and the model ----

## Deliverables ----

## References ----
