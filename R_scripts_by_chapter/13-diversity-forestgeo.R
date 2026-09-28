# ForestGEO Community Ecology: Why are there so many tree species, and where? ----
# Source: 13-diversity-forestgeo.qmd
# All R code chunks extracted in order of appearance.

# ForestGEO Community Ecology: Why are there so many tree species, and where? ----

## setup ----
#| label: setup
#| include: false
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

## Goals ----

## Preparatory Steps ----

## Background ----

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
## Run this ONCE, then comment it out.
install.packages("vegan")

### libraries ----
#| label: libraries
library(vegan)      # community ecology: diversity, rarefaction, accumulation
library(tidyverse)  # data wrangling and ggplot2
library(broom)      # tidy model output

data(BCI)           # load the Barro Colorado Island plot-by-species table

### peek ----
#| label: peek
dim(BCI)            # rows = 1-ha plots, columns = species
BCI[1:5, 1:6]       # a corner of the table: counts of trees, by species, by plot

## Walk me through this data table ----

## Measuring diversity at BCI ----

### Richness, plot by plot ----

#### richness ----
#| label: richness
richness <- specnumber(BCI)     # one richness value per 1-ha plot
summary(richness)               # how many species per hectare?

#### fig-richness-hist ----
#| label: fig-richness-hist
#| fig-cap: "Distribution of tree species richness across the fifty 1-ha BCI plots."
tibble(richness = richness) |>
  ggplot(aes(x = richness)) +
  geom_histogram(binwidth = 5, fill = "forestgreen", colour = "white") +
  labs(x = "Species per 1-ha plot", y = "Number of plots") +
  theme_minimal()

### Evenness and the rank--abundance curve ----

#### fig-rankabundance ----
#| label: fig-rankabundance
#| fig-cap: "Rank--abundance (Whittaker) plot for pooled BCI trees. A few species are very common; a long tail is rare."
pooled <- colSums(BCI)                      # total trees of each species, all plots

rank_ab <- tibble(species = names(pooled), abundance = pooled) |>
  filter(abundance > 0) |>
  arrange(desc(abundance)) |>
  mutate(rank = row_number())

ggplot(rank_ab, aes(x = rank, y = abundance)) +
  geom_line(colour = "grey40") +
  geom_point(size = 1) +
  scale_y_log10() +
  labs(x = "Species rank (1 = most abundant)",
       y = "Total abundance (log scale)") +
  theme_minimal()

## Follow the Logic: why a log scale, and why the long tail matters ----

### One number: Shannon and Simpson diversity ----

#### diversity-indices ----
#| label: diversity-indices
shannon <- diversity(BCI, index = "shannon")   # accounts for richness + evenness
simpson <- diversity(BCI, index = "simpson")   # prob. two random trees differ

tibble(shannon = shannon, simpson = simpson) |>
  summarise(across(everything(), list(mean = mean, sd = sd)))

#### hill ----
#| label: hill
## The "effective number of species": how many EQUALLY-common species
## would give the same Shannon diversity?
effective_species <- exp(shannon)
summary(effective_species)

## Follow the Logic: what `exp(H)` means ----

### Rarefaction: comparing fairly ----

#### rarefaction ----
#| label: rarefaction
stems <- rowSums(BCI)                 # total trees per plot
min_stems <- min(stems)               # thin every plot to this many
rarefied <- rarefy(BCI, sample = min_stems)

tibble(observed = richness, rarefied = rarefied) |>
  summarise(mean_observed = mean(observed),
            mean_rarefied = mean(rarefied))

### Does richness just track the number of stems? ----

#### richness-stems-model ----
#| label: richness-stems-model
model_df <- tibble(richness = richness, stems = rowSums(BCI))

fit <- lm(richness ~ stems, data = model_df)

tidy(fit)      # slope, intercept, and their uncertainty
glance(fit)    # r.squared: how much of richness variation stems explain

#### fig-richness-stems ----
#| label: fig-richness-stems
#| fig-cap: "Plot-level richness vs. number of stems, with a fitted line."
ggplot(model_df, aes(x = stems, y = richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, colour = "darkorange") +
  labs(x = "Stems per 1-ha plot", y = "Species per 1-ha plot") +
  theme_minimal()

## Walk me through `tidy()` and `glance()` ----

## Comparing forests across the globe ----

### sites-table ----
#| label: sites-table
sites <- tribble(
  ~site,             ~country,    ~continent,      ~abs_latitude, ~area_ha, ~species,
  "SCBI",            "USA",       "North America",          38.9,     25.6,       65,
  "BCI",             "Panama",    "Neotropics",              9.2,     50.0,      300,
  "Korup",           "Cameroon",  "Africa",                  5.1,     50.0,      493,
  "Pasoh",           "Malaysia",  "Asia",                    3.0,     50.0,      800,
  "Yasuni",          "Ecuador",   "Neotropics",              0.7,     50.0,     1150
)

sites

## A trap built into this table ----

### fig-latitude ----
#| label: fig-latitude
#| fig-cap: "Tree species richness vs. absolute latitude across forest plots. Point size shows plot area; colour shows continent. Mind the area caveat."
ggplot(sites, aes(x = abs_latitude, y = species,
                  colour = continent, size = area_ha)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = site), vjust = -1, size = 3, show.legend = FALSE) +
  labs(x = "Absolute latitude (degrees from equator)",
       y = "Species richness (in plot)",
       colour = "Continent", size = "Plot area (ha)") +
  theme_minimal() # + scale_y_log10()

## Follow the Logic: from deduction to abduction ----

## Deliverables ----

## References ----
