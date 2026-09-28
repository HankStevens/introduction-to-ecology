# Traits, Physiology, and the Global Spectrum of Plant Form and Function ----
# Source: 05-traits-physiology.qmd
# All R code chunks extracted in order of appearance.

# Traits, Physiology, and the Global Spectrum of Plant Form and Function ----

## Goals ----

## 1. Starting with the individual ----

## 2. What is a functional trait? ----

## 3. The search for universal axes of strategy ----

## 4. Díaz et al. (2016): scale and design ----

## 5. Two dimensions capture most of the variation ----

## 6. A constrained, "lumpy" trait space ----

### A toy example, with two made-up variables ----

#### fig-null-model-toy ----
#| label: fig-null-model-toy
#| echo: false
#| message: false
#| warning: false
#| fig-cap: "Four null models (blue circles) built from every combination of uniform/normal margins and correlated/independent structure, compared to a hypothetical “observed” pattern (orange triangles) with two tight clusters that are positively correlated both within and between clusters. Dashed outlines are convex hulls (a 2-D stand-in for Díaz et al.'s six-dimensional hypervolume); r is the Pearson correlation for each panel."
#| fig-width: 10
#| fig-height: 3

set.seed(444)
n <- 250

## Two hypothetical, made-up axes (arbitrary units, 0-10), chosen to echo
## the two real dimensions from the chapter: overall "size/structural"
## investment vs. "leaf economy" (cheap-fast vs. expensive-slow).
x_lab <- "Structural Investment (hypothetical, arbitrary units)"
y_lab <- "Leaf Economy (hypothetical, arbitrary units)"

## --- Null model 1: Uniform, Independent -----------------------------------
## Every combination of the two variables is equally likely; no relationship
## between them. (Diaz et al.'s null model 1: uniform, independent traits.)
nm1 <- dplyr::tibble(x = runif(n, 0, 10), y = runif(n, 0, 10))

## --- Null model 2: Uniform, Correlated -------------------------------------
## Same uniform marginal ranges as NM1, but now the two variables covary
## (via a Gaussian copula: correlate on the normal scale, then transform
## each margin back to Uniform(0,10)).
rho_null <- 0.6
z <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = matrix(c(1, rho_null, rho_null, 1), 2))
nm2 <- dplyr::tibble(x = pnorm(z[, 1]) * 10, y = pnorm(z[, 2]) * 10)

## --- Null model 3: Normal, Independent --------------------------------------
## Extreme values are rarer than intermediate ones (bell-shaped), but the two
## variables are still unrelated to one another.
nm3 <- dplyr::tibble(x = rnorm(n, 5, 1.6), y = rnorm(n, 5, 1.6))

## --- Null model 4: Normal, Correlated ---------------------------------------
## Bell-shaped margins, and now correlated -- the closest any null model
## gets to "realistic," and still the one Diaz et al. found real trait space
## fell short of.
z4 <- MASS::mvrnorm(n, mu = c(5, 5), Sigma = matrix(c(1.6^2, rho_null * 1.6^2,
                                                       rho_null * 1.6^2, 1.6^2), 2))
nm4 <- dplyr::tibble(x = z4[, 1], y = z4[, 2])

## --- "Observed": hypothetical real data ------------------------------------
## Two tight clusters, positively correlated both *within* each cluster and
## *between* them (i.e., the clusters themselves sit on the same upward
## trend) -- a "lumpy," bimodal occupation of trait space standing in for
## two viable evolutionary solutions that both coordinate the two variables
## in the same direction -- the two-hotspot pattern Diaz et al. describe.
rho_obs <- 0.85
clusterA <- MASS::mvrnorm(n / 2, mu = c(2.5, 2.5),
                           Sigma = matrix(c(0.7^2, rho_obs * 0.7^2,
                                             rho_obs * 0.7^2, 0.7^2), 2))
clusterB <- MASS::mvrnorm(n / 2, mu = c(7.5, 7.5),
                           Sigma = matrix(c(0.7^2, rho_obs * 0.7^2,
                                             rho_obs * 0.7^2, 0.7^2), 2))
obs <- dplyr::tibble(x = c(clusterA[, 1], clusterB[, 1]),
                      y = c(clusterA[, 2], clusterB[, 2]))

## --- Combine into one tidy data frame for facetting ------------------------
panel_levels <- c(
  "Null 1: Uniform, Independent",
  "Null 2: Uniform, Correlated",
  "Null 3: Normal, Independent",
  "Null 4: Normal, Correlated",
  "\"Observed\" (hypothetical real data)"
)

toy_data <- dplyr::bind_rows(
  nm1 |> dplyr::mutate(panel = panel_levels[1]),
  nm2 |> dplyr::mutate(panel = panel_levels[2]),
  nm3 |> dplyr::mutate(panel = panel_levels[3]),
  nm4 |> dplyr::mutate(panel = panel_levels[4]),
  obs |> dplyr::mutate(panel = panel_levels[5])
) |>
  dplyr::mutate(
    panel = factor(panel, levels = panel_levels),
    type  = dplyr::if_else(panel == panel_levels[5], "Observed", "Null model")
  )

## --- Per-panel summary stats: correlation + occupied area (2-D "hypervolume") ----
poly_area <- function(x, y) {
  # shoelace formula for the area of a convex hull
  h <- chull(x, y)
  x <- x[h]; y <- y[h]
  0.5 * abs(sum(x * dplyr::lead(y, default = y[1]) - dplyr::lead(x, default = x[1]) * y))
}

stats_df <- toy_data |>
  dplyr::group_by(panel) |>
  dplyr::summarise(
    r    = cor(x, y),
    area = poly_area(x, y),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    area_null1 = area[panel == panel_levels[1]],
    pct_of_null1 = round(100 * area / area_null1),
    label = paste0("r = ", sprintf("%.2f", r),
                    "\narea ~ ", round(area),
                    " (", pct_of_null1, "% of Null 1)")
  )

## --- Convex hulls, for the dashed outline on each panel --------------------
hulls_df <- toy_data |>
  dplyr::group_by(panel) |>
  dplyr::slice(chull(x, y)) |>
  dplyr::ungroup()

## --- Plot -------------------------------------------------------------------
pal_null     <- "#2a78d6"  # blue  -- null models
pal_observed <- "#eb6834"  # orange -- observed / real data

p_null_models <- ggplot2::ggplot(toy_data, ggplot2::aes(x, y)) +
  ggplot2::geom_polygon(data = hulls_df, ggplot2::aes(group = panel), fill = NA,
                         colour = "grey35", linewidth = 0.4, linetype = "22") +
  ggplot2::geom_point(ggplot2::aes(colour = type, shape = type), size = 1.7, alpha = 0.55, stroke = 0.3) +
  ggplot2::geom_text(data = stats_df, ggplot2::aes(x = 0.2, y = 10.6, label = label),
                      hjust = 0, vjust = 1, size = 3.1, colour = "grey20",
                      lineheight = 0.95, inherit.aes = FALSE) +
  ggplot2::facet_wrap(~panel, nrow = 1) +
  ggplot2::scale_colour_manual(values = c("Null model" = pal_null, "Observed" = pal_observed)) +
  ggplot2::scale_shape_manual(values = c("Null model" = 16, "Observed" = 17)) +
  ggplot2::coord_cartesian(xlim = c(0, 10), ylim = c(0, 11.4), clip = "off") +
  ggplot2::labs(x = x_lab, y = y_lab, colour = NULL, shape = NULL) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "grey92"),
    strip.text = ggplot2::element_text(face = "bold", size = 9),
    panel.spacing = grid::unit(1, "lines"),
    plot.margin = ggplot2::margin(t = 10, r = 14, b = 10, l = 14)
  )

# Also export a standalone copy of the figure for reuse outside the book
# (e.g. slides), matching this book's usual ggsave() convention.
ggplot2::ggsave("figs/null_model_toy_example.png", plot = p_null_models,
                 width = 15, height = 4.2, dpi = 220, bg = "white")

p_null_models

## 7. Traits as windows into physiology ----

## 8. From individual traits to communities and ecosystems ----

## 9. Why anchor a chapter on a single paper? ----

## 10. Data exercise: exploring trait trade-offs with BIEN ----

### bien-trait-exercise ----
#| label: bien-trait-exercise
#| eval: false

library(BIEN)
library(dplyr)
library(tidyr)
library(ggplot2)

## Step 1: see what traits are actually available. Run this yourself and
## look at the output -- the set of available traits can grow over time,
## so don't rely on a list someone else wrote down.
BIEN_trait_list()

## Step 2: choose TWO traits to compare, copied exactly from the
## trait_name column you just looked at.
my_traits <- c("whole plant height", "leaf area")   # <- replace with your choices

## Step 3: build your species list. Pick ONE approach (or combine them).

## 3a. By geographic region -- every species BIEN has recorded for a
## country or a U.S./Canadian state or province:
my_species <- BIEN_list_country(country = "Costa Rica")$scrubbed_species_binomial
# my_species <- BIEN_list_state(country = "United States", state = "Ohio")$scrubbed_species_binomial

## 3b. By taxonomic group -- skip the species list and query a genus or
## family directly instead (use this INSTEAD of Step 4 below):
# my_traits_data <- BIEN_trait_traitbygenus(genus = "Quercus", trait = my_traits)
# my_traits_data <- BIEN_trait_traitbyfamily(family = "Fabaceae", trait = my_traits)

## 3c. Combine region + taxonomy -- narrow a regional list (3a) down to
## one or more genera you're interested in:
# my_species <- my_species[grepl("^Quercus ", my_species)]

## Step 4: download your two traits for your species list.
## (Skip this step if you used option 3b instead.)
my_traits_data <- BIEN_trait_traitbyspecies(species = my_species, trait = my_traits)

## Step 5: look at what actually came back before assuming column names.
## BIEN returns long-format data (one row per species-trait record).
str(my_traits_data)
names(my_traits_data)

## Step 6: reshape to one row per species, one column per trait.
## Column names below (scrubbed_species_binomial, trait_name, trait_value)
## match current BIEN documentation -- confirm against Step 5's output.
traits_wide <- my_traits_data |>
  mutate(trait_value = as.numeric(trait_value)) |>
  group_by(scrubbed_species_binomial, trait_name) |>
  summarise(trait_value = mean(trait_value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = trait_name, values_from = trait_value) |>
  drop_na()

## Step 7: plot your two traits against each other.
ggplot(traits_wide, aes(x = .data[[my_traits[1]]], y = .data[[my_traits[2]]])) +
  geom_point(alpha = 0.7, colour = "#2a78d6", size = 2) +
  labs(x = my_traits[1], y = my_traits[2],
       title = paste("Trait relationship across", nrow(traits_wide), "species")) +
  theme_minimal()

## Step 8: quantify the relationship.
cor(traits_wide[[my_traits[1]]], traits_wide[[my_traits[2]]], use = "complete.obs")

## 11. Reflection questions ----

## References ----
