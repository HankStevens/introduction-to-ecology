## Toy example: illustrating Diaz et al. (2016)'s four null models
## with two made-up variables (not real trait data), for teaching purposes.
##
## The real paper used six traits in six dimensions; here we use just two,
## so students can see, on ordinary x-y axes, what "uniform vs. normal" and
## "correlated vs. independent" null models look like -- and how real,
## evolved trait combinations (simulated here as clustered/"lumpy" data)
## occupy much less space, and are more tightly correlated, than any of them.

library(ggplot2)
library(dplyr)
library(MASS)   # for mvrnorm()

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
nm1 <- tibble(x = runif(n, 0, 10), y = runif(n, 0, 10))

## --- Null model 2: Uniform, Correlated -------------------------------------
## Same uniform marginal ranges as NM1, but now the two variables covary
## (via a Gaussian copula: correlate on the normal scale, then transform
## each margin back to Uniform(0,10)).
rho_null <- 0.6
z <- mvrnorm(n, mu = c(0, 0), Sigma = matrix(c(1, rho_null, rho_null, 1), 2))
nm2 <- tibble(x = pnorm(z[, 1]) * 10, y = pnorm(z[, 2]) * 10)

## --- Null model 3: Normal, Independent --------------------------------------
## Extreme values are rarer than intermediate ones (bell-shaped), but the two
## variables are still unrelated to one another.
nm3 <- tibble(x = rnorm(n, 5, 1.6), y = rnorm(n, 5, 1.6))

## --- Null model 4: Normal, Correlated ---------------------------------------
## Bell-shaped margins, and now correlated -- the closest any null model
## gets to "realistic," and still the one Diaz et al. found real trait space
## fell short of.
z4 <- mvrnorm(n, mu = c(5, 5), Sigma = matrix(c(1.6^2, rho_null * 1.6^2,
                                                 rho_null * 1.6^2, 1.6^2), 2))
nm4 <- tibble(x = z4[, 1], y = z4[, 2])

## --- "Observed": hypothetical real data ------------------------------------
## Two tight clusters, positively correlated both *within* each cluster and
## *between* them (i.e., the clusters themselves sit on the same upward
## trend) -- a "lumpy," bimodal occupation of trait space standing in for two
## viable evolutionary solutions that both coordinate the two variables in
## the same direction, e.g. a small/cheap cluster and a large/expensive
## cluster -- the two-hotspot pattern Diaz et al. describe.
rho_obs <- 0.85
clusterA <- mvrnorm(n / 2, mu = c(2.5, 2.5),
                     Sigma = matrix(c(0.7^2, rho_obs * 0.7^2,
                                      rho_obs * 0.7^2, 0.7^2), 2))
clusterB <- mvrnorm(n / 2, mu = c(7.5, 7.5),
                     Sigma = matrix(c(0.7^2, rho_obs * 0.7^2,
                                      rho_obs * 0.7^2, 0.7^2), 2))
obs <- tibble(x = c(clusterA[, 1], clusterB[, 1]),
              y = c(clusterA[, 2], clusterB[, 2]))

## --- Combine into one tidy data frame for facetting ------------------------
panel_levels <- c(
  "Null 1: Uniform, Independent",
  "Null 2: Uniform, Correlated",
  "Null 3: Normal, Independent",
  "Null 4: Normal, Correlated",
  "\"Observed\" (hypothetical real data)"
)

toy_data <- bind_rows(
  nm1 %>% mutate(panel = panel_levels[1]),
  nm2 %>% mutate(panel = panel_levels[2]),
  nm3 %>% mutate(panel = panel_levels[3]),
  nm4 %>% mutate(panel = panel_levels[4]),
  obs %>% mutate(panel = panel_levels[5])
) %>%
  mutate(
    panel = factor(panel, levels = panel_levels),
    type  = if_else(panel == panel_levels[5], "Observed", "Null model")
  )

## --- Per-panel summary stats: correlation + occupied area (2-D "hypervolume") ----
poly_area <- function(x, y) {
  # shoelace formula for the area of a convex hull
  h <- chull(x, y)
  x <- x[h]; y <- y[h]
  0.5 * abs(sum(x * dplyr::lead(y, default = y[1]) - dplyr::lead(x, default = x[1]) * y))
}

stats_df <- toy_data %>%
  group_by(panel) %>%
  summarise(
    r    = cor(x, y),
    area = poly_area(x, y),
    .groups = "drop"
  ) %>%
  mutate(
    area_null1 = area[panel == panel_levels[1]],
    pct_of_null1 = round(100 * area / area_null1),
    label = paste0("r = ", sprintf("%.2f", r),
                    "\narea ~ ", round(area),
                    " (", pct_of_null1, "% of Null 1)")
  )

## --- Convex hulls, for the dashed outline on each panel --------------------
hulls_df <- toy_data %>%
  group_by(panel) %>%
  slice(chull(x, y)) %>%
  ungroup()

## --- Plot -------------------------------------------------------------------
pal_null     <- "#2a78d6"  # blue  -- null models
pal_observed <- "#eb6834"  # orange -- observed / real data

p_null_models <- ggplot(toy_data, aes(x, y)) +
  geom_polygon(data = hulls_df, aes(group = panel), fill = NA,
               colour = "grey35", linewidth = 0.4, linetype = "22") +
  geom_point(aes(colour = type, shape = type), size = 1.7, alpha = 0.55, stroke = 0.3) +
  geom_text(data = stats_df, aes(x = 0.2, y = 10.6, label = label),
            hjust = 0, vjust = 1, size = 3.1, colour = "grey20",
            lineheight = 0.95, inherit.aes = FALSE) +
  facet_wrap(~panel, nrow = 1) +
  scale_colour_manual(values = c("Null model" = pal_null, "Observed" = pal_observed)) +
  scale_shape_manual(values = c("Null model" = 16, "Observed" = 17)) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 11.4), clip = "off") +
  labs(
    title = "Four null models vs. a hypothetical \"observed\" pattern",
    subtitle = "Two made-up variables -- not real trait data -- illustrating the logic behind Fig. 1 of Diaz et al. (2016)",
    x = x_lab, y = y_lab, colour = NULL, shape = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92"),
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey30", size = 10),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(t = 10, r = 14, b = 10, l = 14)
  )

ggsave("figs/null_model_toy_example.png", plot = p_null_models,
       width = 15, height = 4.2, dpi = 220, bg = "white",
       device = grDevices::png, type = "cairo")

print(stats_df, width = Inf)
