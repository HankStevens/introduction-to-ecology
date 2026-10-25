# Basic Ecological Concepts and Data in R -- all R code chunks ----
# Author: Dr. Hank Stevens
# Extracted from the .qmd sources listed in _quarto.yml on 2026-10-17.
# Each chunk is headed '## <file> | chunk <n>: <label> ----'.
# Chunks marked [eval: false] or [include: false] were not run (or not shown) in the book.
# Quarto '#|' chunk options are retained as comments; '#| label:' is moved into the header.


# Preface: Preface (index.qmd) ----

## index | chunk 1: setup [include: false] ----
#  original chunk options: setup, echo=FALSE, include=FALSE
options(gargle_oauth_email = "stevenmh@miamioh.edu", 
        message=FALSE, 
        warning = FALSE)
library(tidyverse)
library(dagitty)
library(ggdag)
# library(rethinking)
library(googlesheets4)


# Chapter 1: Thinking about thinking about concepts (01-thinking.qmd) ----

## 01-thinking | chunk 1 ----
#| echo: false
#| message: false
library(tidyverse)
library(patchwork)

theme_set( theme_bw(base_size=12) )

## 01-thinking | chunk 2: fig-data ----
#| fig-cap: "I counted the number of arthropods on ten plants. The average of those counts (lambda) was 9.47. "
#| echo: false
#| fig-width: 4
#| fig-asp: 1
#| 


# data
sum_y <- 150
n  <- 15


# naive parameters based on data only
alpha.lik <- sum_y
beta.lik <- n

set.seed(1)
data <- tibble(bug_counts = rpois(n, lambda=alpha.lik/beta.lik))

lambda.hat <- mean( data$bug_counts )

xmin <- 0
xmax <- 15
ymin <- 0; ymax <- 0.8

data_hist <- ggplot(data = data, aes(x = bug_counts)) +
  geom_histogram(binwidth = 1, center = 0, color = "white") +
  coord_cartesian(xlim = c(xmin, xmax)) +
  # theme_bw() +
  labs(subtitle = "Raw data") + 
  annotate("text", x = 7, y = 4,
         label = sprintf("lambda == %.2f", lambda.hat ),
         parse = TRUE)

data_hist 

## 01-thinking | chunk 3: fig-learning ----
#| fig-cap: "Before I ever counted arthropods on plants, I believed there would be about 2 bugs on each plant (on average), and probably not more than 5 or 6. The *prior distribution* describes that initial guess and uncertainty. The normalized *likelihood* shown here is the probability of observing those counts, assuming a Poisson distribution and that the true mean is the observed mean. The *posterior* distribution shows us the combination (literally the product) of the prior and normalized likelihood. The posterior is our updated understanding of what the mean count number is for the entire population of plants I sampled from."
#| echo: false

# parameter priors
alpha.prior <- 2.2  # shape
beta.prior <- 1.1 # rate

# qgamma(0.975, shape=alpha.prior, rate=beta.prior)

# data
sum_y <- 150
n  <- 15


# naive parameters based on data only
alpha.lik <- sum_y + 1
beta.lik <- n

set.seed(1)
data <- tibble(bug_counts = rpois(n, lambda=alpha.lik/beta.lik))

# parameter posteriors
alpha.post <- alpha.prior + sum_y
beta.post <- beta.prior + n


xmin <- 0
xmax <- 15
ymin <- 0; ymax <- 0.8

data_hist <- ggplot(data = data, aes(x = bug_counts)) +
  geom_histogram(binwidth = 1, center = 0, color = "white") +
  coord_cartesian(xlim = c(xmin, xmax)) +
  theme_bw() +
  labs(subtitle = "Raw data")

prior_distr <- ggplot() +
  geom_function(fun=dgamma, args=list(shape = alpha.prior, rate = beta.prior),
                xlim=c(xmin, xmax), 
                n=1001, lty="dotted", color="#6FA8DC", linewidth=1) + 
  coord_cartesian(ylim=c(ymin, ymax)) + 
  labs(x="Expected average bug count", y="Probably density", subtitle="Prior understanding") +
  theme_bw() 

data_only_distr <- ggplot() +
  geom_function(fun=dgamma, args=list(shape = alpha.lik, rate = beta.lik),
                xlim=c(xmin, xmax), 
                n=1001, lty="dashed", color="#E59866", linewidth=1) +
  coord_cartesian(ylim=c(ymin, ymax)) +
  labs(x="Expected average bug count", y="Probably density", subtitle="Likelihood of data") +
  theme_bw() 

peak_x <- (alpha.post - 1) / beta.post
peak_y <- dgamma(peak_x - 0.5, shape = alpha.post, rate = beta.post)

all_distr <- ggplot() +
  geom_function(aes(color = "Prior", linetype = "Prior"),
                fun = dgamma, args = list(shape = alpha.prior, rate = beta.prior),
                xlim = c(xmin, xmax), n = 1001, linewidth = 1) +
  geom_function(aes(color = "Likelihood", linetype = "Likelihood"),
                fun = dgamma, args = list(shape = alpha.lik, rate = beta.lik),
                xlim = c(xmin, xmax), n = 1001, linewidth = 1) +
  geom_function(aes(color = "Posterior", linetype = "Posterior"),
                fun = dgamma, args = list(shape = alpha.post, rate = beta.post),
                xlim = c(xmin, xmax), n = 1001, linewidth = 1) +
  coord_cartesian(ylim = c(ymin, ymax)) +
  labs(x="Expected average bug count", y="Probably density", subtitle="All distributions") +
  annotate("text", x=5, y=.7, label="Posterior") + 
  annotate("curve",
           x = 5, y = .65, xend = peak_x-1, yend = peak_y - .05,
           curvature = 0.3, arrow = arrow(length = unit(0.2, "cm")),
           color = "black") +
  scale_color_manual(name = "Distribution",
                    breaks = c("Prior", "Likelihood", "Posterior"),
                    values = c(Prior = "#6FA8DC", Likelihood = "#E59866", Posterior = "#BAA0A1")) +
scale_linetype_manual(name = "Distribution",
                       breaks = c("Prior", "Likelihood", "Posterior"),
                       values = c(Prior = "dotted", Likelihood = "dashed", Posterior = "solid")) +
    guides(color = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1)) +
  theme_bw() 

design <- "
ABC
DEF
"

plot_spacer() + data_hist + guide_area() +
  prior_distr + data_only_distr + all_distr +
  plot_layout(design = design, guides = "collect")


# Chapter 2: Computer setup and first R script (02-computer-setup.qmd) ----

## 02-computer-setup | chunk 1 ----
getwd()

## 02-computer-setup | chunk 2 ----
## [title] e.g., My first R script in BIO 209W
## [Date]
## [your name]
## [the full path to your Rwork directory]

## 02-computer-setup | chunk 3 ----
#  original chunk options: results='hide'
-1:5

## 02-computer-setup | chunk 4 ----
-1:5

## 02-computer-setup | chunk 5 ----
a <- -1:5

## 02-computer-setup | chunk 6 ----
# show or 'print' a
a

## 02-computer-setup | chunk 7 ----
str(a)

## 02-computer-setup | chunk 8 ----
f <- 9
f

## 02-computer-setup | chunk 9 ----
2 < -3

## 02-computer-setup | chunk 10 ----
b <- runif(7, min=0, max=1)
b

## 02-computer-setup | chunk 11 ----
# multiply a and b
ab <- a * b
## Show a, b, and ab
a
b
ab

## 02-computer-setup | chunk 12 ----
d <- data.frame(a=a, b=b, a_times_b = ab)
d

## 02-computer-setup | chunk 13 ----
str(d)

## 02-computer-setup | chunk 14 ----
# export or write a file
write.csv(x=d, file="output/myDataframe.csv", row.names=FALSE)
# including row.names=FALSE prevents R from adding row names.

## 02-computer-setup | chunk 15 ----
# type='p' is for points only
plot(a, ab, type='p') 

## 02-computer-setup | chunk 16 [eval: false] ----
#  original chunk options: eval=FALSE
install.packages("ggplot2")

## 02-computer-setup | chunk 17 ----
library(ggplot2)

## 02-computer-setup | chunk 18 ----
## plot data and fit a curve
ggplot(data=d, aes(x=a, y=a_times_b)) + 
  geom_line() +
  labs(x="a", y="Product of a and b")

## 02-computer-setup | chunk 19 ----
ggsave("figs/myPlot.png")
ggsave("figs/myPlot.jpg", width=7, height = 3)
ggsave("figs/myPlot.pdf", width=3, height = 5)


# Chapter 3: Environmental Justice: How income relates to environment. (03-trees_and_people.qmd) ----

## 03-trees_and_people | chunk 1: setup [include: false] ----
#  original chunk options: setup, include=FALSE
options(gargle_oauth_email = "stevenmh@miamioh.edu", message=FALSE)
library(patchwork)

## 03-trees_and_people | chunk 2 ----
# if needed, install these packages
# install.packages("googlesheets4")
# install.packages("ggplot2")

# load these packages
library(googlesheets4)
library(ggplot2)

# Import data. 
# This code will require that you have permission to access our G-Sheet.
# You may be asked to select various options and/or sign in to your
# Miami Google account.

# The function read_sheet() requires a Google Sheet URL in quotes, and
# allows you to skip lines that are not data, 
# such as a table description.

d <- read_sheet("https://docs.google.com/spreadsheets/d/1uYCuTziJiLNvVOQ9Q65BxdWcNgDF6EnF8L1T3O_yATw/edit#gid=0",
                skip=2)

# Uncomment to show us the names of the variables
# names(d)

# make a scatterplot
plot1 <- ggplot(data=d, aes(x=Median_Income, 
                   y=Tree_canopy_cover, 
                   label=`City (only)`)) + geom_text() +
  labs(y="Tree cover (%)", x="Median income", title="No relation twixt tree cover and income?")

ggsave("figs/myCoverIncome_text.png", plot=plot1, width=10, height = 10)

plot2 <- ggplot(data=d, aes(x=Median_Income, 
                   y=Tree_canopy_cover, 
                   label=`City (only)`)) + 
  geom_point() +
  labs(y="Tree cover (%)", x="Median income", title="No relation twixt tree cover and income?")

# save the figure (dimensions in inches)
ggsave("figs/myCoverIncome_points.png", plot=plot2, width=7, height = 7)

## 03-trees_and_people | chunk 3: fig-data ----
#  original chunk options: echo=TRUE, fig.cap="The noisy but perhaps positive relation between percent of an location covered in trees and the median income of that location.", out.width="80%"
d10 <- d[1:10,]
myPlot <- ggplot(data=d10, aes(x=Median_Income, 
                   y=Tree_canopy_cover, 
                   label=`City (only)`)) + geom_text() +
  labs(y="Tree cover (%)", x="Median income", title="No relation twixt tree cover and income?")

myPlot


# Chapter 4: Physical Environment: A Fate of Earth (04-global_temp_anomalies_v2.qmd) ----

## 04-global_temp_anomalies_v2 | chunk 1 [eval: false] ----
#| eval: false
## install tidyverse
## tidyverse is a"universe" or collection of related R packages
## that have a particular coding style that helps a lot.
install.packages("tidyverse", repos = "https://cloud.r-project.org")

## 04-global_temp_anomalies_v2 | chunk 2 ----
## load R packages
# this makes these packages available to R in this session.
library(tidyverse)

## 04-global_temp_anomalies_v2 | chunk 3 ----
# read in data
temps <- read.csv(file = "data/global_surface_anomalies_2025.csv", # full file name
                  skip=3, # skip these lines because they contain metadata.
                  header = TRUE # Tell R that the first row is column namaes
                  ) # last parenthesis

## 04-global_temp_anomalies_v2 | chunk 4 ----
summary(temps)

## 04-global_temp_anomalies_v2 | chunk 5 ----
## get the min and max
min.anomaly <- min(temps$deg_C)
max.anomaly <- max(temps$deg_C)

## 04-global_temp_anomalies_v2 | chunk 6 ----
## use the min and the max
# note the two equal signs, a pipe, and then two more equals signs.
subset(temps, deg_C==min.anomaly | deg_C==max.anomaly )

## 04-global_temp_anomalies_v2 | chunk 7 ----
## Find the anomaly in the year you were born
subset(temps, year==1960) # use your year with two equal signs

## 04-global_temp_anomalies_v2 | chunk 8 ----
## we will create a plot object and stick it in "p"
p <- ggplot(data=temps, # the data frame to use
       ## aes() stand for "aesthetics"
       aes(x=year, y=deg_C), # define x and y variables to plot
       ) + # closing parenthesis and add a plus sign to add another graphical element
  geom_line() # the type of graph to plot using x and y

## 04-global_temp_anomalies_v2 | chunk 9 ----
p

## 04-global_temp_anomalies_v2 | chunk 10 ----
## add a trend line
p + geom_smooth(linetype="dashed")

## 04-global_temp_anomalies_v2 | chunk 11 ----
# first find the anomaly in 2002
anomaly.2002 <- temps$deg_C[temps$year==2002]
anomaly.2002
p + geom_smooth() +
  labs(
    y="Surface temp. anomaly (deg C)",
    subtitle="Relative to average 1951-1980"
    ) +
  geom_hline(yintercept = anomaly.2002, linetype=3)

## 04-global_temp_anomalies_v2 | chunk 12 ----
## Load a necessary R package
library(mgcv) # stands for "Mixed Generalized Additive Model Computation Vehicle"

## 04-global_temp_anomalies_v2 | chunk 13 ----
## Create the model
mod <- gamm(deg_C ~ s(year),  # "deg_C as a function of year
            data=temps, # data frame
          correlation = corCAR1(form=~year), # specify autocorrelation among samples
          method="REML" # fit using restricted maximum likelihood
          )

## 04-global_temp_anomalies_v2 | chunk 14 ----
## years for fit and prediction
years = data.frame(year=1880:2072)

## 04-global_temp_anomalies_v2 | chunk 15 ----
## include uncertainty with se.fit = TRUE
preds <- data.frame(
  predict(mod$gam, newdata=years, type="response", se.fit=TRUE)
  )

# combine columns of data frames
my.predictions <- cbind(years, preds)

## 04-global_temp_anomalies_v2 | chunk 16 ----
# first three lines , and all columns
my.predictions[1:3, ]

## 04-global_temp_anomalies_v2 | chunk 17 ----
### Create 95% confidence intervals
crit.t.low <- qt(0.025, df = df.residual(mod$gam))
crit.t.high <- qt(0.975, df = df.residual(mod$gam))

### this requires that you loaded the tidyverse package
my.predictions <- my.predictions %>% # my data
  ## transform the data set to include two new columns
  transform(
    upper.conf.lim=fit + (crit.t.high * se.fit),
    lower.conf.lim=fit + (crit.t.low * se.fit))
###
## view the first three rows
my.predictions[1:3, ]

## 04-global_temp_anomalies_v2 | chunk 18 [eval: false] ----
#  original chunk options: eval=FALSE, echo=TRUE
## basic data and x, y values
g <- ggplot(data = my.predictions, aes(x = year, y = fit)) +
  ## adding a wide region or band (a ribbon)
  geom_ribbon(aes(ymin = lower.conf.lim, ymax = upper.conf.lim,
                  x = year),
              alpha = 0.2, fill = "black") +
  ## add the expected local trend
  geom_line() +
  ## adding the points for each year
  geom_point(data = temps, mapping = aes(x = year, y = deg_C),
               inherit.aes = FALSE) +
  ## add a better label for the y axis
  labs(y = "Global temp. anomaly")
  ## simplify the image

## 04-global_temp_anomalies_v2 | chunk 19 ----
#  original chunk options: eval=TRUE, echo=FALSE
## basic data and x, y values
g2 <- ggplot(data = my.predictions, aes(x = year, y = fit)) +
  ## adding a wide region or band (a ribbon)
  geom_ribbon(aes(ymin = lower.conf.lim, ymax = upper.conf.lim,
                  x = year),
              alpha = 0.2, fill = "black") +
  ## add the expected local trend
  geom_line() +
  ## adding the points for each year
  geom_point(data = temps, mapping = aes(x = year, y = deg_C),
               inherit.aes = FALSE) +
  ## add a better label for the y axis
  labs(y = "Global temperature anomaly")  +
  ## make this different from what the students SHOULD turn in.
  theme_bw()

## 04-global_temp_anomalies_v2 | chunk 20 [eval: false] ----
#  original chunk options: eval=FALSE
g

## 04-global_temp_anomalies_v2 | chunk 21 ----
#  original chunk options: echo=FALSE
g2

## 04-global_temp_anomalies_v2 | chunk 22 [eval: false] ----
#  original chunk options: eval=FALSE
## Save a PNG file into your figs folder
## THIS IS THE GRAPH TO INSERT INTO YOUR ASSIGNMENT
ggsave("figs/Global_temps.png", # name for your figure
       plot=g, ## identify which graph you want to save,
       height=5, width=5)

## 04-global_temp_anomalies_v2 | chunk 23 ----
## First we take a subset of our data set.
a <- subset(temps, year==1950)
b <- subset(temps, year==2022)
increase <- b$deg_C - a$deg_C
increase

## 04-global_temp_anomalies_v2 | chunk 24 ----
## percent increase for Earth
perc.Earth <- increase / 13.9 * 100

## percent increase for humans
perc.fever <- 2/37 * 100

perc.Earth; perc.fever


# Chapter 5: Traits, Physiology, and the Global Spectrum of Plant Form and Function (05-traits-physiology.qmd) ----

## 05-traits-physiology | chunk 1: fig-null-model-toy ----
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

## 05-traits-physiology | chunk 2: bien-step-1 [eval: false] ----
#| eval: false

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

## 05-traits-physiology | chunk 3: bien-step-2 [eval: false] ----
#| eval: false

# Step 2: choose your two traits ----
## Copy one pair from the trait-pair table. Pair 1 shown here.
my_traits <- c("leaf area per leaf dry mass",
               "leaf nitrogen content per leaf dry mass")

## 05-traits-physiology | chunk 4: bien-step-3 [eval: false] ----
#| eval: false

# Step 3: get the data for your group (RUN ONLY ONE ROUTE) ----

## Route A: one genus from the genus table ----
## One call, no species list needed. Replace "Quercus" with your genus.
my_traits_data <- BIEN_trait_traitbygenus(genus = "Quercus", trait = my_traits)

## Route B: one species list from the species-list table ----
## Type (or copy) the species names from your chosen list inside c(...),
## with each name in quotes. The Ohio hardwood list is shown here, shortened
## to its first five species as a template; paste in the rest of yours.
# my_species <- c("Acer saccharum", "Acer rubrum", "Acer negundo",
#                 "Acer saccharinum", "Aesculus glabra")
# length(my_species)   # should be roughly 20-30, never hundreds
# my_traits_data <- BIEN_trait_traitbyspecies(species = my_species,
#                                             trait = my_traits)

## Route C: one country, trimmed to one genus from the country table ----
## Download ONE trait at a time for the whole country, then keep only your genus.
# country_data <- BIEN_trait_country(country = "Costa Rica", trait = my_traits[1])
# my_traits_data <- country_data |> filter(grepl("^Piper ", scrubbed_species_binomial))
## Then repeat for the second trait and combine with bind_rows():
# country_data2 <- BIEN_trait_country(country = "Costa Rica", trait = my_traits[2])
# my_traits_data <- bind_rows(my_traits_data,
#   country_data2 |> filter(grepl("^Piper ", scrubbed_species_binomial)))

## 05-traits-physiology | chunk 5: bien-step-4 [eval: false] ----
#| eval: false

# Step 4: look at what came back before assuming column names ----
## BIEN returns long-format data (one row per species-trait record).
nrow(my_traits_data)
names(my_traits_data)
table(my_traits_data$trait_name)    # how many records for each of your two traits?

## 05-traits-physiology | chunk 6: bien-step-5 [eval: false] ----
#| eval: false

# Step 5: reshape to one row per species, one column per trait ----
## Column names below (scrubbed_species_binomial, trait_name, trait_value)
## match current BIEN documentation -- confirm against Step 4's output.
traits_wide <- my_traits_data |>
  mutate(trait_value = as.numeric(trait_value)) |>
  group_by(scrubbed_species_binomial, trait_name) |>
  summarise(trait_value = mean(trait_value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = trait_name, values_from = trait_value) |>
  drop_na()

nrow(traits_wide)    # number of species with BOTH traits
## If this is fewer than about 10, choose a different genus, list, or trait pair.

## 05-traits-physiology | chunk 7: bien-step-6 [eval: false] ----
#| eval: false

# Step 6: plot your two traits against each other ----
ggplot(traits_wide, aes(x = .data[[my_traits[1]]], y = .data[[my_traits[2]]])) +
  geom_point(alpha = 0.7, colour = "#2a78d6", size = 2) +
  labs(x = my_traits[1], y = my_traits[2],
       title = paste("Trait relationship across", nrow(traits_wide), "species")) +
  theme_minimal()
ggsave("figs/my_trait_relationship.png", width = 5, height = 5)

## 05-traits-physiology | chunk 8: bien-step-7 [eval: false] ----
#| eval: false

# Step 7: quantify the relationship ----
cor(traits_wide[[my_traits[1]]], traits_wide[[my_traits[2]]], use = "complete.obs")
## Trait values often span orders of magnitude; if your plot looks like a
## "comet" crowded in one corner, try the correlation on log10-transformed values:
cor(log10(traits_wide[[my_traits[1]]]), log10(traits_wide[[my_traits[2]]]))


# Chapter 6: Behavior: Extra-pair copulation in a monogamous species (06-extrapair_copulation.qmd) ----

## 06-extrapair_copulation | chunk 1 ----
#| echo: false
a <- read.csv("data/epr_209.csv")
a[1:4,]
rm(d209)

## 06-extrapair_copulation | chunk 2 [eval: false] ----
#| eval: false
getwd()

## 06-extrapair_copulation | chunk 3 ----
library(tidyverse)

## 06-extrapair_copulation | chunk 4 ----
d209 <- read.csv("data/epr_209.csv")
str(d209)

## 06-extrapair_copulation | chunk 5 ----
d209 <- d209 %>%
  mutate(
    epc = ifelse(pairtype=="extra", 1, 0)
  )
str(d209)

## 06-extrapair_copulation | chunk 6 [eval: false] ----
#| eval: false
library(tidyverse)

## 06-extrapair_copulation | chunk 7 ----
glimpse(mpg)

## 06-extrapair_copulation | chunk 8: bp1 ----
#| fig.cap: "Box-and-whisker plot of highway mileage (hwy) vs. car type (class), with explicit labels. A boxplot shows the distribution of data, by groups."
ggplot(mpg, aes(x = class, y = hwy)) + geom_boxplot() + 
  labs(x="type of car", y="highway mileage")

## 06-extrapair_copulation | chunk 9: sp1 ----
#| fig.cap: "Scatterplot of highway mileage vs. city mileage (miles per gal)."
ggplot(mpg, aes(x=cty, y=hwy)) + geom_point() + 
  labs(x="city mileage", y="highway mileage")

## 06-extrapair_copulation | chunk 10: splm ----
#| fig.cap: "Scatterplot with a fitted straight line, fitting a linear model."
ggplot(mpg, aes(x=cty, y=hwy)) + geom_point() + geom_smooth(method="lm")

## 06-extrapair_copulation | chunk 11: sploess ----
#| fig.cap: "Scatterplot with a flexibly fitted line, using local polynomial regression fitting."
## for a scatterplot with a fitted curved line 
ggplot(mpg, aes(x=cty, y=hwy)) + geom_point() + geom_smooth()

## 06-extrapair_copulation | chunk 12 ----
mpg2 <- mpg %>% 
  mutate(
    prob.front.wheel.drive = ifelse(drv=="f", 1, 0)
  )

## 06-extrapair_copulation | chunk 13: bp2 ----
#| fig.cap: "Higher mileage cars tend to have front-wheel drive. Graph shows the probability that a car with a particular city mileage is front wheel drive. The curve is the result of a type of generalized linear model (glm) called *logistic regression* which assumes binomial data."
ggplot(mpg2, aes(cty, prob.front.wheel.drive)) + 
  geom_point() + 
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"))


# Chapter 7: Density-independent Population Growth: Humans on the rise (07-DI-pop-growth.qmd) ----

## 07-DI-pop-growth | chunk 1 ----
#  original chunk options: echo=TRUE
library(tidyverse) # for readr, dplyr and others
library(patchwork) # to combine ggplot2 figures

## 07-DI-pop-growth | chunk 2 ----
d <- read_csv("data/HumanPopData-2023-01-01.csv", 
              skip=3 # skip three lines of metadata
              )
glimpse(d)

## 07-DI-pop-growth | chunk 3: fig-p1 ----
#| fig-cap: "The human population has increased over time. Note the common log scale on the y-axis. Note that years before the current era (BCE)  are negative numbers, while numbers in the current era (CE) are positive."
d %>% 
  ggplot(aes(x=Year, y=est.N)) + 
  geom_point() + geom_line() + 
  scale_y_log10() + labs(y="Estimated human population size") 

## 07-DI-pop-growth | chunk 4 ----
## Get the right time points and find h
t <- 100
# recall h = (t+h) - t
t.plus.h <- 200
h  <- t.plus.h - t
t; t.plus.h; h

## 07-DI-pop-growth | chunk 5 ----
## in base R
Nt <- d$est.N[d$Year == t]
Nth <- d$est.N[d$Year == t.plus.h]
Nt; Nth

## 07-DI-pop-growth | chunk 6 ----
## using tidyverse syntax
Nt <-  d %>% # use d
  filter(Year == t) %>%  # use just values in this year
   select(est.N) %>% # select just N
  as.numeric() # convert from a data frame to a numeric variable

Nth <-  d %>% # use d
  filter(Year == t.plus.h) %>%  # ue just values in this year
   select(est.N) %>% # select just N
  as.numeric() # convert from a data frame to a numeric variable
Nt; Nth

## 07-DI-pop-growth | chunk 7 ----
# the natural log of the ratio of population sizes
logNth.over.Nt <- log(Nth / Nt)
logNth.over.Nt

## 07-DI-pop-growth | chunk 8 ----
# finding r
r <- logNth.over.Nt / h
r

## 07-DI-pop-growth | chunk 9 ----
# create an empty vector of missing values.
r <- rep(NA, nrow(d))

## 07-DI-pop-growth | chunk 10 ----
# Do the following steps FOR each row of our data.
# Note we start with row 2, and use row 2 and row 1....
for(i in 2:nrow(d)) {
  # the second, more recent, year
  t.plus.h <- d$Year[i]
  # the previous year
  t <- d$Year[i-1]
  
  # the time interval in years
  h <- t.plus.h - t
  
  # the more recent N
  Nth <- d$est.N[i]
  # the previous N
  Nt <- d$est.N[i-1]
  
  # the natural log ratio 
  logNth.over.Nt <- log(Nth/Nt)
  
  # r, which we put in the appropriate slot, year t
  r[i-1] <- logNth.over.Nt/h
  
}

# last, let's check the result
glimpse(r)

## 07-DI-pop-growth | chunk 11 ----
d.r <- cbind(d, r=r)

## 07-DI-pop-growth | chunk 12 ----
d.r[257:259,]

## 07-DI-pop-growth | chunk 13 ----
d.r <- d.r[1:258,]

## 07-DI-pop-growth | chunk 14: fig-n1 ----
#| fig.cap: "Estimates of per capita growth are positive for nearly every time interval since the agricultural revolution."
#| fig-show: 'hold'
#| out-width: '65%'

# population size, using the first data frame
ggplot(data=d.r, aes(x=Year, y=r)) + geom_line() +
  geom_point() + labs(title="Per capita growth rate")

## 07-DI-pop-growth | chunk 15 ----
#| fig-show: 'hide'

# filter in all years greater than or equal to 1
d.r.y1 <- d.r %>% filter(Year >= 1)

ggplot(data=d.r.y1, aes(x=Year, y=est.N)) + geom_line() +
  geom_point() + labs(title="Population Size")

ggplot(data=d.r.y1, aes(x=Year, y=r)) + geom_line() +
  geom_point() + labs(title="Per capita growth rate since 1 CE")

## 07-DI-pop-growth | chunk 16: fig-n2 ----
#| echo: FALSE
#| fig.cap: "In the current era, per capita growth remains mostly positive, and then rises sharply and begin to fluctuate dramatically after 1500."
p1 <- ggplot(data=d.r.y1, aes(x=Year, y=est.N)) + geom_line() +
  geom_point() + labs(title="Population Size")
p2 <- ggplot(data=d.r.y1, aes(x=Year, y=r)) + geom_line() +
  geom_point() + labs(title="Per capita growth rate since 1 CE")
p1 / p2 

## 07-DI-pop-growth | chunk 17: fig-rn ----
#| echo: FALSE
#| fig.cap: "Per capita growth rate vs. population size, since 1 CE after 1500. What do you make of this? :)"
ggplot(data=d.r.y1, aes(x=est.N, y=r, color=Year)) + 
  geom_point() 

## 07-DI-pop-growth | chunk 18: answersDIgrowth [eval: false] ----
#| echo: false
#| message: false
#| results: 'hide'
#| eval: false

library(tidyverse)
d <- read.csv("data/HumanPopData-2023-01-01.csv", 
              skip=3 # skip three lines of metadata
              )

d2 <- d %>% filter(Year %in% c(-10000, -9000, 0, 100, 1960, 1961, 2020, 2021))
r <- numeric(4)
for(i in 1:4){
  th <- i*2
  t <- th-1
  r[i] <- log(d2$est.N[th]/d2$est.N[t])/(d2$Year[th] - d2$Year[t])
}


d3 <- data.frame(t = d2$Year[c(1,3,5,7)],
                 Nt = d2$est.N[c(1,3,5,7)],
                 Nth = d2$est.N[c(1,3,5,7)+1],
                 r = r
                 )
d3
r2020 <- d3 %>% filter(t==2020) %>% select(r) %>%
  as.numeric()

n2020 <- d3 %>% filter(t==2020) %>% select(Nt) %>%
  as.numeric()
n2050 <- n2020*exp(r2020 * 30)
d_2050 <- c(2050, n2050, NA, NA)
names(d_2050) <- names(d3)

d4 <- rbind(d3, d_2050)
d4
write_csv(d4, "Human_pop_growth.csv")


# Chapter 8: Structured demography: How a graduate student helped save the loggerhead sea turtle (08-structured-pop-growth_v2.qmd) ----

## 08-structured-pop-growth_v2 | chunk 1 ----
getwd()

## 08-structured-pop-growth_v2 | chunk 2 ----
## for graphics and data manipulation
library(tidyverse)

## 08-structured-pop-growth_v2 | chunk 3 ----
A <- matrix(
  c(0,      0,      0,      0,    127,      4,    80,
    0.6747, 0.7370, 0,      0,      0,      0,     0,
    0,      0.0486, 0.6610, 0,      0,      0,     0,
    0,      0,      0.0147, 0.6907, 0,      0,     0,
    0,      0,      0,      0.0518, 0,      0,      0,
    0,      0,      0,      0,      0.8091, 0,      0,
    0,      0,      0,      0,      0,      0.8091, 0.8089), 
nrow=7, ncol=7, byrow=TRUE)
rownames(A) <- colnames(A) <- c("H","J_s","J_l", "sub", "B_1", "B_2", "M")
A

## 08-structured-pop-growth_v2 | chunk 4 ----
original_lambda <- Re( eigen(A)$values[1] )
original_lambda

## 08-structured-pop-growth_v2 | chunk 5 ----
## new matrix 
A1 <- A 

## Change just the hatchling survival/growth
A1[2,1] <- 0.999
## look at the new matrix
A1

## 08-structured-pop-growth_v2 | chunk 6 ----
## using Re() extracts just the real part of the eigenvalue, and not the imaginary part
Re( eigen(A1)$values[1] )

## 08-structured-pop-growth_v2 | chunk 7 ----
## new matrix with NEW NAME, A2
## make sure you start with the ORIGINAL matrix A 
A2 <- A

## Change just the stasis and growth for small juveniles
A2[2,2] <- 0.857 # stasis
A2[3,2] <- 	0.142 # growth
A2

## 08-structured-pop-growth_v2 | chunk 8 ----
Re( eigen(A2)$values[1] )

## 08-structured-pop-growth_v2 | chunk 9: tbl-changes ----
#| tbl-cap: "New values to use  in the loggerhead sea turtle transition matrix, changing only one stage at a time. These changes reflect 99.9% annual survival. Stasis values belong on the diagonal, such as `A[5,5] <- 0`, and growth values belong below the diagonal, such as `A[6,5]<-0.999`. Start with the original matrix, make a copy, and then change only one stage at a time."
#| echo: false

d <- c(1, 7, 8, 6, 1, 1, 30)

## Calculate survivorship in the 7-year stage
## confirm that we get what Crouse got


## increase annual survival to 99%
p = 0.999

P <- round( (1-p^(d-1))/(1-p^d)*p, 3)
G <- round(p^d * (1-p)/(1-p^d), 3)
G[7] <- 0
changes <- data.frame(
  Stage=c("H","J_s","J_l", "sub", "B_1", "B_2", "M"),
           Stasis = P, Growth=G)
knitr::kable(changes, booktabs=FALSE)

## 08-structured-pop-growth_v2 | chunk 10 [eval: false] ----
#| eval: false
#| echo: false
#| 

lambda <- numeric(7)
for(i in 1:6){
M <- A
M[i:(i+1), i] <- as.numeric( changes[i, 2:3] )
lambda[i] <- Re( eigen(M)$values[1] )
}
M <- A
M[7,7] <- as.numeric( changes[7, 2] )
lambda[7] <- Re( eigen(M)$values[1] )

cbind(changes,lambda)

## 08-structured-pop-growth_v2 | chunk 11 [eval: false] ----
#| eval: false
#| echo: false
#| results: 'hide'
A <- matrix(
  c(0,      0,      0,      0,    127,      4,    80,
    0.6747, 0.7370, 0,      0,      0,      0,     0,
    0,      0.0486, 0.6610, 0,      0,      0,     0,
    0,      0,      0.0147, 0.6907, 0,      0,     0,
    0,      0,      0,      0.0518, 0,      0,      0,
    0,      0,      0,      0,      0.8091, 0,      0,
    0,      0,      0,      0,      0,      0.8091, 0.8089), 
nrow=7, ncol=7, byrow=TRUE)
rownames(A) <- colnames(A) <- c("H","J_s","J_l", "sub", "B_1", "B_2", "M")
diagram::plotmat(A, pos=c(7), curve=.4, box.size=.04)

## perfect annual survival
perfect_s <- function(duration=1, p=0.9999999){
  P <- (1-p^(duration-1))/(1-p^duration)*p
  G <- p^duration * (1-p)/(1-p^duration)
return(c(P=P, G=G))
}

stage.durations <- c(1,6,7,5,1,1,1000000)
PG <- sapply(stage.durations, perfect_s )
PG
lambda2 <- numeric(7)
i <- 1
for(i in 1:7){
  row <- i
  col <- i
  A2 <- A
  A2[row, col] <- PG[1,i]
  if(i < 7){
    A2[row+1, col] <- PG[2,i]
  } 
  lambda2[i] <- Re( eigen(A2)$values[1] )

}


# Elasticity
lam <- eigen(A)$value[1]
r.dev <- eigen(A)$vectors[,1]
l.dev <- eigen( t(A) )$vectors[,1]
r.dev <- r.dev/sum(r.dev)
l.dev <- l.dev/l.dev[1]
a <- matrix(l.dev, nrow=7) %*% matrix(r.dev, nrow=1)
s <- a/sum(l.dev*r.dev) 
e <- Re(A/lam * s)

round(e,3)
round(lambda2,3)


# Chapter 9: Density-dependence in population growth (09-density-dep-pop-growth.qmd) ----

## 09-density-dep-pop-growth | chunk 1: fig-logistic ----
#| echo: false
#| fig-cap: "Logistic growth in a hypothetical population (parameters: $r=0.25$, $\\alpha = 1/27000$)."
#| message: false
#| warning: false
#| out-width: "75%"
library(primer)
a <- 1/27000 ; r <- 0.25; N <- c(N=1400)
p <- c(r=r, alpha=a)
out <- as.data.frame(
  ode(y=N, time=seq(0,30), func=clogistic, parms = p)
)
ggplot(out, aes(x=time, y = N)) + geom_line() + theme_bw()

## 09-density-dep-pop-growth | chunk 2 ----
## Define K, N0, the number of generations
K <- 100
gens <- 50
## make numeric vector for each N in each generation, plus one more
N <- numeric(gens)

## Start the population out at this number of individuals
N[1] <- 45

## 09-density-dep-pop-growth | chunk 3 ----
############################################
## THIS IS WHERE YOU PICK THE VALUE OF rd YOU WANT
rd <- 0.2
############################################


## project the population
for(t in 2:gens) { 
  N[t] <- N[t-1] + N[t-1]*rd*(1-N[t-1]/K) 
}

# store this result in a data frame
my.N <- data.frame(year=1:gens, N=N)

#for kicks, look at rows 1-3.
my.N[1:3,]

## 09-density-dep-pop-growth | chunk 4 ----
## graph your data
ggplot(data=my.N, aes(x=year, y=N )) + 
  geom_point() + 
  geom_line() +
  labs(title = "Discrete Logistic Growth",
       subtitle=paste("rd =", rd))

## save your graph USING A UNIQUE NAME
ggsave("figs/dynamics-rd-0.2.png", height=5, width=6)


# Chapter 10: Direct species interactions: understanding mechanisms of invasion in mosquitoes (10-direct-interactions.qmd) ----

## 10-direct-interactions | chunk 1: tbl-mosquito ----
#| tbl-cap: "Lotka-Volterra growth and competition parameters of *A. triseriatus* and *A. albopictus* in water from tree holes and from tires, based on Livdahl and willey (1991). Values of $r$ and $K$ refer to the species on the respective line. Values for alpha or beta refer to the effect of the competitor. For instance, K = 42.83 is the carrying capacity of A. triseriatus in tree hole water, and alpha = 0.4167 is the effect of A. albopictus on A. triseriatus, in tree hole water."
#| echo: false
get.comp <- function(ri, bi, bj){
  r=ri
  Ki <- -ri/bi
  a.B <- -bj*Ki/ri
  c(r = r, Ki=Ki, a.B = a.B)
}
#Tree hole:
tree.hole.At <- get.comp(0.0514, -0.0012, -0.0005)
tree.hole.Aa <- get.comp(0.0798, -0.0015, -0.0011)
tire.At <- get.comp(0.0591, -0.0018, -0.0015)
tire.Aa <- get.comp(0.0904, -0.0020, -0.0005)
d <- as.data.frame( 
  rbind(tree.hole.At, tree.hole.Aa, tire.At, tire.Aa) )
row.names(d) <- c("A. triseriatus (in treehole water)",
                  "A. albopictus (in treehole water)",
                  "A. triseriatus (in tire water)",
                  "A. albopictus (in tire water)")
colnames(d) <- c("r", "K", "alpha or beta")


knitr::kable(d,
  digits=c(4,4,4),
  align='c',
  booktabs=TRUE)

## 10-direct-interactions | chunk 2 ----
#| echo: false
#| results: 'hide'

## in tree hole water...
# Can Aa invade?
tree.hole.Aa["Ki"] > tree.hole.Aa["a.B"]*tree.hole.At["Ki"]
# can At invade?
tree.hole.At["Ki"] > tree.hole.At["a.B"]*tree.hole.Aa["Ki"]

## in tire water
# Can Aa invade?
tire.Aa["Ki"] > tire.Aa["a.B"]*tire.At["Ki"]
# Can At invade?
tire.At["Ki"] > tire.At["a.B"]*tire.Aa["Ki"]

## 10-direct-interactions | chunk 3 [eval: false] ----
#| eval: false
#| echo: false
library(tidyverse)
out <- with(d,
     {
       A.triseriatus.treehole <- c(K[1], K[1]/`alpha or beta`[1])
        A.albopictus.treehole <- c(K[2], K[2]/`alpha or beta`[2])
         A.triseriatus.tire <- c(K[3], K[3]/`alpha or beta`[3])
          A.albopictus.tire <- c(K[4], K[4]/`alpha or beta`[4])
          out <- rbind(A.triseriatus.treehole, 
                       A.albopictus.treehole, 
                       A.triseriatus.tire, 
                       A.albopictus.tire)
          colnames(out) <- c("Ki", "Ki/coef")
          as.data.frame(out)
     })
out
out2 <- data.frame(
  Water_type = c("Treehole", "Treehole", "Treehole", "Treehole",
                 "Tire", "Tire", "Tire", "Tire"),
  x=c(0, out[1,2], out[2,1], 0, 
      0, out[3,2], out[4,1], 0),
  
  y=c(out[1,1], 0, 0, out[2,2],
      out[3,1], 0, 0, out[4,2])
)
out2


 
{plot(out2[1:2,], type="l", 
     xlim=c(0, max(out2[,1])), ylim=c(0, max(out2[,2]))); 
  lines(out2[3:4,1], out2[3:4,2], lty=2)
title("Treehole Water")
}
  
{plot(out2[5:6,], type="l", 
     xlim=c(0, max(out2[,1])), ylim=c(0, max(out2[,2]))); lines(out2[7:8,1],out2[7:8,2], lty=2)
title("Tire Water")
}


# Chapter 11: Mutualisms: Do ants protect pea plants? (11-mutualisms.qmd) ----

## 11-mutualisms | chunk 1 [eval: false] ----
#| eval: false
setwd("C:/Users/chenjoe/BIO209W/Rwork")

## 11-mutualisms | chunk 2 ----
getwd()

## 11-mutualisms | chunk 3 ----
#| message: false
## This script is for chapter XX in Hank's Intro to Ecology primer.

## We always use...
library(tidyverse)
## or usually just ggplot2 and dplyr packages within the tidyverse.

## 11-mutualisms | chunk 4 ----
#| echo: false
d <- read.csv("data/ArthropodObservations.csv",
              skip=2)

## 11-mutualisms | chunk 5 [eval: false] ----
#| eval: false
## NOTE that we skip the first 2 lines because they are metadata
d <- read.csv("ArthropodObservations.csv",
              skip=2)

## 11-mutualisms | chunk 6 ----
# glimpse(d)
# or 
str(d)

## 11-mutualisms | chunk 7 [eval: false] ----
#| eval: false
ggplot(data=DATA, aes(x=IV_1, y=DV, color=IV_2)) + 
  geom_boxplot(notch = TRUE) + 
  facet_grid( IV_3 ~ IV_4)


# Chapter 12: Of wolves and moose, on Isle Royale (12-predator-prey.qmd) ----

## 12-predator-prey | chunk 1 ----
getwd()

## 12-predator-prey | chunk 2 ----
#| message: false
## This script is for chapter XX in Hank's Intro to Ecology primer.

## We always use...
library(tidyverse)
## or usually just ggplot2 and dplyr packages within the tidyverse.

## 12-predator-prey | chunk 3 [eval: false] ----
#| eval: false
## For this chapter we also use deSolve
install.packages("deSolve")

## 12-predator-prey | chunk 4 ----
library(deSolve)

## 12-predator-prey | chunk 5 ----
#| echo: false
d <- read.csv("data/isleRoyaleData.csv",
              skip=2)

## 12-predator-prey | chunk 6 [eval: false] ----
#| eval: false
## NOTE that we skip the two lines because they are metadata
d <- read.csv("isleRoyaleData.csv",
              skip=2)

## 12-predator-prey | chunk 7 ----
# glimpse(d)
# or 
str(d)

## 12-predator-prey | chunk 8: fig-mw1 ----
#| fig-cap: "Natural populations undergo fluctuations. Here we smooth the data over different proportions of the time series to reveal different patterns of squiggliness."
#| message: false
ggplot(d, aes(Year, N, colour=Species)) + geom_line() + 
  facet_grid(Species~., scales = "free") + 
  geom_smooth( se=FALSE, span=.2, color="red", linewidth=.5) + 
  geom_smooth( se=FALSE, span=.7, color="black", linewidth=.5) 
ggsave("figs/MooseWolfTS.png", height=5, width=6)

## 12-predator-prey | chunk 9: fig-MWpp ----
#| fig-cap: "Use this phase plane plot to count how many times the populations undergo a complete counterclockwise cycle."
dw <- pivot_wider(d, names_from=Species, values_from=N)
ggplot(dw, aes(Moose, Wolf, colour=Year)) + 
  geom_path(arrow=arrow(length=unit(3, "mm"), angle = 15, type = "open")) + 
  labs(subtitle="Each arrow is one annual time step.")
ggsave("figs/MooseWolfPP.png", height=5, width=5.5)

## 12-predator-prey | chunk 10 [eval: false] ----
#  original chunk options: eval=FALSE
View(dw)
mean(dw$Wolf)
mean(dw$Moose)

## 12-predator-prey | chunk 11 ----
# the number of observations or rows of data
n <- nrow(dw)

# predator and prey numbers for all years except the last.
Pt <- dw$Wolf[-n]
Nt <- dw$Moose[-n]

# predator and prey numbers for all years except the first
Pt1 <- dw$Wolf[-1]
Nt1 <- dw$Moose[-1]

## 12-predator-prey | chunk 12 ----
# each year's r
r <- log(Pt1/Pt)
r
# the maximum
rMax <- max(r)
rMax

## 12-predator-prey | chunk 13 ----
a <- rMax / mean(dw$Wolf)
a

## 12-predator-prey | chunk 14 ----
# mortality, estimated as the minimum log( P[t+1] / P[t] )
m <- abs( min(r) )
m

## 12-predator-prey | chunk 15 ----
b <- m / (a * mean(dw$Moose))
b

## 12-predator-prey | chunk 16 ----
predpreyLV <- function(t, y, p) {
    with( as.list( c(y,p) ), {
      dN <- r*N - a*N*P
      dP <- b*a*N*P - m*P
      return( list( c(dN, dP) ))
    })
}

## 12-predator-prey | chunk 17 ----
parms <- c(r = rMax, a=a, b=b, m=m)
y0 <- c(N=560, P=20)
times <- seq(0, 52, by=0.1)

## 12-predator-prey | chunk 18 ----
out <- ode(y=y0, t=times, fun=predpreyLV, parms=parms)

## 12-predator-prey | chunk 19: fig-LVts ----
#| fig-cap: "Predicted Lotka-Volterra dynamics based on the Isle Royale moose-wolf data."
op <- out %>% as.data.frame() %>%
  pivot_longer(cols=N:P, names_to = "Populations", 
                   values_to="Number")

ggplot(op, aes(x=time, y=Number, colour=Populations)) + geom_line() +
      labs(x="Years") + scale_color_discrete(breaks=c("N","P")) + 
      facet_grid(Populations~., scales="free")

ggsave("figs/LV_MooseWolfTS.png", height=5, width=6)

## 12-predator-prey | chunk 20: fig-LVpp ----
#| fig-cap: "Phase plane diagram of the Lotka-Volterra model of moose wolf dynamics. Once the paramters are fixed and the initial abundances are selected, then the dynamics are completely determined. Initial abundances are moose (560) and wolves (20). The line is black and widest in 1959, and becomes progressively more narrow and blue over time."
outw <- out %>% as.data.frame() 

ggplot(outw, aes(N, P, linewidth= -time, color=time)) + 
  geom_path() + 
  labs(x="Moose population size", y="Wolf population size" )
ggsave("figs/LV_MooseWolfPP.png", height=5, width=6)


# Chapter 13: ForestGEO Community Ecology: Why are there so many tree species, and where? (13-diversity-forestgeo.qmd) ----

## 13-diversity-forestgeo | chunk 1: setup [include: false] ----
#| include: false
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

## 13-diversity-forestgeo | chunk 2: install [eval: false] ----
#| eval: false
## Run this ONCE, then comment it out.
install.packages("vegan")

## 13-diversity-forestgeo | chunk 3: libraries ----
library(vegan)      # community ecology: diversity, rarefaction, accumulation
library(tidyverse)  # data wrangling and ggplot2
library(broom)      # tidy model output

data(BCI)           # load the Barro Colorado Island plot-by-species table

## 13-diversity-forestgeo | chunk 4: peek ----
dim(BCI)            # rows = 1-ha plots, columns = species
BCI[1:5, 1:6]       # a corner of the table: counts of trees, by species, by plot

## 13-diversity-forestgeo | chunk 5: richness ----
richness <- specnumber(BCI)     # one richness value per 1-ha plot
summary(richness)               # how many species per hectare?

## 13-diversity-forestgeo | chunk 6: fig-richness-hist ----
#| fig-cap: "Distribution of tree species richness across the fifty 1-ha BCI plots."
tibble(richness = richness) |>
  ggplot(aes(x = richness)) +
  geom_histogram(binwidth = 5, fill = "forestgreen", colour = "white") +
  labs(x = "Species per 1-ha plot", y = "Number of plots") +
  theme_minimal()

## 13-diversity-forestgeo | chunk 7: fig-rankabundance ----
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

## 13-diversity-forestgeo | chunk 8: diversity-indices ----
shannon <- diversity(BCI, index = "shannon")   # accounts for richness + evenness
simpson <- diversity(BCI, index = "simpson")   # prob. two random trees differ

tibble(shannon = shannon, simpson = simpson) |>
  summarise(across(everything(), list(mean = mean, sd = sd)))

## 13-diversity-forestgeo | chunk 9: hill ----
## The "effective number of species": how many EQUALLY-common species
## would give the same Shannon diversity?
effective_species <- exp(shannon)
summary(effective_species)

## 13-diversity-forestgeo | chunk 10: rarefaction ----
stems <- rowSums(BCI)                 # total trees per plot
min_stems <- min(stems)               # thin every plot to this many
rarefied <- rarefy(BCI, sample = min_stems)

tibble(observed = richness, rarefied = rarefied) |>
  summarise(mean_observed = mean(observed),
            mean_rarefied = mean(rarefied))

## 13-diversity-forestgeo | chunk 11: richness-stems-model ----
model_df <- tibble(richness = richness, stems = rowSums(BCI))

fit <- lm(richness ~ stems, data = model_df)

tidy(fit)      # slope, intercept, and their uncertainty
glance(fit)    # r.squared: how much of richness variation stems explain

## 13-diversity-forestgeo | chunk 12: fig-richness-stems ----
#| fig-cap: "Plot-level richness vs. number of stems, with a fitted line."
ggplot(model_df, aes(x = stems, y = richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, colour = "darkorange") +
  labs(x = "Stems per 1-ha plot", y = "Species per 1-ha plot") +
  theme_minimal()

## 13-diversity-forestgeo | chunk 13: sites-table ----
sites <- tribble(
  ~site,             ~country,    ~continent,      ~abs_latitude, ~area_ha, ~species,
  "SCBI",            "USA",       "North America",          38.9,     25.6,       65,
  "BCI",             "Panama",    "Neotropics",              9.2,     50.0,      300,
  "Korup",           "Cameroon",  "Africa",                  5.1,     50.0,      493,
  "Pasoh",           "Malaysia",  "Asia",                    3.0,     50.0,      800,
  "Yasuni",          "Ecuador",   "Neotropics",              0.7,     50.0,     1150
)

sites

## 13-diversity-forestgeo | chunk 14: fig-latitude ----
#| fig-cap: "Tree species richness vs. absolute latitude across forest plots. Point size shows plot area; colour shows continent. Mind the area caveat."
ggplot(sites, aes(x = abs_latitude, y = species,
                  colour = continent, size = area_ha)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = site), vjust = -1, size = 3, show.legend = FALSE) +
  labs(x = "Absolute latitude (degrees from equator)",
       y = "Species richness (in plot)",
       colour = "Continent", size = "Plot area (ha)") +
  theme_minimal() # + scale_y_log10()

## 13-diversity-forestgeo | chunk 15: deliverable-accumulation [eval: false] ----
#| eval: false
accum <- specaccum(BCI)   # cumulative species as plots are added
accum_df <- tibble(plots = accum$sites,
                   richness = accum$richness,
                   sd = accum$sd)

ggplot(accum_df, aes(x = plots, y = richness)) +
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd),
              fill = "grey80") +
  geom_line(linewidth = 1) +
  labs(x = "Number of 1-ha plots sampled",
       y = "Cumulative species found") +
  theme_minimal()


# Chapter 14: GBIF: Why are there so many species, and where? (14-diversity-gbif.qmd) ----

## 14-diversity-gbif | chunk 1: setup [include: false] ----
#| include: false
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

## 14-diversity-gbif | chunk 2: install [eval: false] ----
#| eval: false
## Run ONCE, then comment out.
install.packages(c("rgbif", "vegan"))

## 14-diversity-gbif | chunk 3: libraries ----
library(rgbif)      # live access to the GBIF biodiversity repository
library(vegan)      # diversity, rarefaction
library(tidyverse)  # wrangling and ggplot2
library(broom)      # tidy model output

## 14-diversity-gbif | chunk 4: palm-key ----
#| cache: true
palm_key <- name_backbone(name = "Arecaceae")$usageKey
palm_key

## 14-diversity-gbif | chunk 5: country-richness ----
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

## 14-diversity-gbif | chunk 6: fig-country-richness ----
#| fig-cap: "Recorded palm species richness by country. The three left-hand bars sit at nearly the same latitude but on different continents."
countries |>
  mutate(country = fct_reorder(country, richness)) |>
  ggplot(aes(x = country, y = richness, fill = continent)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Recorded palm species (GBIF)", fill = "Region") +
  theme_minimal()

## 14-diversity-gbif | chunk 7: fig-effort ----
#| fig-cap: "Recorded richness rises with sampling effort (number of records). Raw richness is partly a map of where people have looked."
ggplot(countries, aes(x = records, y = richness, label = country)) +
  geom_point(size = 2) +
  geom_text(vjust = -0.8, size = 3) +
  scale_x_log10() +
  labs(x = "Number of GBIF records (log scale)", y = "Recorded palm species") +
  theme_minimal()

## 14-diversity-gbif | chunk 8: rarefy-data ----
#| cache: true
get_species_counts <- function(iso, n = 3000) {
  occ_search(taxonKey = palm_key, country = iso,
             hasCoordinate = TRUE, limit = n)$data |>
    filter(!is.na(species)) |>
    count(species, name = "n")
}

co <- get_species_counts("CO")   # Colombia
cm <- get_species_counts("CM")   # Cameroon

## 14-diversity-gbif | chunk 9: rarefy-compute ----
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

## 14-diversity-gbif | chunk 10: gradient ----
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

## 14-diversity-gbif | chunk 11: fig-gradient ----
#| fig-cap: "Palm species richness in equal-sized 10°×10° boxes vs. absolute latitude, with a fitted line."
ggplot(boxes, aes(x = abs_latitude, y = richness)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "darkorange") +
  labs(x = "Absolute latitude (degrees from equator)",
       y = "Palm species in box") +
  theme_minimal()

## 14-diversity-gbif | chunk 12: gradient-model ----
fit <- lm(richness ~ abs_latitude, data = boxes)
tidy(fit)      # slope: species lost per degree of latitude
glance(fit)    # r.squared: how much of the variation latitude explains


# Chapter 15: Metabolic Ecology: How do body size and temperature affect nutrient cycling? (15-metabolic-ecology_v_2.qmd) ----

## 15-metabolic-ecology_v_2 | chunk 1: setup [include: false] ----
#| include: false
library(tidyverse)
library(broom)

## 15-metabolic-ecology_v_2 | chunk 2 [eval: false] ----
#| eval: false
install.packages("broom")

## 15-metabolic-ecology_v_2 | chunk 3 [eval: false] ----
#| eval: false
library(tidyverse)
library(broom)

## 15-metabolic-ecology_v_2 | chunk 4 ----
# Note we skip the first 8 lines because they are 'metadata',
# which is 'data about data` or information about the variables.

fish <- read.csv("data/fish_nutrient_excretion.csv", skip=8)
glimpse(fish)

## 15-metabolic-ecology_v_2 | chunk 5 ----
summary(fish)

## 15-metabolic-ecology_v_2 | chunk 6 ----
boltzmann_k <- 8.617333262e-5

## 15-metabolic-ecology_v_2 | chunk 7 ----
## Add log-transformed columns to the data
fish <- fish %>% #start with our fish data set, and
  mutate(log_mass = log(mass_g), # create ln(mass)
         log_N = log(N_u.h), # ln(N excretion)
         log_P = log(P_u.h), # ln(P excretion)
         temp_K = 273.15 + temp_C, # degrees Kelvin
         inv.kT = 1/(boltzmann_k * temp_K) # inverse scaled Kelvin
  )

## 15-metabolic-ecology_v_2 | chunk 8: fig-raw-N ----
#| fig-cap: "Nitrogen excretion rate vs. body mass on arithmetic (raw) axes."
ggplot(fish, aes(x = mass_g, y = N_u.h)) +
  geom_point(alpha = 0.5) +
  labs(x = "Body mass (g)",
       y = expression(paste("N excretion (", mu, "mol ", fish^{-1}, " ", hr^{-1}, ")")),
       title = "N excretion vs. body mass") +
  theme_minimal()

## 15-metabolic-ecology_v_2 | chunk 9: fig-log-N ----
#| fig-cap: "Log nitrogen excretion rate vs. log body mass. If the allometric model holds, this should be approximately linear."
ggplot(fish, aes(x = log(mass_g), y = log(N_u.h))) +
  geom_point(alpha = 0.5) +
  labs(x = expression(log[10]~"(body mass, g)"),
       y = expression(log[10]~"(N excretion, " * mu * "mol " * fish^{-1} * " " * hr^{-1} * ")"),
       title = "Allometric scaling of N excretion") +
  theme_minimal()

## 15-metabolic-ecology_v_2 | chunk 10: fig-regression-N1 ----
#| fig-cap: "Linear regression of log N excretion on log body mass, for each temperature."
ggplot(fish, aes(x = log_mass, y = log_N)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~temp_C) + 
  labs(x = expression(log~"(body mass, g)"),
       y = expression(log~"(N excretion)"),
       title = "Allometric scaling of N excretion") +
  theme_minimal()

## 15-metabolic-ecology_v_2 | chunk 11 ----
## Fit the regression
m_N_all <- lm(log_N ~ log_mass + inv.kT, data = fish)

## 15-metabolic-ecology_v_2 | chunk 12 ----
plot(m_N_all, which=1)

## 15-metabolic-ecology_v_2 | chunk 13 ----
## View the results
# ignore the intercept
confint(m_N_all)

## 15-metabolic-ecology_v_2 | chunk 14 [eval: false] ----
#| echo: false
#| eval: false
fish <- fish %>%
  mutate(
    log_mass_groups = cut_number(log_mass, 6)
  )
ggplot(fish, aes(x = inv.kT, y = log_N)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~log_mass_groups, scales="free")
  labs(x = expression(log[10]~"(body mass, g)"),
       y = expression(log[10]~"(N excretion)"),
       title = "Allometric scaling of N excretion") +
  theme_minimal()

## 15-metabolic-ecology_v_2 | chunk 15 [eval: false] ----
#| eval: false
#| echo: false
cf <- coef(m_N_all)

# Temperature
degC <- c(14, 16, 18, 20)
inv.kT2 <- 1/(boltzmann_k *(273.15+degC))
logM <- log(46)
d <- data.frame(inv.kT=inv.kT2, log_mass=logM)

p <- predict(m_N_all, newdata=d)
p[-1] / p[-4] - 1

## body mass
degC <- c(20)
inv.kT2 <- 1/(boltzmann_k *(273.15+degC))
quantile(fish$mass_g)
logM <- log(c(25, 35, 45, 55, 65))
d <- data.frame(inv.kT=inv.kT2, log_mass=logM)

p <- predict(m_N_all, newdata=d)
p
p[-1] / p[-5] - 1


# Chapter 16: An Introduction to the Carbon Cycle and Global Heating (16-ecosystem-intro.qmd) ----

## 16-ecosystem-intro | chunk 1 ----
getwd()

## 16-ecosystem-intro | chunk 2 ----
#| message: false
## This script is for chapter XX in Hank's Intro to Ecology primer.

## We always use...
library(tidyverse)
## or usually just ggplot2 and dplyr packages within the tidyverse.
## In this chapter we also need
library(deSolve)
# for solving or integrating differential equations

## 16-ecosystem-intro | chunk 3 ----
or <- 91 # diffusion out of the ocean, due to respiration of microbes
ro <- 1 # river outgassing (diffusion)
fi <- 4 # fire
cp <- 0.1 # cement production
ff <- 7.6 # fossil fuel combustion (1998) 
lu <- 1.5 # land use change 
tr <- 56 # terrestrial respiration
inputs <- or + ro + fi + cp + ff + lu + tr
inputs

## 16-ecosystem-intro | chunk 4 ----
npp.o <- 92 # net primary production in the ocean (diffusion)
we <- 0.2 # Weathering of carbonate rock
ls <- 2.8 # land sink (burial)
npp.l <- 60 # net primary production on land

outputs <- npp.o + we + ls + npp.l
outputs

## 16-ecosystem-intro | chunk 5 ----
net.flux <- inputs - outputs
net.flux # in Pg C per year

## 16-ecosystem-intro | chunk 6 ----
# the pool
At <- 762

## 16-ecosystem-intro | chunk 7 ----
At.plus.1 <- At + net.flux
At.plus.1

## 16-ecosystem-intro | chunk 8 ----
#| echo: false
#| results: hide

net.flux

t <- c(2,25)
at.2.and.25 <- At + net.flux * t
at.2.and.25

## 16-ecosystem-intro | chunk 9 ----
## write code two years and for 25 years
## 

## 16-ecosystem-intro | chunk 10 ----
respiration <- 91 # Pg
photosynthesis <- 91 # Pg
carbonic_acid <- 1 #Pg

## 16-ecosystem-intro | chunk 11 ----
## simple first order equation.
## start
atmosphere1 <- function(t,y,p){
  with( as.list(c(y, p)), {
    
    ## this is the bit to recognize
    dA.dt <- I - e*A
    
    return(list( c(dA.dt), 
                 ## convert gigatonnes C to CO2 equivalent mass 
                 CO2_global_mass = A * (12 + 2*16)/12,
                 ## convert gigatonnes C to CO2 equivalent ppm
                 CO2_ppm = A / 2.13
    ) )
  })
}
## end

## 16-ecosystem-intro | chunk 12 ----
# Constant influx, and diffusion rate
p1 <- c(I=6.2, e=0.008)
# starting atmospheric carbon pool
y0 <- c(A=762);

## 16-ecosystem-intro | chunk 13 ----
# from zero to 100, by steps of one year
t <- seq(from=0, to=1000, by=1)
# integrate the rate equation for all time points, 
# starting with y0 at time zero, using our function and parameters.
output <- ode(y=y0, times=t, func=atmosphere1, parms=p1)

## 16-ecosystem-intro | chunk 14 ----
#| eval: true
# Finally, we plot the output.
out_df <- as.data.frame(output) 
firstorderA <- ggplot(out_df, aes(time, A)) + geom_line() + 
  labs(y="Atmospheric carbon pool (Pg)",
       x="Year") + 
  theme_bw()
firstorderA
ggsave("figs/firstOrderAtmosphere.png", width=5, height=4)

## 16-ecosystem-intro | chunk 15 ----
#| echo: false
#| results: hide
asymp <- with(as.list(p1), I/e)
asymp

## 16-ecosystem-intro | chunk 16 ----
#| echo: false
#| results: hide

# net.flux
# at2and25

# firstorderA
# asymp

## 1. A* varies inversely with e (the per-concentration export rate)
## 1. Simple budget has no limit, while the first order has an asymptote
## 1. Oceans will acidify?
## 1. higher temps will excerbate dissolution

## 16-ecosystem-intro | chunk 17: blackK [eval: false] ----
#| eval: false
Omega <- 1372
sigma <- 5.67e-8
albedo <- 0.3

temp_K <- ( (1-albedo) * Omega/(4*sigma) )^(1/4)
temp_K

## 16-ecosystem-intro | chunk 18 ----
#| echo: false
K <- c(255, 288, 290)^4
A <- c(0, 280, 450)
m <- nls(K ~ .7*1372/(4*5.67e-8*(A+1)^g), start=list(g=-.08))
g <- - as.numeric( coef(m) )

## 16-ecosystem-intro | chunk 19: fig-g ----
#| fig-cap: "Temperatures predicted from first principles and using Hank's greenhouse constant, $g$, which adjusts for observed and predicted temperatures, based on actual CO2 and temperature data and extremely complex and well-tested models used in IPCC reports."
#| message: false
#| warning: false

## graphing the curve of temperature vs. CO2
g <- 0.08488
Omega <- 1372
sigma <- 5.67e-8
albedo <- 0.3

# function to describe the relationship
predict.warming <- 
  function(x, g, Omega, sigma, albedo){ 
    ((1-albedo)*Omega/(4*sigma)*x^g )^(1/4)
    }

base <- ggplot() + lims(x=c(0, 500), y=c(250,300))
base + 
  geom_function(fun = predict.warming, 
                     args = list(g = g, Omega, sigma, albedo),
                n=5001) +
  annotate(geom="point", x=c(0, 280, 450), 
           y=c(250, 288, 290) ) +
  labs(
    x="Carbon dioxide (ppm)", 
      y="Predicted temperatures",
  ) + 
  theme_bw()

## 16-ecosystem-intro | chunk 20 ----
Earth1 <- function(t,y,p){
  with( as.list(c(y, p)), {
    ## hypothesized relation of carbon inputs and outputs for 
    ## the atmosphere
    ## constant input I, diffusion loss out eA
    ## CO2 rate equation, units CO2 / year
    dA.dt <- I + (1-A/280)
    
    ## incoming radiant energy
    F.in <- Omega/4 
    ## outgoing radiant energy
    F.out <- a*Omega/4 + (sigma/A^g)*K^4
    ## heat energy rate equation
    dK.dt <- (F.in - F.out)/lag
    
    ## What the temperature WOULD be, 
    ## without a lag 
    Equilibrium_K = ( (1-a) * Omega / (4*(sigma)) * A^g )^(1/4)
    
    ### Have R return these results to us
    return(list( c(dA.dt, dK.dt) , 
                 K_eq = Equilibrium_K
    ) )
  })
}

## 16-ecosystem-intro | chunk 21 ----
t <- seq(0, 77, 1/52)

## 16-ecosystem-intro | chunk 22 ----
y0 <- c(A=420, # CO2 equivalents in ppm in 2023
        K=289 # degrees Kelvin in 2023
        )

## 16-ecosystem-intro | chunk 23 ----
#| message: false
library(tidyverse) # data wrangling and graphics
library(deSolve) # for numerical integration of ODE models

## 16-ecosystem-intro | chunk 24 ----
parameters1 <- c(Omega = 1372, # incoming solar radiative forcing
       a=0.3, # albedo
       sigma = 5.67e-8, # Stefan-Boltzmann constant for radiative loss
       g = 0.08488, # greenhouse constant
       I = 2.1, # increase in CO2 ppm per year
       lag = 50 # lag time to partial temp equilibration
)

## 16-ecosystem-intro | chunk 25 ----
output1 <- ode(y=y0, 
               times=t, 
               func=Earth1, 
               parms=parameters1)
head(output1)

## 16-ecosystem-intro | chunk 26 ----
tail(output1)

## 16-ecosystem-intro | chunk 27 ----
p1.1 <- output1 %>% as.data.frame() %>%
  mutate(
    Year = time + 2023
  ) %>%
  ggplot(aes(x=Year, y=A)) + 
  geom_line() + labs(y="Atmospheric CO2 (ppm)") +
  theme_bw()
p1.1
ggsave("figs/Exp1_CO2.png", plot=p1.1)

## 16-ecosystem-intro | chunk 28 ----
out.long1 <- output1 %>% as.data.frame() %>%
  select(time, K, K_eq) %>%
  transmute(
    Year = time + 2023,
    degC = K -273,
    degC_eq = K_eq -273
  ) %>%
  pivot_longer(cols=-Year, names_to = "State_variable", 
               values_to="Value")

p2.1 <- ggplot(out.long1, aes(x=Year, y=Value, color=State_variable)) + 
  geom_line() + 
  labs(y="Degrees Celcius")  +
  theme_bw()
p2.1
ggsave("figs/Exp1_temps.png", plot=p2.1)

## 16-ecosystem-intro | chunk 29 ----
# eval: false
# uncomment and run
# install.package("patchwork", dep=TRUE)

# after installation, load it:
library(patchwork)

# use it.
# try
p2.1 / p1.1
# or 
p1.1 + p2.1
ggsave("figs/Exp1_CO2andK.png", height=4, width=8)

## 16-ecosystem-intro | chunk 30 ----
parameters2 <- c(Omega = 1372, # incoming solar radiative forcing
       a=0.3, # albedo
       sigma = 5.67e-8, # Stefan-Boltzmann constant for radiative loss
       g = 0.08492, # greenhouse constant
       I = 0.5, # increase in CO2 ppm per year
       lag = 50 # lag time to partial temp equilibration
)

## 16-ecosystem-intro | chunk 31 ----
output2 <- ode(y=y0, times=t, func=Earth1, parms=parameters2)
tail(output2)

## 16-ecosystem-intro | chunk 32 [eval: false] ----
#| eval: false
p1.2 <- output2 %>% as.data.frame() %>%
  mutate(
    Year = time + 2023
  ) %>%
  ggplot(aes(x=Year, y=A)) + 
  geom_line() + labs(y="Atmospheric CO2 (ppm)") +
  theme_bw()
p1.2
ggsave("figs/Exp2_CO2.png", plot=p1.2)

## 16-ecosystem-intro | chunk 33 [eval: false] ----
#| eval: false
out.long2 <- output2 %>% as.data.frame() %>%
  select(time, K, K_eq) %>%
  transmute(
    Year = time + 2023,
    degC = K -273,
    degC_eq = K_eq -273
  ) %>%
  pivot_longer(cols=-Year, names_to = "State_variable", 
               values_to="Value")

p2.2 <- ggplot(out.long2, aes(x=Year, y=Value, color=State_variable)) + 
  geom_line() + 
  labs(y="Degrees Celcius")  +
  theme_bw()
p2.2
ggsave("figs/Exp2_temps.png")

## 16-ecosystem-intro | chunk 34 ----
parameters3 <- c(Omega = 1372, # incoming solar radiative forcing
       a=0.3, # albedo
       sigma = 5.67e-8, # Stefan-Boltzmann constant for radiative loss
       g = 0.08492, # greenhouse constant
       I = 0.357, # increase in CO2 ppm per year
       lag = 50 # lag time to partial temp equilibration
)

## 16-ecosystem-intro | chunk 35 ----
output3 <- ode(y=y0, times=t, func=Earth1, parms=parameters3)
tail(output3)

## 16-ecosystem-intro | chunk 36 [eval: false] ----
#| eval: false
p1.3 <- output3 %>% as.data.frame() %>%
  mutate(
    Year = time + 2023
  ) %>%
  ggplot(aes(x=Year, y=A)) + 
  geom_line() + labs(y="Atmospheric CO2 (ppm)") +
  theme_bw()
p1.3
ggsave("figs/Exp3_CO2.png", plot=p1.3)

## 16-ecosystem-intro | chunk 37 [eval: false] ----
#| eval: false
out.long3 <- output3 %>% as.data.frame() %>%
  select(time, K, K_eq) %>%
  transmute(
    Year = time + 2023,
    degC = K -273,
    degC_eq = K_eq -273
  ) %>%
  pivot_longer(cols=-Year, names_to = "State_variable", 
               values_to="Value")

p2.3 <- ggplot(out.long3, aes(x=Year, y=Value, color=State_variable)) + 
  geom_line() + 
  labs(y="Degrees Celcius")  +
  theme_bw()
p2.3
ggsave("figs/Exp3_temps.png", plot=p2.3)

## 16-ecosystem-intro | chunk 38 [eval: false] ----
#| echo: false
#| eval: false
pc <- (p1.1 + p2.1)/
(p1.2 + p2.2)/
(p1.3 + p2.3)
ggsave("figs/carbon2.png", plot=pc)
exp1h <- c(output1[1,], diff=output1[1,4]-output1[1,3])
exp1t <- c(output1[nrow(output1),], diff=output1[nrow(output1),4]-output1[nrow(output1),3])

exp2h <- c(output2[1,], diff=output2[1,4]-output2[1,3])
exp2t <- c(output2[nrow(output2),], diff=output2[nrow(output2),4]-output2[nrow(output2),3])

exp3h <- c(output3[1,], diff=output3[1,4]-output3[1,3])
exp3t <- c(output3[nrow(output3),], diff=output3[nrow(output3),4]-output3[nrow(output1),3])

a <- rbind.data.frame(exp1h, exp1t, exp2h, exp2t, exp3h, exp3t)
names(a) <- c("year", "A", "K", "K_eq", "K_eq-K")
write_csv(a, file="carbon2.csv")


# Chapter 17: Funding Ecological and Environmental Science: Following the Money (17-funding-science.qmd) ----

## 17-funding-science | chunk 1 [eval: false] ----
#| eval: false
install.packages("jsonlite", repos = "https://cloud.r-project.org")

## 17-funding-science | chunk 2 ----
## load packages
library(jsonlite)
library(tidyverse)
library(googlesheets4)

## 17-funding-science | chunk 3 ----
## a named vector: the names are labels we choose, the values are USAspending's codes
agency_codes <- c(NSF  = "049",
                   EPA  = "068",
                   USDA = "012",
                   DOE  = "089",
                   DOI  = "014",
                   NASA = "080")

## a function that, given one toptier code, returns that agency's
## budgetary resources for every fiscal year USAspending has on file
get_budget <- function(code) {
  url <- paste0("https://api.usaspending.gov/api/v2/agency/", code,
                "/budgetary_resources/")
  result <- fromJSON(url)          # ask the API, and parse its JSON response
  result$agency_data_by_year       # this is the part we actually want: one row per fiscal year
}

## 17-funding-science | chunk 4 ----
## map() applies get_budget() to each element of agency_codes;
## bind_rows(..., .id="agency") stacks the six results and labels each
budget_list <- map(agency_codes, get_budget)
budget_data <- bind_rows(budget_list, .id = "agency")

## take a look at what we got
names(budget_data)
head(budget_data)

## 17-funding-science | chunk 5: fig-agency-budgets ----
#| fig-cap: "Total budgetary resources for six federal agencies relevant to ecological and environmental science, in nominal (not inflation-adjusted) dollars."
p_agency_budgets <- ggplot(data = budget_data,
       aes(x = fiscal_year, y = agency_budgetary_resources / 1e9,
           color = agency)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(x = "Fiscal year",
       y = "Budgetary resources (billion $, nominal)",
       color = "Agency",
       title = "Federal budgets for science-relevant agencies")
p_agency_budgets

## 17-funding-science | chunk 6 [eval: false] ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-agency-budgets.png", plot = p_agency_budgets,
       width = 7, height = 5)

## 17-funding-science | chunk 7 ----
## Annual average CPI-U (all items, U.S. city average), 1982-84 = 100.
## Source, 2017-2025: U.S. Bureau of Labor Statistics,
## https://www.bls.gov/regions/mid-atlantic/data/consumerpriceindexannualandsemiannual_table.htm
## Note: the 2025 average is BLS's own preliminary figure--October 2025 data
## could not be collected during a lapse in appropriations that year.
## Source, 2026: BLS has not published a 2026 annual average yet--the year
## isn't over. Rather than leave 2026 blank, we use a *projected* value:
## the Federal Reserve Bank of Philadelphia's Survey of Professional
## Forecasters (SPF), Q2 2026 release, put median expected headline CPI
## inflation for 2026 at 3.5% (Q4-over-Q4 basis):
## https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/spf-q2-2026
## We applied that growth rate to the 2025 annual average (321.943 * 1.035
## = ~333.2) to get a rough 2026 estimate. This is explicitly an estimate,
## not an official BLS figure--replace it with the real annual average
## once BLS publishes it (early the following year).
## Stored in data/cpi.csv; add a row there each time you re-teach this
## chapter, replacing last year's projection with the real BLS figure and
## adding a new projected row for the new current year.
cpi <- read_csv("data/cpi.csv")

## 17-funding-science | chunk 8 ----
## the CPI value for our reference year
base_cpi <- cpi$cpi_u[cpi$fiscal_year == 2025]

budget_real <- budget_data %>%
  left_join(cpi, by = "fiscal_year") %>%
  ## multiplying by (base year's CPI / that year's CPI) rescales every
  ## year's dollars into 2025 purchasing power
  mutate(budget_real_2025usd = agency_budgetary_resources * (base_cpi / cpi_u))

## 17-funding-science | chunk 9: fig-agency-budgets-real ----
#| fig-cap: "The same six agency budgets as @fig-agency-budgets, expressed in constant 2025 dollars."
p_agency_budgets_real <- budget_real %>%
  filter(!is.na(budget_real_2025usd)) %>%
  ggplot(aes(x = fiscal_year, y = budget_real_2025usd / 1e9, color = agency)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(x = "Fiscal year",
       y = "Budgetary resources (billion 2025 $)",
       color = "Agency",
       title = "Federal budgets for science-relevant agencies, inflation-adjusted")
p_agency_budgets_real

## 17-funding-science | chunk 10 [eval: false] ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-agency-budgets-real.png", plot = p_agency_budgets_real,
       width = 7, height = 5)

## 17-funding-science | chunk 11 ----
## Federal R&D OBLIGATIONS by agency--NOT each agency's entire budget, but the
## slice NCSES classifies as research and development (plus R&D facilities/
## equipment, i.e. "R&D plant"). In millions of dollars.
##
## IMPORTANT: nine of these ten rows are OBLIGATIONS, but DOE_BER is not.
## Source (NSF, EPA, USDA, NOAA, NASA, DOE, DOI, FWS, USGS, USFS): NCSES Survey
## of Federal Funds for Research and Development, Table 3, multiple survey
## years: https://ncses.nsf.gov/surveys/federal-funds-research-development.
## This is OBLIGATIONS: a legal commitment the agency made that year (a grant
## awarded, a contract signed)--the same "obligated" concept as
## agency_total_obligated back in Part 1. It is NOT the same as an OUTLAY
## (cash actually paid out, which can lag obligations by months or years),
## and it is NOT the same as an APPROPRIATION (the budget authority Congress
## granted, which an agency can carry into future years without fully
## obligating it the same year).
## Source (DOE_BER only): NCSES does not break DOE down by program office, so
## this row comes directly from DOE Office of Science's own congressional
## budget justifications for Biological and Environmental Research (BER).
## Those documents report ENACTED APPROPRIATIONS, not obligations--a related
## but different number (see above). Treat the DOE_BER row as "what Congress
## gave BER to work with," not "what BER legally committed," and keep that
## distinction in mind when comparing it to the other nine rows in
## @fig-rd-only-subagencies. See
## https://science.osti.gov/budget/Budget-by-Program/BER-Budget for the source
## documents by year.
## Stored in data/rd_only.csv. NCSES (and DOE) revise their own estimates in
## the following year's release (a "preliminary" figure becomes "final"), so a
## few of these will differ slightly from whatever is on the source website
## when you read this--that is normal for survey/budget data, not a mistake.
## Update/extend data/rd_only.csv when you re-teach the chapter.
rd_only <- read_csv("data/rd_only.csv")

## 17-funding-science | chunk 12: fig-rd-only-departments ----
#| fig-cap: "Research and development obligations only (not total agency budget), for the six toptier agencies, FY2019-2025."
p_rd_departments <- rd_only %>%
  filter(agency %in% c("NSF", "EPA", "USDA", "DOE", "DOI", "NASA")) %>%
  ggplot(aes(x = fiscal_year, y = rd_obligations / 1000, color = agency)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(x = "Fiscal year", y = "R&D obligations (billion $, nominal)",
       color = "Agency",
       title = "Research-only funding, toptier agencies")
p_rd_departments

## 17-funding-science | chunk 13 [eval: false] ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-rd-only-departments.png", plot = p_rd_departments,
       width = 7, height = 5)

## 17-funding-science | chunk 14: fig-rd-only-subagencies ----
#| fig-cap: "Research and development obligations only, for NOAA, FWS, USGS, and the Forest Service (sub-agencies USAspending can't show on their own) and DOE BER (a program office isolated from DOE's much larger, mostly non-ecological R&D total), FY2019-2025."
p_rd_subagencies <- rd_only %>%
  filter(agency %in% c("NOAA", "FWS", "USGS", "USFS", "DOE_BER")) %>%
  ggplot(aes(x = fiscal_year, y = rd_obligations, color = agency)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(x = "Fiscal year", y = "R&D obligations (million $, nominal)",
       color = "Agency",
       title = "Research-only funding, sub-agencies and DOE BER")
p_rd_subagencies

## 17-funding-science | chunk 15 [eval: false] ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-rd-only-subagencies.png", plot = p_rd_subagencies,
       width = 7, height = 5)

## 17-funding-science | chunk 16: fig-total-vs-research ----
#| fig-cap: "Total agency budget (USAspending) versus research-only obligations (NCSES, or DOE's own budget justifications for DOE BER) for fiscal year 2024. NOAA, FWS, USGS, the Forest Service, and DOE BER have no bar for 'entire agency budget': the first four are not toptier agencies (see above), and DOE BER is a program office, not an agency--DOE's own 'entire agency budget' bar is what its BER bar should be compared against."
compare_year <- 2024

total_fy <- budget_data %>%
  filter(fiscal_year == compare_year) %>%
  transmute(agency, amount = agency_budgetary_resources / 1e6,
            category = "Entire agency budget")

research_fy <- rd_only %>%
  filter(fiscal_year == compare_year) %>%
  transmute(agency, amount = rd_obligations,
            category = "Research (R&D) obligations only")

p_total_vs_research <- bind_rows(total_fy, research_fy) %>%
  ggplot(aes(x = agency, y = amount / 1000, fill = category)) +
  geom_col(position = "dodge") +
  labs(x = "Agency", y = "Billion $",
       fill = NULL,
       title = paste("How much of each agency's FY", compare_year, "budget is research?"))
p_total_vs_research

## 17-funding-science | chunk 17 [eval: false] ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-total-vs-research.png", plot = p_total_vs_research,
       width = 7, height = 5)

## 17-funding-science | chunk 18 [eval: false] ----
#| eval: false
d_state <- read_sheet("PASTE_YOUR_CLASS_GOOGLE_SHEET_URL_HERE")

## 17-funding-science | chunk 19 ----
#| echo: false
## a small stand-in data set so this document renders without a live class sheet
d_state <- tibble::tribble(
  ~state,     ~program,               ~fiscal_year, ~apportionment,
  "Ohio",     "Wildlife Restoration", 2022,          9800000,
  "Ohio",     "Wildlife Restoration", 2023,          10100000,
  "Ohio",     "Wildlife Restoration", 2024,          10400000,
  "Montana",  "Wildlife Restoration", 2022,          16200000,
  "Montana",  "Wildlife Restoration", 2023,          16500000,
  "Montana",  "Wildlife Restoration", 2024,          16900000
)

## 17-funding-science | chunk 20: fig-state-funding ----
#| fig-cap: "Wildlife Restoration apportionments for a sample of states. Your class figure will include every state your classmates collected."
p_state_funding <- ggplot(data = d_state,
       aes(x = fiscal_year, y = apportionment / 1e6, color = state)) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~program) +
  labs(x = "Fiscal year", y = "Apportionment (million $)", color = "State")
p_state_funding

## 17-funding-science | chunk 21 [eval: false] ----
#| eval: false
## Save a PNG file to your working directory--this is the figure to
## insert into your deliverable document.
ggsave("figs/fig-state-funding.png", plot = p_state_funding,
       width = 7, height = 5)

## 17-funding-science | chunk 22 ----
## pick a keyword related to something you're curious about--
## try your own before settling on "biodiversity"
url <- "https://api.nsf.gov/services/v1/awards.json?keyword=biodiversity"
nsf_result <- fromJSON(url)

## the awards themselves are nested inside the response--explore its structure
## before assuming you know what's in it
str(nsf_result, max.level = 3)

## 17-funding-science | chunk 23 ----
## look at the list of award titles to help you pick one that interests you
nsf_result$response$award$title

## pick one (change the row number to whichever award you chose)
my_award <- nsf_result$response$award[1, ]
my_award$title
my_award$abstractText

## the total dollar amount NSF obligated to this award
my_award$fundsObligatedAmt

## 17-funding-science | chunk 24 ----
## NSF's Division of Environmental Biology (DEB) budget--not NSF's total
## budget, and not even all of BIO's budget, but the one division that most
## directly funds population, community, and ecosystem ecology. In millions
## of dollars.
## Source: NSF's own Congressional Budget Justification, Biological Sciences
## (BIO) directorate chapter, which breaks BIO's budget down by division.
## Unlike DOE (which reports clean enacted appropriations for BER every
## year), NSF's public budget documents do not give DEB a consistent,
## fully-enacted multi-year series--figures are a mix of "Actual," "Base
## Plan," and "Request," and some years are marked "TBD" in the document
## available when that year's chapter was written. The `status` column
## below tells you which is which; do not treat all rows as equally solid.
## FY2021 Actual: https://nsf-gov-resources.nsf.gov/about/budget/fy2023/pdf/69_fy2023.pdf
## FY2023 Actual, FY2025 Request: https://nsf-gov-resources.nsf.gov/files/66_fy2025.pdf
## Stored in data/deb_budget.csv. Replace the FY2025 row with the enacted
## figure once it's final, and add newer years as NSF publishes them.
deb_budget <- read_csv("data/deb_budget.csv")

## 17-funding-science | chunk 25: fig-deb-budget ----
#| fig-cap: "NSF's Division of Environmental Biology (DEB) budget--the part of NSF most directly funding population, community, and ecosystem ecology. FY2025 is a request, not yet a final enacted figure; see the `status` column."
p_deb_budget <- ggplot(deb_budget,
       aes(x = factor(fiscal_year), y = deb_budget_millions, fill = status)) +
  geom_col() +
  labs(x = "Fiscal year", y = "DEB budget (million $)", fill = "Status",
       title = "NSF's Division of Environmental Biology (DEB)")
p_deb_budget

## 17-funding-science | chunk 26 [eval: false] ----
#| eval: false
## Save a PNG file to your working directory.
ggsave("figs/fig-deb-budget.png", plot = p_deb_budget, width = 7, height = 5)


# Appendix A: Community structure (appendix_Community_structure.qmd) ----

## appendix_Community_structure | chunk 1 [include: false] ----
#| include: false
library(kableExtra) # for making tables in Rmd and Qmd files

## appendix_Community_structure | chunk 2 ----
#| message: false
library(vegan) # multivariate analysis
library(tidyverse) # data wrangling and graphics

## appendix_Community_structure | chunk 3 ----
#| results: hide
d <- data.frame(
  moisture = 1:12,
  Halesia = c(5,8,4,1,9,13,3,1,1, 0,0,0),
  Pinus = c(0,0,0,0,0,0,0,0,1,4,54,49),
  Quercus = c(0,0,0,0,0,2,1,8,24,10,.1,0),
  Tilia = c(29,11,9,1,14,3, 0,0,0,0,0,0),
  Tsuga = c(20,22,34,62,18,.1,.1,1, 0,0,0,0)
)
d

## appendix_Community_structure | chunk 4: tbl-whit ----
#| echo: false
#| tbl-cap: Small subset of data from Whittaker (1956) Fig. 4 & Table 3, reflecting a moisture gradient.

kbl(d, booktabs=TRUE)

## appendix_Community_structure | chunk 5 ----
#| results: hide
d2 <- d %>% select(moisture, Tilia, Tsuga, Halesia, Quercus, Pinus)

# if the above generates an error message, it may be because
# of 'select()' refers to the wrong R package. If you get an errror, 
# try referring specifically to a particular package with the following code. 
# Uncomment the following and use this instead.
# d <- d %>% dplyr::select(moisture, Tilia, Tsuga, Halesia, Quercus, Pinus)

d2

## appendix_Community_structure | chunk 6: tbl-whit2 ----
#| echo: false
#| tbl-cap: Reorganizing Table 1 to comunicate better.

kbl( round(d2,0) )

## appendix_Community_structure | chunk 7 ----
yl <- d2 %>%
  pivot_longer(cols=Tilia:Pinus, values_to="abundance", names_to="species")
head(yl)

## appendix_Community_structure | chunk 8: fig-whit1 ----
#| fig-cap: Tree species abundances along a moisture gradient in the Smoky Mountains of North Carolina, USA (Whittker 1956). Y-axis scaled by square root transformation.

ggplot(data=yl, aes(x=moisture, y=abundance, 
               color=species, linetype=species)) + 
  geom_line() +
  scale_y_sqrt() +
  labs(y="Abundance", x="Moisture index")

## appendix_Community_structure | chunk 9 ----
Y <- d2 %>% select(-moisture)
Yw <- wisconsin(Y)
# totals of each site (rows)
apply(Yw, MARGIN=1, sum)

# resulting maxima of each species (cols) 
apply(Yw, MARGIN=2, max)

## appendix_Community_structure | chunk 10 ----
# Using a function in vegan
decostand(Y, method="max", MARGIN=2)

# Doing the same thing in base R
apply(Y, 2, function(x) x/max(x)) 

## appendix_Community_structure | chunk 11 ----
# Use square roots of our data
Ysr <- apply(Y, 2, function(x) (x^0.5)) 
Ysr

## appendix_Community_structure | chunk 12 ----
Y.euc.raw <- vegdist(Y, method="euclid")
round(Y.euc.raw,1)

## appendix_Community_structure | chunk 13: fig-compareEuc ----
#| fig-caption: Euclidean distances among sites depends whether the data were transformed. Distances are positively correlated, but distances among some pairs of sites change their rank-order.
Y.euc.sqrt <- vegdist(Ysr, method="euclid")
plot(Y.euc.raw, Y.euc.sqrt )

## appendix_Community_structure | chunk 14 ----
Y.bray <- vegdist(Ysr, method="bray")
Y.jac <- vegdist(Ysr, method="jaccard")
Y.gow <- vegdist(Ysr, method="gower")

## appendix_Community_structure | chunk 15 ----
d.dist <- data.frame(
  Y.euc.raw, Y.euc.sqrt, Y.bray, Y.jac, Y.gow
  )
# in the event the above returns an error message, 
# uncomment and use the following:
# d.dist <- data.frame(
#   Y.euc.raw = as.numeric(Y.euc), Y.bray = as.numeric(Y.bray), 
#   Y.jac = as.numeric(Y.jac), Y.gow = as.numeric(Y.gow)
#   )

## appendix_Community_structure | chunk 16 ----
pairs(d.dist)

## appendix_Community_structure | chunk 17 ----
cor(d.dist, method="spearman")

## appendix_Community_structure | chunk 18 ----
#| message: false
m1 <- metaMDS(Y, distance="euc",
              autotransform = FALSE,
              k=2, # number of dimensions
              trace=0 # silence simulation progress info
)
m1
plot(m1, type="t")

## appendix_Community_structure | chunk 19 ----
m2 <- metaMDS(Y, distance="bray",
              autotransform = TRUE,
              k=2, # number of dimensions
              trace=0 # silence simulation progress info
)
m2
plot(m2, type="t")

## appendix_Community_structure | chunk 20 ----
dw <- read.csv("data/WHITTAKER_TABLE_3.csv")
dw[is.na(dw)] <- 0
# transpose the data, but exclude the first column of species names

dt <- t(dw[,-1])
# use the species names as column names
colnames(dt) <-  dw$SP

## appendix_Community_structure | chunk 21 ----
m.whit1 <- metaMDS(dt, distance="euc",
              autotransform = FALSE,
              k=2, # number of dimensions
              trace=0 # silence simulation progress info
)

## appendix_Community_structure | chunk 22 ----
m.whit2 <- metaMDS(dt, distance="bray",
              autotransform = TRUE,
              k=2, # number of dimensions
              trace=0 # silence simulation progress info
)

## appendix_Community_structure | chunk 23 ----
#| fig-show: 'hold'
plot(m.whit1, display="sites", type="t")
plot(m.whit2, display="sites" , type="t")
