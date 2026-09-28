# Thinking about thinking about concepts ----
# Source: 01-thinking.qmd
# All R code chunks extracted in order of appearance.

## code-chunk-1 ----
#| echo: false
#| message: false
library(tidyverse)
library(patchwork)

theme_set( theme_bw(base_size=12) )

## Goals ----

## Introduction ----

## Observation ----

### fig-data ----
#| label: fig-data
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

#### For you to do ----

## Learning, or Bayesian Updating ----

### Four Examples of Bayesian Reasoning ----

#### Formal Bayesian reasoning ----

##### fig-learning ----
#| label: fig-learning
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



### Three important notes ----

#### For you to do ----

## Logic ----

### Deduction ----

#### Three contrasting examples ----

### Induction ----

### Abduction ----

#### For you to do ----

## Mathematics ----

## Causal Inference ----

## Emergence ----

### Interactions of emergent properties ----

## Review ----

## Cross-cutting concepts ----

### Scale ----

### Hierarchy ----

### Emergence and complexity ----
