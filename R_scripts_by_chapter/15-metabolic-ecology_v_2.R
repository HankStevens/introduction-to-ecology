# Metabolic Ecology: How do body size and temperature affect nutrient cycling? ----
# Source: 15-metabolic-ecology_v_2.qmd
# All R code chunks extracted in order of appearance.

# Metabolic Ecology: How do body size and temperature affect nutrient cycling? ----

## setup ----
#| label: setup
#| include: false
library(tidyverse)
library(broom)

## Acknowledgement ----

## Goals ----

## Preparatory steps ----

## Background ----

### The Metabolic Theory of Ecology: Why body size and temperature matter ----

### Allometry: how rates scale with body size ----

## Follow the Logic: Why log-transform? ----

### Temperature dependence and the Arrhenius Equation ----

### Nutrient excretion by fish ----

## Using logarithms ----

## Hypotheses, our data, and predictions ----

### Predictions ----

## Getting R ready for work ----

### code-chunk-2 ----
#| eval: false
install.packages("broom")

### code-chunk-3 ----
#| eval: false
library(tidyverse)
library(broom)

### code-chunk-4 ----
# Note we skip the first 8 lines because they are 'metadata',
# which is 'data about data` or information about the variables.

fish <- read.csv("data/fish_nutrient_excretion.csv", skip=8)
glimpse(fish)

### code-chunk-5 ----
summary(fish)

## Exploring the data ----

### code-chunk-6 ----
boltzmann_k <- 8.617333262e-5

### code-chunk-7 ----
## Add log-transformed columns to the data
fish <- fish %>% #start with our fish data set, and
  mutate(log_mass = log(mass_g), # create ln(mass)
         log_N = log(N_u.h), # ln(N excretion)
         log_P = log(P_u.h), # ln(P excretion)
         temp_K = 273.15 + temp_C, # degrees Kelvin
         inv.kT = 1/(boltzmann_k * temp_K) # inverse scaled Kelvin
  )
         

### Raw-scale plots ----

#### fig-raw-N ----
#| label: fig-raw-N
#| fig-cap: "Nitrogen excretion rate vs. body mass on arithmetic (raw) axes."
ggplot(fish, aes(x = mass_g, y = N_u.h)) +
  geom_point(alpha = 0.5) +
  labs(x = "Body mass (g)",
       y = expression(paste("N excretion (", mu, "mol ", fish^{-1}, " ", hr^{-1}, ")")),
       title = "N excretion vs. body mass") +
  theme_minimal()

### Log-transformed plots ----

#### fig-log-N ----
#| label: fig-log-N
#| fig-cap: "Log nitrogen excretion rate vs. log body mass. If the allometric model holds, this should be approximately linear."
ggplot(fish, aes(x = log(mass_g), y = log(N_u.h))) +
  geom_point(alpha = 0.5) +
  labs(x = expression(log[10]~"(body mass, g)"),
       y = expression(log[10]~"(N excretion, " * mu * "mol " * fish^{-1} * " " * hr^{-1} * ")"),
       title = "Allometric scaling of N excretion") +
  theme_minimal()

#### fig-regression-N1 ----
#| label: fig-regression-N1
#| fig-cap: "Linear regression of log N excretion on log body mass, for each temperature."
ggplot(fish, aes(x = log_mass, y = log_N)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~temp_C) + 
  labs(x = expression(log~"(body mass, g)"),
       y = expression(log~"(N excretion)"),
       title = "Allometric scaling of N excretion") +
  theme_minimal()

### Allometric and temperature scaling of excretion rates ----

#### Mulltiple linear regression ----

##### code-chunk-11 ----
## Fit the regression
m_N_all <- lm(log_N ~ log_mass + inv.kT, data = fish)

##### code-chunk-12 ----
plot(m_N_all, which=1)

#### Evaluating the predictions ----

##### code-chunk-13 ----
## View the results
# ignore the intercept
confint(m_N_all)

#### **Do the same** for P excretion. ----

##### code-chunk-14 ----
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

### Thinking bigger: ecosystem consequences ----

#### code-chunk-15 ----
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



## Deliverables ----
