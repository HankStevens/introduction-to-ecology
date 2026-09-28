# Behavior: Extra-pair copulation in a monogamous species ----
# Source: 06-extrapair_copulation.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Preparatory Steps ----

## Background: What causes extra pair copulation in prairie voles? ----

### Tinbergen's four questions ----

### Determinants of extra-pair copulation in voles ----

## Data from the Solomon/Keane lab ----

### code-chunk-1 ----
#| echo: false
a <- read.csv("data/epr_209.csv")
a[1:4,]
rm(d209)

## Your objectives ----

### Write your own script ----

#### code-chunk-2 ----
#| eval: false
getwd()

#### code-chunk-3 ----
library(tidyverse)

#### code-chunk-4 ----
d209 <- read.csv("data/epr_209.csv")
str(d209)

### Recoding "pairtype" ----

#### code-chunk-5 ----
d209 <- d209 %>%
  mutate(
    epc = ifelse(pairtype=="extra", 1, 0)
  )
str(d209)

## Graphing in `ggplot()` ----

### code-chunk-6 ----
#| eval: false
library(tidyverse)

### code-chunk-7 ----
glimpse(mpg)

### bp1 ----
#| label: bp1
#| fig.cap: "Box-and-whisker plot of highway mileage (hwy) vs. car type (class), with explicit labels. A boxplot shows the distribution of data, by groups."
ggplot(mpg, aes(x = class, y = hwy)) + geom_boxplot() + 
  labs(x="type of car", y="highway mileage")

#### Scatterplots ----

##### sp1 ----
#| label: sp1
#| fig.cap: "Scatterplot of highway mileage vs. city mileage (miles per gal)."
ggplot(mpg, aes(x=cty, y=hwy)) + geom_point() + 
  labs(x="city mileage", y="highway mileage")

##### splm ----
#| label: splm
#| fig.cap: "Scatterplot with a fitted straight line, fitting a linear model."
ggplot(mpg, aes(x=cty, y=hwy)) + geom_point() + geom_smooth(method="lm")

##### sploess ----
#| label: sploess
#| fig.cap: "Scatterplot with a flexibly fitted line, using local polynomial regression fitting."
## for a scatterplot with a fitted curved line 
ggplot(mpg, aes(x=cty, y=hwy)) + geom_point() + geom_smooth()

#### Categorical responses ----

##### code-chunk-12 ----
mpg2 <- mpg %>% 
  mutate(
    prob.front.wheel.drive = ifelse(drv=="f", 1, 0)
  )

##### bp2 ----
#| label: bp2
#| fig.cap: "Higher mileage cars tend to have front-wheel drive. Graph shows the probability that a car with a particular city mileage is front wheel drive. The curve is the result of a type of generalized linear model (glm) called *logistic regression* which assumes binomial data."
ggplot(mpg2, aes(cty, prob.front.wheel.drive)) + 
  geom_point() + 
  geom_smooth(method="glm", 
              method.args = list(family = "binomial"))

## Deliverables ----
