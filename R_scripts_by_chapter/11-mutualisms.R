# Mutualisms: Do ants protect pea plants? ----
# Source: 11-mutualisms.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Preparatory steps ----

## Background on partridge pea and friends ----

## Your hypothesis and predictions ----

## Data analysis ----

### code-chunk-1 ----
#| eval: false
setwd("C:/Users/chenjoe/BIO209W/Rwork")

### code-chunk-2 ----
getwd()

### code-chunk-3 ----
#| message: false
## This script is for chapter XX in Hank's Intro to Ecology primer.

## We always use...
library(tidyverse)
## or usually just ggplot2 and dplyr packages within the tidyverse.


### The data ----

#### code-chunk-4 ----
#| echo: false
d <- read.csv("data/ArthropodObservations.csv",
              skip=2)

#### code-chunk-5 ----
#| eval: false
## NOTE that we skip the first 2 lines because they are metadata
d <- read.csv("ArthropodObservations.csv",
              skip=2)

#### code-chunk-6 ----
# glimpse(d)
# or 
str(d)

## Display the data to evaluate your predictions ----

### code-chunk-7 ----
#| eval: false
ggplot(data=DATA, aes(x=IV_1, y=DV, color=IV_2)) + 
  geom_boxplot(notch = TRUE) + 
  facet_grid( IV_3 ~ IV_4)

## Deliverables ----
