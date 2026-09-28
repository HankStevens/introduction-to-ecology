# Community structure ----
# Source: appendix_Community_structure.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Preparatory steps ----

## Background ----

## DRAFT ----

## Community structure ----

### Background ----

#### code-chunk-1 ----
#| include: false
library(kableExtra) # for making tables in Rmd and Qmd files

#### code-chunk-2 ----
#| message: false
library(vegan) # multivariate analysis
library(tidyverse) # data wrangling and graphics

#### code-chunk-3 ----
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

#### tbl-whit ----
#| label: tbl-whit
#| echo: false
#| tbl-cap: Small subset of data from Whittaker (1956) Fig. 4 & Table 3, reflecting a moisture gradient.

kbl(d, booktabs=TRUE)

#### code-chunk-5 ----
#| results: hide
d2 <- d %>% select(moisture, Tilia, Tsuga, Halesia, Quercus, Pinus)

# if the above generates an error message, it may be because
# of 'select()' refers to the wrong R package. If you get an errror, 
# try referring specifically to a particular package with the following code. 
# Uncomment the following and use this instead.
# d <- d %>% dplyr::select(moisture, Tilia, Tsuga, Halesia, Quercus, Pinus)

d2

#### tbl-whit2 ----
#| label: tbl-whit2
#| echo: false
#| tbl-cap: Reorganizing Table 1 to comunicate better.

kbl( round(d2,0) )

#### code-chunk-7 ----
yl <- d2 %>%
  pivot_longer(cols=Tilia:Pinus, values_to="abundance", names_to="species")
head(yl)

#### fig-whit1 ----
#| label: fig-whit1
#| fig-cap: Tree species abundances along a moisture gradient in the Smoky Mountains of North Carolina, USA (Whittker 1956). Y-axis scaled by square root transformation.

ggplot(data=yl, aes(x=moisture, y=abundance, 
               color=species, linetype=species)) + 
  geom_line() +
  scale_y_sqrt() +
  labs(y="Abundance", x="Moisture index")

## Dissimilarities between sites ----

### Treating sites and species equally ----

#### code-chunk-9 ----
Y <- d2 %>% select(-moisture)
Yw <- wisconsin(Y)
# totals of each site (rows)
apply(Yw, MARGIN=1, sum)

# resulting maxima of each species (cols) 
apply(Yw, MARGIN=2, max)

#### code-chunk-10 ----
# Using a function in vegan
decostand(Y, method="max", MARGIN=2)

# Doing the same thing in base R
apply(Y, 2, function(x) x/max(x)) 

#### code-chunk-11 ----
# Use square roots of our data
Ysr <- apply(Y, 2, function(x) (x^0.5)) 
Ysr

#### code-chunk-12 ----
Y.euc.raw <- vegdist(Y, method="euclid")
round(Y.euc.raw,1)

#### fig-compareEuc ----
#| label: fig-compareEuc
#| fig-caption: Euclidean distances among sites depends whether the data were transformed. Distances are positively correlated, but distances among some pairs of sites change their rank-order.
Y.euc.sqrt <- vegdist(Ysr, method="euclid")
plot(Y.euc.raw, Y.euc.sqrt )

#### code-chunk-14 ----
Y.bray <- vegdist(Ysr, method="bray")
Y.jac <- vegdist(Ysr, method="jaccard")
Y.gow <- vegdist(Ysr, method="gower")

#### code-chunk-15 ----
d.dist <- data.frame(
  Y.euc.raw, Y.euc.sqrt, Y.bray, Y.jac, Y.gow
  )
# in the event the above returns an error message, 
# uncomment and use the following:
# d.dist <- data.frame(
#   Y.euc.raw = as.numeric(Y.euc), Y.bray = as.numeric(Y.bray), 
#   Y.jac = as.numeric(Y.jac), Y.gow = as.numeric(Y.gow)
#   )

#### code-chunk-16 ----
pairs(d.dist)

#### code-chunk-17 ----
cor(d.dist, method="spearman")

## Ordination - a huge topic ----

### Unconstrained ordination ----

#### code-chunk-18 ----
#| message: false
m1 <- metaMDS(Y, distance="euc",
              autotransform = FALSE,
              k=2, # number of dimensions
              trace=0 # silence simulation progress info
)
m1
plot(m1, type="t")

#### code-chunk-19 ----
m2 <- metaMDS(Y, distance="bray",
              autotransform = TRUE,
              k=2, # number of dimensions
              trace=0 # silence simulation progress info
)
m2
plot(m2, type="t")

## And now with a larger data set ----

### code-chunk-20 ----
dw <- read.csv("data/WHITTAKER_TABLE_3.csv")
dw[is.na(dw)] <- 0
# transpose the data, but exclude the first column of species names

dt <- t(dw[,-1])
# use the species names as column names
colnames(dt) <-  dw$SP

### code-chunk-21 ----
m.whit1 <- metaMDS(dt, distance="euc",
              autotransform = FALSE,
              k=2, # number of dimensions
              trace=0 # silence simulation progress info
)

### code-chunk-22 ----
m.whit2 <- metaMDS(dt, distance="bray",
              autotransform = TRUE,
              k=2, # number of dimensions
              trace=0 # silence simulation progress info
)

### code-chunk-23 ----
#| fig-show: 'hold'
plot(m.whit1, display="sites", type="t")
plot(m.whit2, display="sites" , type="t")
