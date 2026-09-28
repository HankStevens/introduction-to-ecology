# Of wolves and moose, on Isle Royale ----
# Source: 12-predator-prey.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Preparatory steps ----

## Background ----

## Details about our data ----

## Your hypothesis and predictions ----

## Data analysis ----

### code-chunk-1 ----
getwd()

### code-chunk-2 ----
#| message: false
## This script is for chapter XX in Hank's Intro to Ecology primer.

## We always use...
library(tidyverse)
## or usually just ggplot2 and dplyr packages within the tidyverse.

### code-chunk-3 ----
#| eval: false
## For this chapter we also use deSolve
install.packages("deSolve")

### code-chunk-4 ----
library(deSolve)

### Load the data ----

#### code-chunk-5 ----
#| echo: false
d <- read.csv("data/isleRoyaleData.csv",
              skip=2)

#### code-chunk-6 ----
#| eval: false
## NOTE that we skip the two lines because they are metadata
d <- read.csv("isleRoyaleData.csv",
              skip=2)

#### code-chunk-7 ----
# glimpse(d)
# or 
str(d)

#### fig-mw1 ----
#| label: fig-mw1
#| fig-cap: "Natural populations undergo fluctuations. Here we smooth the data over different proportions of the time series to reveal different patterns of squiggliness."
#| message: false
ggplot(d, aes(Year, N, colour=Species)) + geom_line() + 
  facet_grid(Species~., scales = "free") + 
  geom_smooth( se=FALSE, span=.2, color="red", linewidth=.5) + 
  geom_smooth( se=FALSE, span=.7, color="black", linewidth=.5) 
ggsave("figs/MooseWolfTS.png", height=5, width=6)

## Population growth models ----

### Examples of population growth models ----

## Predator-prey equations ----

## Lotka-Volterra prey-dependent predation ----

## Follow the Logic: What we are about to do, and why ----

### fig-MWpp ----
#| label: fig-MWpp
#| fig-cap: "Use this phase plane plot to count how many times the populations undergo a complete counterclockwise cycle."
dw <- pivot_wider(d, names_from=Species, values_from=N)
ggplot(dw, aes(Moose, Wolf, colour=Year)) + 
  geom_path(arrow=arrow(length=unit(3, "mm"), angle = 15, type = "open")) + 
  labs(subtitle="Each arrow is one annual time step.")
ggsave("figs/MooseWolfPP.png", height=5, width=5.5)

### Equilbria ----

## Follow the Logic: Reading the equilibrium ----

### code-chunk-10 ----
View(dw)
mean(dw$Wolf)
mean(dw$Moose)

### code-chunk-11 ----
# the number of observations or rows of data
n <- nrow(dw)

# predator and prey numbers for all years except the last.
Pt <- dw$Wolf[-n]
Nt <- dw$Moose[-n]

# predator and prey numbers for all years except the first
Pt1 <- dw$Wolf[-1]
Nt1 <- dw$Moose[-1]

### Intrinsic rate of increase, r ----

#### code-chunk-12 ----
# each year's r
r <- log(Pt1/Pt)
r
# the maximum
rMax <- max(r)
rMax

### Attack rate, a ----

#### code-chunk-13 ----
a <- rMax / mean(dw$Wolf)
a

### Predator mortality rate, m ----

#### code-chunk-14 ----
# mortality, estimated as the minimum log( P[t+1] / P[t] )
m <- abs( min(r) )
m

### Conversion efficiency, b ----

#### code-chunk-15 ----
b <- m / (a * mean(dw$Moose))
b

## Predicting population dynamics ----

### code-chunk-16 ----
predpreyLV <- function(t, y, p) {
    with( as.list( c(y,p) ), {
      dN <- r*N - a*N*P
      dP <- b*a*N*P - m*P
      return( list( c(dN, dP) ))
    })
}

### code-chunk-17 ----
parms <- c(r = rMax, a=a, b=b, m=m)
y0 <- c(N=560, P=20)
times <- seq(0, 52, by=0.1)

### code-chunk-18 ----
out <- ode(y=y0, t=times, fun=predpreyLV, parms=parms)

### fig-LVts ----
#| label: fig-LVts
#| fig-cap: "Predicted Lotka-Volterra dynamics based on the Isle Royale moose-wolf data."
op <- out %>% as.data.frame() %>%
  pivot_longer(cols=N:P, names_to = "Populations", 
                   values_to="Number")

ggplot(op, aes(x=time, y=Number, colour=Populations)) + geom_line() +
      labs(x="Years") + scale_color_discrete(breaks=c("N","P")) + 
      facet_grid(Populations~., scales="free")

ggsave("figs/LV_MooseWolfTS.png", height=5, width=6)

### fig-LVpp ----
#| label: fig-LVpp
#| fig-cap: "Phase plane diagram of the Lotka-Volterra model of moose wolf dynamics. Once the paramters are fixed and the initial abundances are selected, then the dynamics are completely determined. Initial abundances are moose (560) and wolves (20). The line is black and widest in 1959, and becomes progressively more narrow and blue over time."
outw <- out %>% as.data.frame() 

ggplot(outw, aes(N, P, linewidth= -time, color=time)) + 
  geom_path() + 
  labs(x="Moose population size", y="Wolf population size" )
ggsave("figs/LV_MooseWolfPP.png", height=5, width=6)

## Discussion questions ----

## Deliverables ----
