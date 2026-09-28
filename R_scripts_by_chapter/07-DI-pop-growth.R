# Density-independent Population Growth: Humans on the rise ----
# Source: 07-DI-pop-growth.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Preparatory Steps ----

## Background ----

### Population growth of *Homo sapiens*: a case study ----

### Projecting population size into the future ----

## Follow the Logic: Why does this equation exist? ----

## Methods ----

### code-chunk-1 ----
library(tidyverse) # for readr, dplyr and others
library(patchwork) # to combine ggplot2 figures

### Load data ----

#### code-chunk-2 ----
d <- read_csv("data/HumanPopData-2023-01-01.csv", 
              skip=3 # skip three lines of metadata
              )
glimpse(d)

#### fig-p1 ----
#| label: fig-p1
#| fig-cap: "The human population has increased over time. Note the common log scale on the y-axis. Note that years before the current era (BCE)  are negative numbers, while numbers in the current era (CE) are positive."
d %>% 
  ggplot(aes(x=Year, y=est.N)) + 
  geom_point() + geom_line() + 
  scale_y_log10() + labs(y="Estimated human population size") 

### Simple math: Calculations by hand ----

#### code-chunk-4 ----
## Get the right time points and find h
t <- 100
# recall h = (t+h) - t
t.plus.h <- 200
h  <- t.plus.h - t
t; t.plus.h; h

#### code-chunk-5 ----
## in base R
Nt <- d$est.N[d$Year == t]
Nth <- d$est.N[d$Year == t.plus.h]
Nt; Nth

#### code-chunk-6 ----
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

#### code-chunk-7 ----
# the natural log of the ratio of population sizes
logNth.over.Nt <- log(Nth / Nt)
logNth.over.Nt

#### code-chunk-8 ----
# finding r
r <- logNth.over.Nt / h
r

### Programming: Use a for-loop to do all the calculations ----

#### code-chunk-9 ----
# create an empty vector of missing values.
r <- rep(NA, nrow(d))

#### code-chunk-10 ----
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


#### code-chunk-11 ----
d.r <- cbind(d, r=r)

#### code-chunk-12 ----
d.r[257:259,]

#### code-chunk-13 ----
d.r <- d.r[1:258,]

## Changes in human population growth rate ----

### fig-n1 ----
#| label: fig-n1
#| fig.cap: "Estimates of per capita growth are positive for nearly every time interval since the agricultural revolution."
#| fig-show: 'hold'
#| out-width: '65%'

# population size, using the first data frame
ggplot(data=d.r, aes(x=Year, y=r)) + geom_line() +
  geom_point() + labs(title="Per capita growth rate")

### code-chunk-15 ----
#| fig-show: 'hide'

# filter in all years greater than or equal to 1
d.r.y1 <- d.r %>% filter(Year >= 1)

ggplot(data=d.r.y1, aes(x=Year, y=est.N)) + geom_line() +
  geom_point() + labs(title="Population Size")

ggplot(data=d.r.y1, aes(x=Year, y=r)) + geom_line() +
  geom_point() + labs(title="Per capita growth rate since 1 CE")

### fig-n2 ----
#| label: fig-n2
#| echo: FALSE
#| fig.cap: "In the current era, per capita growth remains mostly positive, and then rises sharply and begin to fluctuate dramatically after 1500."
p1 <- ggplot(data=d.r.y1, aes(x=Year, y=est.N)) + geom_line() +
  geom_point() + labs(title="Population Size")
p2 <- ggplot(data=d.r.y1, aes(x=Year, y=r)) + geom_line() +
  geom_point() + labs(title="Per capita growth rate since 1 CE")
p1 / p2 

### Your own interval ----

### How does per capita growth rate change with population size? ----

#### fig-rn ----
#| label: fig-rn
#| echo: FALSE
#| fig.cap: "Per capita growth rate vs. population size, since 1 CE after 1500. What do you make of this? :)"
ggplot(data=d.r.y1, aes(x=est.N, y=r, color=Year)) + 
  geom_point() 

## Projecting into the future ----

## Deliverables ----

### answersDIgrowth ----
#| label: answersDIgrowth
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
