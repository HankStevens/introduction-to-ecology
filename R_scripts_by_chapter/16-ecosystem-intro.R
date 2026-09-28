# An Introduction to the Carbon Cycle and Global Heating ----
# Source: 16-ecosystem-intro.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Preparatory steps ----

## Get R ready for work ----

### code-chunk-1 ----
getwd()

### code-chunk-2 ----
#| message: false
## This script is for chapter XX in Hank's Intro to Ecology primer.

## We always use...
library(tidyverse)
## or usually just ggplot2 and dplyr packages within the tidyverse.
## In this chapter we also need
library(deSolve)
# for solving or integrating differential equations

## Background on ecosystems ----

## A simple budget provides one explanation for why we have so much carbon in the atmosphere ----

### Rate of change ----

#### code-chunk-3 ----
or <- 91 # diffusion out of the ocean, due to respiration of microbes
ro <- 1 # river outgassing (diffusion)
fi <- 4 # fire
cp <- 0.1 # cement production
ff <- 7.6 # fossil fuel combustion (1998) 
lu <- 1.5 # land use change 
tr <- 56 # terrestrial respiration
inputs <- or + ro + fi + cp + ff + lu + tr
inputs

#### code-chunk-4 ----
npp.o <- 92 # net primary production in the ocean (diffusion)
we <- 0.2 # Weathering of carbonate rock
ls <- 2.8 # land sink (burial)
npp.l <- 60 # net primary production on land

outputs <- npp.o + we + ls + npp.l
outputs

#### code-chunk-5 ----
net.flux <- inputs - outputs
net.flux # in Pg C per year

#### code-chunk-6 ----
# the pool
At <- 762

#### code-chunk-7 ----
At.plus.1 <- At + net.flux
At.plus.1

#### code-chunk-8 ----
#| echo: false
#| results: hide

net.flux

t <- c(2,25)
at.2.and.25 <- At + net.flux * t
at.2.and.25

#### code-chunk-9 ----
## write code two years and for 25 years
## 

## Simple dynamics emerge from processes like diffusion, and complicate prediction ----

### A first order flux ----

### Earth's atmosphere as a first order equation ----

#### code-chunk-10 ----
respiration <- 91 # Pg
photosynthesis <- 91 # Pg
carbonic_acid <- 1 #Pg

#### code-chunk-11 ----
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

#### code-chunk-12 ----
# Constant influx, and diffusion rate
p1 <- c(I=6.2, e=0.008)
# starting atmospheric carbon pool
y0 <- c(A=762);

#### code-chunk-13 ----
# from zero to 100, by steps of one year
t <- seq(from=0, to=1000, by=1)
# integrate the rate equation for all time points, 
# starting with y0 at time zero, using our function and parameters.
output <- ode(y=y0, times=t, func=atmosphere1, parms=p1)

#### code-chunk-14 ----
#| eval: true
# Finally, we plot the output.
out_df <- as.data.frame(output) 
firstorderA <- ggplot(out_df, aes(time, A)) + geom_line() + 
  labs(y="Atmospheric carbon pool (Pg)",
       x="Year") + 
  theme_bw()
firstorderA
ggsave("figs/firstOrderAtmosphere.png", width=5, height=4)

#### code-chunk-15 ----
#| echo: false
#| results: hide
asymp <- with(as.list(p1), I/e)
asymp

## Deliverables, part 1 ----

### code-chunk-16 ----
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


## Linking two different ecosystem processes shows lagged effects ----

### blackK ----
#| label: blackK
#| eval: false
Omega <- 1372
sigma <- 5.67e-8
albedo <- 0.3

temp_K <- ( (1-albedo) * Omega/(4*sigma) )^(1/4)
temp_K

### Adding greenhouse (heat-trapping) gases ----

#### Intuition about $T = F(A)$ ----

##### code-chunk-18 ----
#| echo: false
K <- c(255, 288, 290)^4
A <- c(0, 280, 450)
m <- nls(K ~ .7*1372/(4*5.67e-8*(A+1)^g), start=list(g=-.08))
g <- - as.numeric( coef(m) )

##### fig-g ----
#| label: fig-g
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

### Why is all this math important? ----

### Expected temperatures now, and also what the temperature would become ----

## Simulations: *in silico* experiments ----

### code-chunk-20 ----
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


### code-chunk-21 ----
t <- seq(0, 77, 1/52)

### code-chunk-22 ----
y0 <- c(A=420, # CO2 equivalents in ppm in 2023
        K=289 # degrees Kelvin in 2023
        )

### code-chunk-23 ----
#| message: false
library(tidyverse) # data wrangling and graphics
library(deSolve) # for numerical integration of ODE models

### Experiment 1: Business as usual ----

#### code-chunk-24 ----
parameters1 <- c(Omega = 1372, # incoming solar radiative forcing
       a=0.3, # albedo
       sigma = 5.67e-8, # Stefan-Boltzmann constant for radiative loss
       g = 0.08488, # greenhouse constant
       I = 2.1, # increase in CO2 ppm per year
       lag = 50 # lag time to partial temp equilibration
)

#### code-chunk-25 ----
output1 <- ode(y=y0, 
               times=t, 
               func=Earth1, 
               parms=parameters1)
head(output1)

#### code-chunk-26 ----
tail(output1)

#### code-chunk-27 ----
p1.1 <- output1 %>% as.data.frame() %>%
  mutate(
    Year = time + 2023
  ) %>%
  ggplot(aes(x=Year, y=A)) + 
  geom_line() + labs(y="Atmospheric CO2 (ppm)") +
  theme_bw()
p1.1
ggsave("figs/Exp1_CO2.png", plot=p1.1)

#### code-chunk-28 ----
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

#### code-chunk-29 ----
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

### Experiment 2: Keep CO2 at 2023 levels (420 ppm) ----

#### code-chunk-30 ----
parameters2 <- c(Omega = 1372, # incoming solar radiative forcing
       a=0.3, # albedo
       sigma = 5.67e-8, # Stefan-Boltzmann constant for radiative loss
       g = 0.08492, # greenhouse constant
       I = 0.5, # increase in CO2 ppm per year
       lag = 50 # lag time to partial temp equilibration
)

#### code-chunk-31 ----
output2 <- ode(y=y0, times=t, func=Earth1, parms=parameters2)
tail(output2)

#### code-chunk-32 ----
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

#### code-chunk-33 ----
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

### Experiment 3: Reduce CO2 to 1980 levels (338 ppm) ----

#### code-chunk-34 ----
parameters3 <- c(Omega = 1372, # incoming solar radiative forcing
       a=0.3, # albedo
       sigma = 5.67e-8, # Stefan-Boltzmann constant for radiative loss
       g = 0.08492, # greenhouse constant
       I = 0.357, # increase in CO2 ppm per year
       lag = 50 # lag time to partial temp equilibration
)

#### code-chunk-35 ----
output3 <- ode(y=y0, times=t, func=Earth1, parms=parameters3)
tail(output3)

#### code-chunk-36 ----
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

#### code-chunk-37 ----
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

## Deliverables, part 2 ----

### code-chunk-38 ----
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
