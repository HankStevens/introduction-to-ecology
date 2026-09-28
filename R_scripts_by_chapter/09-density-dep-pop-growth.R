# Density-dependence in population growth ----
# Source: 09-density-dep-pop-growth.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Preparatory steps ----

## Models as assumptions ----

### fig-logistic ----
#| label: fig-logistic
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

### Math describes nature: on parameters and their meanings ----

### Solving a rate equation ----

### Assignment 1: Use math to understand the consequences of our assumptions. ----

## Follow the Logic: How to read a mathematical result ----

## Discrete difference equations ----

### Assignment 2: Explore delayed density-dependence ----

#### code-chunk-2 ----
## Define K, N0, the number of generations
K <- 100
gens <- 50
## make numeric vector for each N in each generation, plus one more
N <- numeric(gens)

## Start the population out at this number of individuals
N[1] <- 45

#### code-chunk-3 ----
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

#### code-chunk-4 ----
## graph your data
ggplot(data=my.N, aes(x=year, y=N )) + 
  geom_point() + 
  geom_line() +
  labs(title = "Discrete Logistic Growth",
       subtitle=paste("rd =", rd))

## save your graph USING A UNIQUE NAME
ggsave("figs/dynamics-rd-0.2.png", height=5, width=6)

## Deliverables ----
