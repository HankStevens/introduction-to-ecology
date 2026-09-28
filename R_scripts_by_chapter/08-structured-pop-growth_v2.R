# Structured demography: How a graduate student helped save the loggerhead sea turtle ----
# Source: 08-structured-pop-growth_v2.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Preparatory steps ----

## Demography ----

## Loggerhead sea turtles ----

### Questions about loggerhead survival ----

## Demographic modeling ----

### Connecting life history information to a model ----

### Connecting life histories to demographic matrices ----

## Analysis of a demographic matrix ----

### Population growth rate, $\lambda$ ----

#### code-chunk-1 ----
getwd()

#### code-chunk-2 ----
## for graphics and data manipulation
library(tidyverse)

#### code-chunk-3 ----
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

#### code-chunk-4 ----
original_lambda <- Re( eigen(A)$values[1] )
original_lambda

## Finding the life history stage that could make the biggest difference ----

### Helping hatchlings ----

#### code-chunk-5 ----
## new matrix 
A1 <- A 

## Change just the hatchling survival/growth
A1[2,1] <- 0.999
## look at the new matrix
A1

#### code-chunk-6 ----
## using Re() extracts just the real part of the eigenvalue, and not the imaginary part
Re( eigen(A1)$values[1] )

### Helping small juveniles ----

#### code-chunk-7 ----
## new matrix with NEW NAME, A2
## make sure you start with the ORIGINAL matrix A 
A2 <- A

## Change just the stasis and growth for small juveniles
A2[2,2] <- 0.857 # stasis
A2[3,2] <- 	0.142 # growth
A2

#### code-chunk-8 ----
Re( eigen(A2)$values[1] )

## Simulating helping other stages ----

### tbl-changes ----
#| label: tbl-changes
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

### code-chunk-10 ----
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

## Deliverables ----

### code-chunk-11 ----
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
