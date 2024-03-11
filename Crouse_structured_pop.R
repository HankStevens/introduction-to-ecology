################
## Analysis of Loggerhead sea turtle population dynamics
## Based on D. T. Crouse, L. B. Crowder, and H. Caswell. 1987. A stage-based population model for loggerhead sea turtles and implications for conservation. Ecology, 68:1412–1423.
###########

## load packages
library(tidyverse)

## check my current working directory - it should be Rwork
getwd() #

## enter the data directly, as a *transition matrix*
A <- matrix(
  c(0,      0,      0,      0,    127,      4,    80,
    0.6747, 0.7370, 0,      0,      0,      0,     0,
    0,      0.0486, 0.6610, 0,      0,      0,     0,
    0,      0,      0.0147, 0.6907, 0,      0,     0,
    0,      0,      0,      0.0518, 0,      0,      0,
    0,      0,      0,      0,      0.8091, 0,      0,
    0,      0,      0,      0,      0,      0.8091, 0.8089), 
  nrow=7, ncol=7, byrow=TRUE)

## Create a hypothetical population with these stages
## THIS IS THE STAGE DISTRIBUTION
N0 <- c(100, # hatchlings
        100, # small juveniles
        100, #large juveniles
        100, # subadults
        100, # novice breeders (1st year)
        100, # second year (1st year returning)
        100 # mature
        )
stage.labels <- c("egg/hatchings", "small_juv", "large_juv",
                 "subadults","novice","first_y_rem", "mature")

## add names to the transition matrix rows and columns
colnames(A) <- rownames(A) <- stage.labels
A

###########################
## Population projection 
############################
## Here we project the population forward one year. All we
## have to do is multiply the transition matrix by the stage distribution
N1 <- A %*% N0
N1
## What happened to the stage distribution?

## Here we repeat the process for the next year, this time 
## using year 1 in place of year 0.
## multiply a matrix times N1
N2 <- A %*% N1
N2
## What happened to the stage distribution?

############################
## Now we graph the stage distribution over 21 years.
#############################

## twenty-one years, twenty transitions
N <- matrix(0, # zeroes everywhere
            nrow = 7, # number of stages
            ncol = 21 # number of years
            )
## starting point, as above
N[,1] <- N0 # each stage has 100 inds.

## "For each year, ...
for(t in 1:20){
  ## project forward one year
  N[,t+1] <- A %*% N[,t]
  ## repeat for each subsequent year
} 

## Create a data frame to ease plotting
## R will sort stage names alphabetically unless we specify some other order.
## Here we create a data frame and specify an order for stage names
df2 <- data.frame(stage.name=stage.labels, N) %>% 
  mutate(stage.name = factor(stage.name, levels=stage.labels )) 
View(df2)

## rearrange
df2.long <- pivot_longer(df2, X1:X21, names_to="orig.matrix.column", values_to="N")
View(df2.long)
df2.long <- df2.long %>% # manipulate data set 
  mutate(year = rep(0:20, 7)) %>% # create a variable for each year and...
  select(-orig.matrix.column) # remove the old variable
head(df2.long)

## graph
ggplot(df2.long, aes(x=year, y = N, colour=stage.name)) + 
  geom_line() 

ggplot(df2.long, aes(x=year, y = N, colour=stage.name)) + 
  geom_line() + scale_y_log10() + 
  labs(title="Loggerhead stage abundances (after Crouse et al. 1987)")
ggsave("loggerhead_log10_scale.png")


######################
## Finding lambda
######################
## first we find annual change, N[t+1]/N[t], which is like lambda, but 
## changes each year.
## Each year (in each column), we add all the stages together to get N.
## Then we calculate the annual proportion growing and surviving

# sum all stages in each year
(Total.N <- colSums(N))
annual.proportion<- Total.N[2:21]/Total.N[1:20]
annual.proportion
## what is happening to this annual proportion?


#################################
## Simulation Experiment
## Here we investigate the effects of altering annual survivorship 
## in different stages on population growth rate.

## First, we would like to estimate lambda using eigenanalysis.
## Don't sweat the details. 
## Eigenanalysis is a linear algebra technique that 
## finds the general tendencies in what or how
## a matrix transforms a vector.
## An eigenanalysis generates eigenvalues and eigenvectors.
## The biggest eigenvector is the first eigenvalue, and it is
## the finite rate of increase, or "lambda".

## Often eigenvalues are *complex numbers*, which contain
## a real part and an imaginary part. Lambda is the real part of the biggest
## and first eigenvalue, so we extract that.

## Here are the eigenvalues
(ev <- eigen(A) $ values)
## we select the first one and extract just the real part
lambda <- Re( ev[1] )
lambda
## how does this compare to the annual proportions you found above?

## here we express it as r
log( lambda )

## WHAT DOES LAMBDA < 1 OR r < 0 MEAN FOR THE POPULATION?

#############################
## Now we do experiments to test the effect of increasing survival
## in different stages.
## We will change each survival and stage transition,
## one stage at a time
##############################

## create a copy of the transition matrix
A1 <- A
## Replace the hatchling survival and transition
## into the small juvenile stage with a 
## 100% survival and transition
A1[2,1] <- 0.9999999 # transition probability 1 = 100% survival
## calculate the new lambda and express as r
lambda1 <- Re( eigen(A1)$values[1] )
log(lambda1) # This is r when we set hatchling survivorship = 0.999999
## what is the intrinsic rate of increase?


#####################
## To calculate transitions with near-perfect annual survival requires
## a particular calculation that we won't explain here.

## Small Juveniles (stage 2)
## Annual survivorship (Table 3, Crouse et al. 1987)
p <- 0.7857
## number of years in the stage (Table 3, Crouse et al. 1987)
d <- 7

## Calculate transitions that Crouse used ...
## the probability of surviving in that stage
(P <- (1-p^(d-1))/(1-p^d)*p)
## the probability of surviving and growing into the next stage
(G <- p^d * (1-p)/(1-p^d))

## Experiment and effectively eliminate mortality
## First, calculate the probability of surviving in that stage
p = 0.9999999

(P2 <- (1-p^(d-1))/(1-p^d)*p)
(G2 <- p^d * (1-p)/(1-p^d))
## how do these compare to those that Crouse used?

## Now USE the new transition values for small juveniles
A2 <- A
## survival
A2[2,2] <- P2
## transition
A2[3,2] <- G2
## calculate the new r when we set small juv. survivorship = 0.999999
log( Re(eigen(A2)$values[1] ) )
## what is the new growth rate?


############ Large Juveniles (stage 3)
## Annual survivorship (Table 3, Crouse et al. 1987)
p <- 0.6758
## number of years in the stage (Table 3, Crouse et al. 1987)
d <- 8


## confirm that we get what Crouse got
(P <- (1-p^(d-1))/(1-p^d)*p)
(G <- p^d * (1-p)/(1-p^d))


## Experiment and effectively eliminate mortality
## First, calculate the probability of surviving in that stage
p = 0.9999999

(P3 <- (1-p^(d-1))/(1-p^d)*p)
(G3 <- p^d * (1-p)/(1-p^d))

## Now USE the new transition values for large juveniles
A3 <- A
## no mortality
## survival
A3[3,3] <- P3
## transition
A3[4,3] <- G3
## calculate the new r when we set large juv. survivorship = 0.999999
log( Re( eigen(A3)$values[1] ) )
## what is the new growth rate?


############ Mature adults (stage 7)
## Annual survivorship (Table 3, Crouse et al. 1987)
p <- 0.8091
## number of years in the stage (Table 3, Crouse et al. 1987)
d <- 32


## confirm that we get what Crouse got
(P <- (1-p^(d-1))/(1-p^d)*p)
(G <- p^d * (1-p)/(1-p^d))


## Experiment and effectively eliminate mortality
## First, calculate the probability of surviving in that stage
p = 0.9999999

(P7 <- (1-p^(d-1))/(1-p^d)*p)
## there is no growth into another stage because this is the
## largest, oldest stage

## Now USE the new transition values for mature breeders
A7 <- A
## no mortality
## survival
A7[7,7] <- P7
## no transition
## calculate the new r when we set mature breeder survivorship = 0.999999
log( Re( eigen(A7)$values[1] ) )
## what is the new growth rate?

## Why do you think that our experiment with large juveniles had a bigger effect on growth rate than our experiment with adults? After all, adults actually lay eggs and large juveniles do not. 

