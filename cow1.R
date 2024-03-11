library(deSolve)
library(tidyverse)
library(patchwork)

ffc <- 9.3 # +/- 0.5 fossil fuel consumption
luc <- 1.0 # +/- 0.5 land use change 
lsink <- 3.1 # +/- 0.9 land sink (uptake by plants)
osink <- 2.6 # +/- 0.5 ocean sink (uptake by algae + diffusion)

# Wikipedia: **Each part per million** of CO2 in the atmosphere represents approximately 2.13 gigatonnes of carbon, or 7.82 gigatonnes of CO2.[20]


Atotal <- 2.13 * 420

C = 12
CO2 = C + 16 + 16

## C to CO2
Atotal * CO2/C


## C to ppm
Atotal / 2.13 * 7.82







# 5.7 Gt C 
ppmFlux <- 5.7*1/2.13

# instantaneous rate
current_ppm <- 420
e <- log( (current_ppm+ppmFlux)/current_ppm)
e


sigma <- 5.67*10^-8 # Stefan-Boltzmann constant
a <- 0.3 # albedo
Omega <- 1372 # W/m^2 flux from the sun (omega/4 is the average of what hits Earth's surface)


###############
# exponent w co2 ppm
K <- c(255, 288, 290)^4
A <- c(0, 280, 450)

summary(m <- nls(K ~ .7*1372/(4*5.67e-8*A^g), start=list(g=-.08)))
g <- as.numeric( coef(m) )
g
curve(( .7*1372/(4*5.67e-8*x^g ) )^.25, 100, 500)

# check temp
( (1-a)*Omega/(4*sigma0*255^g) )^(1/4)
( (1-a)*Omega/(4*sigma0*450^g) )^(1/4)



atmosphere1 <- function(t,y,p){
  with( as.list(c(y, p)), {
    dA.dt <- I - e*A
    return(list( c(dA.dt) 
    ) )
  })
}

p3 <- c(e=e, I=4.5)
y0 <- c(A=400); 
t <- seq(0, 500, 1)
output <- ode(y=y0, times=t, func=earth3, parms=p3)
plot(output)



################################
## with CO2 and temperature ####
################################
earth4 <- function(t,y,p){
  with( as.list(c(y, p)), {
    #dA.dt <- I - E
    # dA.dt <- I - e*A
    dA.dt <- I + (1-A/280)
    F.in <- Omega/4 
    F.out <- a*Omega/4 + (sigma/A^g)*K^4
    dH.dt <- (F.in - F.out)/lag
    return(list( c(dA.dt, dH.dt) , 
                 K_eq = ( (1-a) * Omega / (4*(sigma)) * A^g )^(1/4)
    ) )
  })
}

# assume pre-industrial era:
## average temp in Kelvin was 288 (~ 273.15 + 15)
## CO2 eq. = 250 ppm
## H* = r/sigma * A = r/sigma * 250
# Let

t <- seq(0, 77, 1/365)
p <- c(Omega = 1372, # incoming solar radiative forcing
       a=0.3, # albedo
       sigma = 5.67e-8, # Stefan-Boltzmann constant for radiative loss
       g = 0.08492, # greenhouse effect
       I = 4.5, # net gigatonnes C per year
     
       lag = 50 # lag time to partial temp equilibration
)
y0 <- c(A=420, K=289)

output <- ode(y=y0, times=t, func=earth4, parms=p)
plot(output)

p1 <- output %>% as.data.frame() %>%
  mutate(
    Year = time + 2023
  ) %>%
  ggplot(aes(x=Year, y=A)) + 
  geom_line() + labs(y="Atmospheric CO2 (ppm)") +
  theme_bw()

out.long <- output %>% as.data.frame() %>%
  select(time, K, K_eq) %>%
  transmute(
    Year = time + 2023,
    degC = K -273,
    degC_eq = K_eq -273
  ) %>%
  pivot_longer(cols=-Year, names_to = "State_variable", 
               values_to="Value")

p2 <- ggplot(out.long, aes(x=Year, y=Value, color=State_variable)) + 
  geom_line() + 
  labs(y="Degrees Celcius")  +
  theme_bw()

p2 / p1




#################
## roughgarden
rg11 <- function(t, y, p){
  with(as.list(c(y,p)), {
    db.dt <- (q - h*(b-a))/w
    return(list(c(db.dt)))
  })
}

p <- c(a =18, # atmosphere deg C
       h=50, # heat transfer coef cal/(hr degC)
       q=1000, # sol rad cal/hr
       w = 10 # grams
)

t <- seq(0, 2, .01)

b.initial <- c(b=30)

output <- ode(y=b.initial, times=t, func=rg11, parms=p)

plot(output)

