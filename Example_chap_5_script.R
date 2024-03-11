# Pat Smith
# Homework ___
#February 14



setwd ("/Users/smithpk/downloads/A - BIO 672/") #set working directory
getwd() #check that directory is set

#install packages
install.packages("bbmle")
install.packages("deSolve")
install.packages("LakeMetabolizer")
install.packages("lubridate")
install.packages("tidyverse")

#loadpackages
library("bbmle")
library("deSolve")
library("LakeMetabolizer")
library("lubridate")
library("tidyverse")

#define the function for the model
NPZD<-function(t,state,parameters) #Nutrient, Phytoplankton, Zooplankton, and Detritus NPZD
{ # open paratheses to lay out function parameters
  with( # creates a local environment within which R can find things.
    as.list(c(state,parameters)), {  # creates a list with names for the environment
    PAR <- 0.5*(540+440*sin(2*pi*t/365-1.4)) #calculate photosynthetically active radiation
    din            <- max(0,DIN) #dissolved inorganic nitrogen
    Nuptake <- maxUptake * PAR/(PAR+ksPAR) * din/(din+ksDIN)*PHYTO #mmol N m^-3d^-1 #f1 #equation for nitrogen uptake
    
    Grazing        <- maxGrazing * PHYTO/(PHYTO + ksGrazing)*ZOO #f2 zooplankton grazing
    Faeces         <- pFaeces * Grazing #f3 zooplankton faeces production
    Excretion      <- excretionRate * ZOO #f4 zooplankton excretion
    Mortality      <- mortalityRate * ZOO * ZOO #f5 zooplankton mortality
    Mineralisation <- mineralisationRate * DETRITUS #f6 detritus mineralization
    Chlorophyll    <- chlNratio * PHYTO #output variable
    TotalN         <- PHYTO + ZOO + DETRITUS + DIN  #total 
    
    dPHYTO    <- Nuptake - Grazing #change in net phytoplankton N flow
    dZOO      <- Grazing - Faeces - Excretion - Mortality #net change in zooplankton N flow
    dDETRITUS <- Mortality - Mineralisation + Faeces #net change in detritus N flow
    dDIN      <- Mineralisation + Excretion - Nuptake #net change in dissolved inorganic N flow
    
    
    list( 
      c(dPHYTO,dZOO,dDETRITUS,dDIN), 
      c(Chlorophyll = Chlorophyll, PAR=PAR, TotalN= TotalN)
    )  
  })
}  

## THE FLUXES ARE PER DAYS. SCALE PARAMETERS ACCORDINGLY.
#maxuptake sets the limitations to represent limited availability of resources/light
#units
parameters<-c(maxUptake          =1.0,       # day^-1
              ksPAR              =140,       # uEinstm^-2s^-1
              ksDIN              =0.5,       # mmolm^-3
              maxGrazing         =1.0,       # day^-1
              ksGrazing          =1.0,       # mmol N m^-3 (ksphyto)
              pFaeces            =0.3,       # no units
              excretionRate      =0.1,       # day^-1
              mortalityRate      =0.4,       # (mmolNm^-3)^-1(day^-1)
              mineralisationRate =0.1,       # day^-1
              chlNratio          =1)         # mgchl(mmolN)^-1

#setting values of state variables 
state     <-c(PHYTO   =1,   # mmol N m^-3 #units of N
              ZOO     =0.1, 
              DETRITUS=5.0, 
              DIN     =5.0) 


times     <-c(0,365)  #defining time as one year
out       <- as.data.frame( ode(state,times, NPZD, parameters) ) #ode -> desolve function for creating matrix #creating a data frame from that will include our four state variables, chlorophyll, PAR, and total N in one year
out #displaying the set output

num   <- length(out$PHYTO)   # last element #num=2

#grabbing variables from out in row 2 (1 year)
state <- c(PHYTO=out$PHYTO[num],         
           ZOO=out$ZOO[num],           
           DETRITUS=out$DETRITUS[num],
           DIN=out$DIN[num])

times     <-seq(0,730,by=1)   #set time span           
out <-as.data.frame(ode(state, times, NPZD, parameters)) #filling in vales over 2 years

#pivoting data to make it easier to graph
out.long <- out %>% 
  as.data.frame() %>% 
  pivot_longer(-time, names_to="State_vars", values_to="Value") 
# or pivot_longer(cols=PHYTO:TotalN, )
#plot the model
ggplot(data=out.long, aes(time, Value)) + #aes -> aesthetic mapping #time, value are the axis titles
  geom_line() + 
  facet_wrap(~State_vars, scales="free_y")

## provide a unique name for your PNG file
ggsave("myNPZDplot.png")
