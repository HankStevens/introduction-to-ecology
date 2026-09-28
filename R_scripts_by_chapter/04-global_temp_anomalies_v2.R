# Physical Environment: A Fate of Earth ----
# Source: 04-global_temp_anomalies_v2.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Background ----

## Activity ----

### Set up ----

#### code-chunk-1 ----
#| eval: false
## install tidyverse
## tidyverse is a"universe" or collection of related R packages
## that have a particular coding style that helps a lot.
install.packages("tidyverse", repos = "https://cloud.r-project.org")

#### code-chunk-2 ----
## load R packages
# this makes these packages available to R in this session.
library(tidyverse)

### Import data ----

#### code-chunk-3 ----
# read in data
temps <- read.csv(file = "data/global_surface_anomalies_2025.csv", # full file name
                  skip=3, # skip these lines because they contain metadata.
                  header = TRUE # Tell R that the first row is column namaes
                  ) # last parenthesis

### Finding out information about the temperature anomalies ----

#### code-chunk-4 ----
summary(temps)

### Minimum and maximum ----

#### code-chunk-5 ----
## get the min and max
min.anomaly <- min(temps$deg_C)
max.anomaly <- max(temps$deg_C)

#### code-chunk-6 ----
## use the min and the max
# note the two equal signs, a pipe, and then two more equals signs.
subset(temps, deg_C==min.anomaly | deg_C==max.anomaly )

### Using a year ----

#### code-chunk-7 ----
## Find the anomaly in the year you were born
subset(temps, year==1960) # use your year with two equal signs

### Plotting information ----

#### code-chunk-8 ----
## we will create a plot object and stick it in "p"
p <- ggplot(data=temps, # the data frame to use
       ## aes() stand for "aesthetics"
       aes(x=year, y=deg_C), # define x and y variables to plot
       ) + # closing parenthesis and add a plus sign to add another graphical element
  geom_line() # the type of graph to plot using x and y

#### code-chunk-9 ----
p

#### code-chunk-10 ----
## add a trend line
p + geom_smooth(linetype="dashed")

#### code-chunk-11 ----
# first find the anomaly in 2002
anomaly.2002 <- temps$deg_C[temps$year==2002]
anomaly.2002
p + geom_smooth() +
  labs(
    y="Surface temp. anomaly (deg C)",
    subtitle="Relative to average 1951-1980"
    ) +
  geom_hline(yintercept = anomaly.2002, linetype=3)

## Predicting future anomalies ----

### code-chunk-12 ----
## Load a necessary R package
library(mgcv) # stands for "Mixed Generalized Additive Model Computation Vehicle"

### code-chunk-13 ----
## Create the model
mod <- gamm(deg_C ~ s(year),  # "deg_C as a function of year
            data=temps, # data frame
          correlation = corCAR1(form=~year), # specify autocorrelation among samples
          method="REML" # fit using restricted maximum likelihood
          )

### code-chunk-14 ----
## years for fit and prediction
years = data.frame(year=1880:2072)

### code-chunk-15 ----
## include uncertainty with se.fit = TRUE
preds <- data.frame(
  predict(mod$gam, newdata=years, type="response", se.fit=TRUE)
  )

# combine columns of data frames
my.predictions <- cbind(years, preds)

### code-chunk-16 ----
# first three lines , and all columns
my.predictions[1:3, ]

### code-chunk-17 ----
### Create 95% confidence intervals
crit.t.low <- qt(0.025, df = df.residual(mod$gam))
crit.t.high <- qt(0.975, df = df.residual(mod$gam))

### this requires that you loaded the tidyverse package
my.predictions <- my.predictions %>% # my data
  ## transform the data set to include two new columns
  transform(
    upper.conf.lim=fit + (crit.t.high * se.fit),
    lower.conf.lim=fit + (crit.t.low * se.fit))
###
## view the first three rows
my.predictions[1:3, ]

### code-chunk-18 ----
## basic data and x, y values
g <- ggplot(data = my.predictions, aes(x = year, y = fit)) +
  ## adding a wide region or band (a ribbon)
  geom_ribbon(aes(ymin = lower.conf.lim, ymax = upper.conf.lim,
                  x = year),
              alpha = 0.2, fill = "black") +
  ## add the expected local trend
  geom_line() +
  ## adding the points for each year
  geom_point(data = temps, mapping = aes(x = year, y = deg_C),
               inherit.aes = FALSE) +
  ## add a better label for the y axis
  labs(y = "Global temp. anomaly")
  ## simplify the image

### code-chunk-19 ----
## basic data and x, y values
g2 <- ggplot(data = my.predictions, aes(x = year, y = fit)) +
  ## adding a wide region or band (a ribbon)
  geom_ribbon(aes(ymin = lower.conf.lim, ymax = upper.conf.lim,
                  x = year),
              alpha = 0.2, fill = "black") +
  ## add the expected local trend
  geom_line() +
  ## adding the points for each year
  geom_point(data = temps, mapping = aes(x = year, y = deg_C),
               inherit.aes = FALSE) +
  ## add a better label for the y axis
  labs(y = "Global temperature anomaly")  +
  ## make this different from what the students SHOULD turn in.
  theme_bw()

### code-chunk-20 ----
g

### code-chunk-21 ----
g2

### code-chunk-22 ----
## Save a PNG file into your figs folder
## THIS IS THE GRAPH TO INSERT INTO YOUR ASSIGNMENT
ggsave("figs/Global_temps.png", # name for your figure
       plot=g, ## identify which graph you want to save,
       height=5, width=5)

## Does Earth have a fever? ----

### code-chunk-23 ----
## First we take a subset of our data set.
a <- subset(temps, year==1950)
b <- subset(temps, year==2022)
increase <- b$deg_C - a$deg_C
increase

### code-chunk-24 ----
## percent increase for Earth
perc.Earth <- increase / 13.9 * 100

## percent increase for humans
perc.fever <- 2/37 * 100

perc.Earth; perc.fever

## Questions to answer ----

## Deliverables ----
