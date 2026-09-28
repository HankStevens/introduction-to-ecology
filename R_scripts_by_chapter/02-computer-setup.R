# Computer setup and first R script ----
# Source: 02-computer-setup.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Background ----

### Understanding you computer ----

### Folders, directories, and directory structure ----

### The location of a folder or file: The Path ----

#### File paths in Windows 11 operating system ----

#### File paths on Mac Tahoe operating system ----

### File types and file name extensions ----

#### Three important file types ----

#### Windows ----

#### Macs ----

## Working with CSV files on Windows and Apple computers ----

## Installing R ----

### What and Why ----

### How-to videos (optional) ----

### How-to instructions ----

### Where ----

### Installing RStudio ----

### R $\neq$ RStudio ----

## Set up your folders for this class ----

### Why we all use the same structure ----

### Where do scripts go? ----

## Working in R ----

### Set your "working directory" ----

#### code-chunk-1 ----
getwd()

### Create a *project* ----

### Scripts: Start and save a script ----

#### code-chunk-2 ----
## [title] e.g., My first R script in BIO 209W
## [Date]
## [your name]
## [the full path to your Rwork directory]

### Entering and running code ----

#### code-chunk-3 ----
-1:5

#### code-chunk-4 ----
-1:5

### Assignment operator ----

#### code-chunk-5 ----
a <- -1:5

### Examining objects ----

#### code-chunk-6 ----
# show or 'print' a
a

#### code-chunk-7 ----
str(a)

### Things you'll find out about R ----

#### code-chunk-8 ----
f <- 9
f

#### code-chunk-9 ----
2 < -3

#### code-chunk-10 ----
b <- runif(7, min=0, max=1)
b

#### code-chunk-11 ----
# multiply a and b
ab <- a * b
## Show a, b, and ab
a
b
ab

### Combining vectors into a data frame ----

#### code-chunk-12 ----
d <- data.frame(a=a, b=b, a_times_b = ab)
d

#### code-chunk-13 ----
str(d)

#### code-chunk-14 ----
# export or write a file
write.csv(x=d, file="output/myDataframe.csv", row.names=FALSE)
# including row.names=FALSE prevents R from adding row names.

### Plotting data ----

#### code-chunk-15 ----
# type='p' is for points only
plot(a, ab, type='p') 

## Install an R package ----

### code-chunk-16 ----
install.packages("ggplot2")

## Load and use an R package ----

### code-chunk-17 ----
library(ggplot2)

### code-chunk-18 ----
## plot data and fit a curve
ggplot(data=d, aes(x=a, y=a_times_b)) + 
  geom_line() +
  labs(x="a", y="Product of a and b")

### Saving your graphs ----

#### code-chunk-19 ----
ggsave("figs/myPlot.png")
ggsave("figs/myPlot.jpg", width=7, height = 3)
ggsave("figs/myPlot.pdf", width=3, height = 5)

## Getting help ----

## Deliverables ----
