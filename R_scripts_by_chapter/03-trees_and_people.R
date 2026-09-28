# Environmental Justice: How income relates to environment. ----
# Source: 03-trees_and_people.qmd
# All R code chunks extracted in order of appearance.

## setup ----
options(gargle_oauth_email = "stevenmh@miamioh.edu", message=FALSE)
library(patchwork)

## Goals ----

## Background ----

## Methods ----

### Instructions ----

### Graphing data to assess our predictions ----

#### code-chunk-2 ----
# if needed, install these packages
# install.packages("googlesheets4")
# install.packages("ggplot2")

# load these packages
library(googlesheets4)
library(ggplot2)

# Import data. 
# This code will require that you have permission to access our G-Sheet.
# You may be asked to select various options and/or sign in to your
# Miami Google account.

# The function read_sheet() requires a Google Sheet URL in quotes, and
# allows you to skip lines that are not data, 
# such as a table description.

d <- read_sheet("https://docs.google.com/spreadsheets/d/1uYCuTziJiLNvVOQ9Q65BxdWcNgDF6EnF8L1T3O_yATw/edit#gid=0",
                skip=2)

# Uncomment to show us the names of the variables
# names(d)

# make a scatterplot
plot1 <- ggplot(data=d, aes(x=Median_Income, 
                   y=Tree_canopy_cover, 
                   label=`City (only)`)) + geom_text() +
  labs(y="Tree cover (%)", x="Median income", title="No relation twixt tree cover and income?")

ggsave("figs/myCoverIncome_text.png", plot=plot1, width=10, height = 10)

plot2 <- ggplot(data=d, aes(x=Median_Income, 
                   y=Tree_canopy_cover, 
                   label=`City (only)`)) + 
  geom_point() +
  labs(y="Tree cover (%)", x="Median income", title="No relation twixt tree cover and income?")

# save the figure (dimensions in inches)
ggsave("figs/myCoverIncome_points.png", plot=plot2, width=7, height = 7)

#### fig-data ----
#| label: fig-data
d10 <- d[1:10,]
myPlot <- ggplot(data=d10, aes(x=Median_Income, 
                   y=Tree_canopy_cover, 
                   label=`City (only)`)) + geom_text() +
  labs(y="Tree cover (%)", x="Median income", title="No relation twixt tree cover and income?")

myPlot

## Deliverables ----
