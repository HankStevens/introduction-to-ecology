## My first R script in BIO 209W Spring 2023
## Hank

# See all available info more clearly
ap <- available.packages()
ap[ap[, "Package"] == "ggplot2", ]

# Check what binary type R is looking for
getOption("pkgType")

# See all columns nicely formatted
library(tibble)
as_tibble(ap[ap[, "Package"] == "ggplot2", ])

# Check multiple packages at once
ap[ap[, "Package"] %in% c("ggplot2", "dplyr", "tidyr"), c("Package", "Version", "Repository")]

a <- -1:5
b <- runif(7, min=0, max=1)
a*b
