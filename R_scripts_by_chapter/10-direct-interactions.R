# Direct species interactions: understanding mechanisms of invasion in mosquitoes ----
# Source: 10-direct-interactions.qmd
# All R code chunks extracted in order of appearance.

## Goals ----

## Preparatory steps ----

### Optional background ----

## Background ----

### tbl-mosquito ----
#| label: tbl-mosquito
#| tbl-cap: "Lotka-Volterra growth and competition parameters of *A. triseriatus* and *A. albopictus* in water from tree holes and from tires, based on Livdahl and willey (1991). Values of $r$ and $K$ refer to the species on the respective line. Values for alpha or beta refer to the effect of the competitor. For instance, K = 42.83 is the carrying capacity of A. triseriatus in tree hole water, and alpha = 0.4167 is the effect of A. albopictus on A. triseriatus, in tree hole water."
#| echo: false
get.comp <- function(ri, bi, bj){
  r=ri
  Ki <- -ri/bi
  a.B <- -bj*Ki/ri
  c(r = r, Ki=Ki, a.B = a.B)
}
#Tree hole:
tree.hole.At <- get.comp(0.0514, -0.0012, -0.0005)
tree.hole.Aa <- get.comp(0.0798, -0.0015, -0.0011)
tire.At <- get.comp(0.0591, -0.0018, -0.0015)
tire.Aa <- get.comp(0.0904, -0.0020, -0.0005)
d <- as.data.frame( 
  rbind(tree.hole.At, tree.hole.Aa, tire.At, tire.Aa) )
row.names(d) <- c("A. triseriatus (in treehole water)",
                  "A. albopictus (in treehole water)",
                  "A. triseriatus (in tire water)",
                  "A. albopictus (in tire water)")
colnames(d) <- c("r", "K", "alpha or beta")


knitr::kable(d,
  digits=c(4,4,4),
  align='c',
  booktabs=TRUE)


## Invasion criteria ----

### Deriving the invasion criterion ----

## Follow the Logic: What the math just told you ----

### Apply the criteria ----

#### code-chunk-2 ----
#| echo: false
#| results: 'hide'

## in tree hole water...
# Can Aa invade?
tree.hole.Aa["Ki"] > tree.hole.Aa["a.B"]*tree.hole.At["Ki"]
# can At invade?
tree.hole.At["Ki"] > tree.hole.At["a.B"]*tree.hole.Aa["Ki"]

## in tire water
# Can Aa invade?
tire.Aa["Ki"] > tire.Aa["a.B"]*tire.At["Ki"]
# Can At invade?
tire.At["Ki"] > tire.At["a.B"]*tire.Aa["Ki"]


## Deliverables ----

### code-chunk-3 ----
#| eval: false
#| echo: false
library(tidyverse)
out <- with(d,
     {
       A.triseriatus.treehole <- c(K[1], K[1]/`alpha or beta`[1])
        A.albopictus.treehole <- c(K[2], K[2]/`alpha or beta`[2])
         A.triseriatus.tire <- c(K[3], K[3]/`alpha or beta`[3])
          A.albopictus.tire <- c(K[4], K[4]/`alpha or beta`[4])
          out <- rbind(A.triseriatus.treehole, 
                       A.albopictus.treehole, 
                       A.triseriatus.tire, 
                       A.albopictus.tire)
          colnames(out) <- c("Ki", "Ki/coef")
          as.data.frame(out)
     })
out
out2 <- data.frame(
  Water_type = c("Treehole", "Treehole", "Treehole", "Treehole",
                 "Tire", "Tire", "Tire", "Tire"),
  x=c(0, out[1,2], out[2,1], 0, 
      0, out[3,2], out[4,1], 0),
  
  y=c(out[1,1], 0, 0, out[2,2],
      out[3,1], 0, 0, out[4,2])
)
out2


 
{plot(out2[1:2,], type="l", 
     xlim=c(0, max(out2[,1])), ylim=c(0, max(out2[,2]))); 
  lines(out2[3:4,1], out2[3:4,2], lty=2)
title("Treehole Water")
}
  
{plot(out2[5:6,], type="l", 
     xlim=c(0, max(out2[,1])), ylim=c(0, max(out2[,2]))); lines(out2[7:8,1],out2[7:8,2], lty=2)
title("Tire Water")
}
