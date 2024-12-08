a <- c(99, 100) == 100
if (a) print('kaixo')

x <- matrix(c(1,2,3,NA,5,6,7,8,9), nrow = 3)
x
measure <- x[upper.tri(x)]
measure
measure[is.na(measure)] <- 0L
measure
measure
library(cluster)
library(sp)

as.matrix <- function(x, ...){
  d <- sqrt(length(x) * 2 +  length(attr(x, 'Labels')))
  y <- matrix(attr(x, 'diagv'), d, d)
  y[upper.tri(y)] <- x
  y[lower.tri(y)] <- t(y)[lower.tri(y)]
  colnames(y) <- rownames(y) <- attr(x, 'Labels')
  return (y)
}

as.matrix(measure)

clusters <- fanny(as.matrix(measure), 2, diss = TRUE, memb.exp = 1.5)
