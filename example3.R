
# dg - integer, maximal power
powers1 <- function(x, dg){
 pw <- matrix(x, nrow = length(x)) 
 prod <- x # current product
 for (i in 2:dg){
   prod <- prod * x
   pw <- cbind(pw, prod) 
   }
 return(pw) 
}

library(microbenchmark) 
x = runif(1000000) 
microbenchmark(
 powers1(x, 8),
 times = 10)

Rprof() # start monitoring
invisible(powers1(x, 8)) # supress function output 
Rprof(NULL) # stop monitoring
summaryRprof() # see the report

library(profvis)
profvis(
  {
    dg <- 8
    pw <- matrix(x, nrow = length(x)) 
    prod <- x # current product
    for (i in 2:dg){
      prod <- prod * x
      pw <- cbind(pw, prod) 
      }
  } 
  )