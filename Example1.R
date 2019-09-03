# Squaring elements of a given vector

square_for <- function(x){
  # [ToDo] Use the for loop
  n <- length(x)
  sq_x <- rep(0, n)
  for (i in 1:n){
    sq_x[i] <- x[i]^2
  }
  return(sq_x)
}

square_sapply <- function(x){
  # [ToDo] Use the sapply function
  sapply(x, function(y) y^2)
}

square_vec <- function(x){
  # [ToDo] Use power(^) function in vector form
  x^2
}

square_vec2 <- function(x){
  # [ToDo] Use multiplication(*) function in vector form
  x * x
}

square_vec3 <- function(x){
  x ** 2
}

# [ToDo] Create a vector x of size 100,000 of normal variables
set.seed(1234)
x <- rnorm(100000)

# [ToDo] Verify that all functions return the same output
y1 <- square_for(x)
y2 <- square_sapply(x)
y3 <- square_vec(x)
y4 <- square_vec2(x)
y5 <- square_vec3(x)

sum(abs(y1 - y2))
sum(abs(y2 - y3))
sum(abs(y3 - y4))
sum(abs(y4 - y5))
sum(abs(y5 - y1))

# [ToDo] Use microbenchmark package to compare three functions in terms of speed
library(microbenchmark)

microbenchmark(
  square_for(x),
  square_sapply(x),
  square_vec(x),
  square_vec2(x),
  square_vec3(x)
)

