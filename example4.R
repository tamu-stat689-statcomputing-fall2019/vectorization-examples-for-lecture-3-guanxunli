p <- 1000
n <- 50
set.seed(03875)
X <- matrix(rnorm(n * p, mean = 10, sd = 3), n, p)
group <- rep(1:2, each = n/2)

# function that returns t-test statistic for each gene
computeT_for <- function(X, group){
  Tstats <- rep(0,p)
  for (j in 1:p){
    Tstats[j] <- t.test(X[, j] ~ group)$stat
  }
  return(Tstats)
}

# evaluting the time
library(microbenchmark)
microbenchmark(
  computeT_for(X, group),
  times = 10
)

# Function that return t-test statistic for each gene
computeT_for2 <- function(X, group){
  Tstats <- rep(0,p)
  for (j in 1:p){
    Tstats[j] <- t.test(X[group==1, j], X[group==2, j])$stat
  }
  return(Tstats)
}
# Evaluate the timing
microbenchmark(
  computeT_for(X, group),
  computeT_for2(X, group),
  times = 10
)

## Different enter face changes a lot

computeT_for3 <- function(X, group){
  n1 <- sum(group == 1)
  n2 <- sum(group == 2)
  Tstats <- rep(0, p)
  for (j in 1:p){
    m1 <- mean(X[group==1, j])
    m2 <- mean(X[group==2, j])
    var1 <- var(X[group==1, j])
    var2 <- var(X[group==2, j])
    Tstats[j] <- (m1 - m2) / sqrt(var1/n1 + var2/n2)
  }
  return(Tstats)
}
# Evaluate the timing
microbenchmark(
  computeT_for(X, group),
  computeT_for2(X, group),
  computeT_for3(X, group),
  times = 10
)

computeT_for4 <- function(X, group){
  
  n1 <- sum(group == 1)
  n2 <- sum(group == 2)
  m1 <- colMeans(X[group==1, ])
  m2 <- colMeans(X[group==2, ])
  
  # apply function
  # var1 <- apply(X[group==1, ], 2, var)
  # var2 <- apply(X[group==2, ], 2, var)
  
  # cov function
  # var1 <- diag(cov(X[group==1, ]))
  # var2 <- diag(cov(X[group==2, ]))
  
  mean_mat1 <- matrix(m1, nrow = dim(X[group==1, ])[1], ncol = dim(X[group==1, ])[2], byrow = TRUE)
  mean_mat2 <- matrix(m2, nrow = dim(X[group==2, ])[1], ncol = dim(X[group==2, ])[2], byrow = TRUE)
  var1 <- 1/(n1 - 1) * colSums((X[group==1, ] - mean_mat1)^2)
  var2 <- 1/(n2 - 1) * colSums((X[group==2, ] - mean_mat2)^2)

  Tstats <- (m1 - m2) / sqrt(var1/n1 + var2/n2)
  return(Tstats)
}

T1 <- computeT_for(X, group)
T2 <- computeT_for2(X, group)
T3 <- computeT_for3(X, group)
T4 <- computeT_for4(X, group)

sum((T1 - T2)^2)
sum((T2 - T3)^2)
sum((T3 - T4)^2)

# Evaluate the timing
microbenchmark(
  computeT_for3(X, group),
  computeT_for4(X, group),
  times = 10
)









