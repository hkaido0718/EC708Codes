rm(list = ls())
library(AER)

set.seed(123)
S <- 1000
ivsim <- rep(0,S)

# Set parameters
theta <- 0.1
beta0 <- 0.5
beta1 <- 1 

# Simulate
for(s in 1:S){
  n <- 100
  w <- rnorm(n)
  u <- rnorm(n)
  v <- rnorm(n)
  x <- theta * w + v
  y <- beta0 + beta1*x + u
  ivres <- ivreg(y~x|w)
  ivsim[s] <- ivres$coefficients["x"]
}

# Plot
hist(ivsim)