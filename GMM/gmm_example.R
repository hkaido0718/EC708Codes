# This code is based on the documentation of the GMM package, which can be found below.
# https://cran.r-project.org/web/packages/gmm/vignettes/gmm_with_R.pdf

# Clear memory
rm(list = ls())

# Packages
library(gmm)
library(lhs)

# Generate normal random variables
n <- 250
set.seed(123) # set seed for replicability
x <- rnorm(n, mean = 4, sd = 2)

# Implementing the 3 moment conditions
g <- function(tet, x)
{
  m1 <- (tet[1] - x)
  m2 <- (tet[2]^2 - (x - tet[1])^2)
  m3 <- x^3 - tet[1]*(tet[1]^2 + 3*tet[2]^2)
  f <- cbind(m1, m2, m3)
  return(f)
}

# Implementing the Jacobian
Dg <- function(tet, x)
{
  jacobian <- matrix(c( 1, 2*(-tet[1]+mean(x)), -3*tet[1]^2-3*tet[2]^2,0, 2*tet[2],
                        -6*tet[1]*tet[2]), nrow=3,ncol=2)
  return(jacobian)
}

# Estimate the two parameters using the GMM.
res <- gmm(g, x, c(0, 0), grad = Dg)
summary(res)

# Check if the result changes with another initial value
gmm(g, x, c(-2, 4), grad = Dg)

# Try different initial values
L <- 10 # number of initial values
ThetaGrid <- 10*randomLHS(L,2)-matrix(rep(c(5,0),each=L),nrow=L) # Grid on [-5,5]x[0,10]
Init <- lapply(seq_len(L), function(i) ThetaGrid[i,])
est  <- function(init) gmm(g, x, init, grad = Dg)
lapply(Init,est)
