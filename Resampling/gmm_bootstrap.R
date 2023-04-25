# Clear memory
rm(list = ls())

# Packages
library(gmm)
library(car)
library(boot)
library(doParallel)
library(doRNG)

# Parameters
T <- 30
S <- 5000
lambda <- 2

# Functions
generate_x <- function(lambda) rexp(T,rate=lambda)

g <- function(lambda, x)
{
  m1 <- x - 1/lambda
  m2 <- x^2 - 2/lambda^2
  f <- cbind(m1,m2)
  return(f)
}

Dg <- function(lambda,x)
{
  jacobian <- matrix(c(1/lambda^2,4/lambda^3),nrow=2,ncol=1)
  return(jacobian)
}


simgen <- function(lambda){
  X <- generate_x(lambda)
  res <- gmm(g, X, 1,grad = Dg , wmatrix="ident",vcov="iid",method="Brent",lower=0,upper=10) # GMM
  lambda_gmm <- res$coefficients[1]
  Z <- sqrt(T)*(lambda_gmm-lambda)
}

#  SimulatingFinite sample distribution of GMM estimator
set.seed(123) # set seed for replications (this generates the same sequence of random numbers)
nCores <- 8   # number of CPUs for parallelization
registerDoParallel(cores = nCores)
results.par.sy <- foreach(i=1:S) %dorng% {
  results.par.sy <- simgen(lambda)
}
sim_results <- t(matrix(unlist(results.par.sy),1,S)) # convert results to a matrix
hist(sim_results,30,prob=TRUE,xlim=c(-10,10),ylim=c(0,0.25))

# Obtain estimate of lambda
set.seed(123)
X <- generate_x(lambda)
res <- gmm(g, X, 1,grad = Dg , wmatrix="ident",vcov="iid",method="Brent",lower=0,upper=10) # GMM
lambda_gmm <- res$coefficients[1]
se = sqrt(res$vcov)*sqrt(T)

# Show the asymptotic normal approximation
xfit <- seq(-10,10,length=50)
yfit <- dnorm(xfit,mean=0,sd=se)
lines(xfit,yfit,col="black")

# Nonparametric Bootstrap for GMM
gmm.fun <- function(data,i){ 
  X <- data[i]
  res <- gmm(g, X, 1,grad = Dg , wmatrix="ident",vcov="iid",method="Brent",lower=0,upper=10)
  c(sqrt(T)*(res$coefficients[1]-lambda_gmm)) }
res.boot<-boot(X,gmm.fun,R=5000)
Zstar_gmm <- res.boot$t
hist(Zstar_gmm,30,prob=TRUE,xlim=c(-10,10),ylim=c(0,0.25))
