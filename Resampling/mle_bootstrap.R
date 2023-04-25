# Clear memory
rm(list = ls())

# Packages
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

simgen <- function(lambda){
  X <- generate_x(lambda)
  lambda_mle <-  1/mean(X) # MLE
  Z <- sqrt(T)*(lambda_mle-lambda)
}


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
lambda_mle = 1/mean(X)
se = sqrt(lambda_mle^2) # sqrt(T)*standard error of MLE 

# Show the asymptotic normal approximation
xfit <- seq(-10,10,length=50)
yfit <- dnorm(xfit,mean=0,sd=se)
lines(xfit,yfit,col="black")

# Parametric bootstrap
mle.fun <- function(data,i){ 
  X <- data[i]
  lambda_star_mle <- 1/mean(X)
  c(sqrt(T)*(lambda_star_mle-lambda_mle)) }
mle.rg <- function(data,mle){
  X <- rexp(T,mle)
}
res.boot<-boot(X,mle.fun,R=5000,sim="parametric",ran.gen=mle.rg,mle=1/mean(X))
hist(res.boot$t,30,prob=TRUE,xlim=c(-10,10),ylim=c(0,0.25))
