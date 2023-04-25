# This program runs a Monte Carlo simulation to see failures of bootstrap

library(boot)
library(doParallel)
library(doRNG)

n <- 1000
mu <- 0 #1/sqrt(n)
x <- rnorm(n)+mu

# Estimator of Abs(mu)
abs.mean <- function(x,i)
{ d <- x[i]
  abs(mean(d))
}

# Bootstrap 
boot.abs <- boot(x,abs.mean,R=9999)
boot.root <- sqrt(n)*(boot.abs$t-boot.abs$t0)
hist(boot.root,br='FD',prob=T,ylim=c(0,1.1*2*dnorm(0)))

# True density
curve(dnorm(x)+dnorm(x+2*abs(mu)*sqrt(n)),-abs(mu)*sqrt(n),n=101,add=T,col='blue',lwd=1)
curve(0*x,-10,-abs(mu)*sqrt(n),add=T,col='blue',lwd=1)

# Subsampling
theta.hat <- abs(mean(x))
subsampling <- function(b) {
  xb <- sample(x,b,replace=FALSE)
  theta.b <- abs(mean(xb))
  root <- sqrt(b)*(theta.b-theta.hat)
  return(root)
}

b <- 50
nreps <- 9999
set.seed(123) 
nCores <- as.numeric(4)
registerDoParallel(cores=nCores)
results.sub.par <- foreach(i=1:nreps) %dorng% {
  results.sub.par <- subsampling(b)
}
sub.root <- t(matrix(unlist(results.sub.par),1,nreps))
hist(sub.root,br='FD',prob=T,ylim=c(0,1.1*2*dnorm(0)))
curve(dnorm(x)+dnorm(x+2*abs(mu)*sqrt(n)),-abs(mu)*sqrt(n),n=101,add=T,col='blue',lwd=1)
curve(0*x,-10,-abs(mu)*sqrt(n),add=T,col='blue',lwd=1)