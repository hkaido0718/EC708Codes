# This code is based on the documentation of the GMM package, which can be found below.
# https://cran.r-project.org/web/packages/gmm/vignettes/gmm_with_R.pdf
# The model is of the form E[r-alpha-zm*beta|h]=0, where r, alpha, zm are K-by-1 vectors.

# Clear memory
rm(list = ls())

# Packages
library(gmm)
library(car)

## CAPM test with GMM
data(Finance)
K <- 4
r <- Finance[1:300, 1:K]
rm <- Finance[1:300, "rm"]
rf <- Finance[1:300, "rf"]
z <- as.matrix(r-rf)
t <- nrow(z)
zm <- rm-rf
h <- matrix(zm, t, 1)
h2 <- matrix(c(zm,zm^2),t,2)

## GMM with W=I, IV=zm (just identified) ##
res <- gmm(z ~ zm, x = h,vcov="HAC",wmatrix="ident")
summary(res)

## 2-step GMM, IV=zm (just identified) ##
res2 <- gmm(z ~ zm, x = h,vcov="HAC")
summary(res2)

## GMM with W=I, IV=zm and zm^2 (overidentified) ##
res3 <- gmm(z ~ zm, x = h2,vcov="HAC",wmatrix="ident")
summary(res3)

## 2-step GMM, IV=zm and zm^2 (overidentified) ##
res4 <- gmm(z ~ zm, x = h2,vcov="HAC")
summary(res4)

## Hypothesis tests ##
linearHypothesis(res4,cbind(diag(K),matrix(0,K,K)),rep(0,K))