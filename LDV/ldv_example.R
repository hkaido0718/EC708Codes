# This code is based on the documentation of the mlogit package developed by Yves Croissant

# Clear memory
rm(list = ls())

# Packages
library(mlogit)

data("Fishing", package = "mlogit")
Fish <- dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")

# Choices are beach, pier, (private) boat, charter (boat)
# Intercept and coefficient on income is normalized to 0 for "beach"
# Covariates are price (alternative specific), catch rate (alternative specific), and income (indiv. specific)

## a pure "conditional logit" model
summary(mlogit(mode ~ price + catch, data = Fish))

## a pure "multinomial logit model"
summary(mlogit(mode ~ 0 | income, data = Fish))

## a general Logit model
m <- mlogit(mode ~ price + catch | income, data = Fish)
summary(m)


## computing PEA
xbar <- with(Fish, data.frame(price = tapply(price, idx(m, 2), mean),
                           catch = tapply(catch, idx(m, 2), mean),
                           income = mean(income)))
effects(m, covariate = "income", data = xbar)
effects(m, covariate = "price", type = "rr", data = xbar) # elasticity
effects(m, covariate = "catch", type = "ar", data = xbar) # absolute change of prob to 1% change in catch

## a mixed logit model
rpl <- mlogit(mode ~ price + catch | income, Fishing, varying = 2:9,
              rpar = c(price= 'n', catch = 'n'), correlation = TRUE,
              halton = NA, R = 50)
summary(rpl)
cov.mlogit(rpl)

## a probit model
m <- mlogit(mode ~ price + catch, data = Fish, probit = TRUE)
summary(m)


