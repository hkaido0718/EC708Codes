# This code is based on the documentation of the PLM package, which can be found below.
# https://www.rdocumentation.org/packages/plm/versions/2.2-0/topics/pgmm

# Clear memory
rm(list = ls())

# Packages
library(plm)
data("EmplUK", package = "plm")

## Arellano and Bond (1991), table 4 col. a1 
a1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + lag(log(capital),0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "onestep")
summary(a1, robust = FALSE)

## Arellano and Bond (1991), table 4 col. a2
a2 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + lag(log(capital),0:2) + lag(log(output), 0:2) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps")
summary(a2, robust = FALSE)

## Arellano and Bond (1991), table 4 col. b1
b1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps")
summary(b1, robust = FALSE)





