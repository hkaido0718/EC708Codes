rm(list = ls())
library(AER)

setwd("/Users/hiroakikaido/Dropbox (Personal)/Teaching/2021/EC708/Stata/IV")
card_data <- read.csv("card.csv",header=T)

iv1_res <- ivreg(lwage ~ educ | nearc4, data=card_data)
summary(iv1_res, vcov = sandwich)

iv2_res <- ivreg(lwage ~ educ | nearc4 + nearc2, data=card_data)
summary(iv2_res, vcov = sandwich)