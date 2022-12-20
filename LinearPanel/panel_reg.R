
library(AER)
library(plm)
rm(list = ls())

data("Produc", package = "plm")

# Fixed effects
res_fe <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, index = c("state","year"), model="within")
summary(res_fe)

# Random effects
res_re <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
           data = Produc, index = c("state","year"), model="random")
summary(res_re)

