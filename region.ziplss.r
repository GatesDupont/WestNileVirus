# Gates Dupont #
# Fall 2018    #
# # # # # # # #

library(mgcv)

pfw = read.csv('~/Desktop/WNV2/PFW_amecro_zerofill_landcover.csv')

#----Subsetting for states----
southeast = c("NC", "SC", "GA", "FL", "AL", "MS", "TN")
pfw = pfw[pfw$state %in% southeast,]

#----Two-step Zero-Infalted Poisson Generalized Additive Model----
date()
if(T){
  se.gam = gam(list(
    maxFlock ~ 
      effortHours +
      effortDays +
      s(yr, k=11) +
      s(locID, bs="re") +
      s(lat,long),
    ~
      effortHours +
      effortDays +
      s(yr, k=11) +
      s(locID, bs="re") +
      s(lat,long)),
    family = ziplss,
    gamma = 1.4,
    data = pfwct) # k=6
}
date()
