# Gates Dupont #
# F18          #
# # # # # # # #

library(mgcv)

pfw = read.csv("~/WNV F18/PFW_amecro_zerofill_landcover.csv")

lowerNortheast = c("NY", "NJ", "CT", "RI")
pfw = pfw[pfw$state %in% lowerNortheast,]
pfw = droplevels(pfw)

date()
if(T){
  l_ne.gam = gam(list(
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
    data = pfw) # k=6
}
date()
