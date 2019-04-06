# Gates Dupont #
# SP19         #
# # # # # # # # 

"Comparing GAMs used in eBird Best Practices: nb, ziplss, tw"

library(mgcv)
library(plyr)
library(dplyr)

#----Load clean data----
pfw = read.csv("~/WNV SP19/PFW_amecro_clean.csv")
pfw = pfw[,-1]

#----Number of base year records----
base = as.numeric(table(pfw$yr)[1])
base = 2000

#----Splitting by year----
pfw.split = pfw %>%
  split(pfw$yr)

#----1000 samples----
sets = vector("list", 1000)
for(i in 1:1000){
  # Set iterative seed for reproducibility
  set.seed(i) 
  # Randomly selecting balanced samples
  equal.years = vector("list", 23)
  for(j in 1:23){
    equal.years[[j]] = sample_n(pfw.split[[j]], size=base, replace=F)
  }
  # Append to sets list
  sets[[i]] = bind_rows(equal.years)
}

#----Dummy Variable----
pfw = sets[[1]]
pfw$dummy = 1


#----Negative-binomial----
nb.model = gam(maxFlock ~ effortHours + effortDays + s(yr, k=11) + s(lat,long), 
               family = "nb",
               gamma = 1.4, 
               data = pfw)

#----ziplss----
ziplss.model = gam(list(
  maxFlock ~ 
    effortHours +
    effortDays +
    s(yr, k=11) +
    s(lat,long),
  ~
    effortHours +
    effortDays +
    s(yr, k=11) +
    s(lat,long)),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----tweedie----
tw.model = gam(maxFlock ~ effortHours + effortDays + s(yr, k=11) + s(lat,long), 
               family = "tw",
               gamma = 1.4, 
               data = pfw)

#----Summary of models----
models = list(nb.model, ziplss.model, tw.model)
for(i in models){
  paste(i$family)
  print(summary(i))
}
