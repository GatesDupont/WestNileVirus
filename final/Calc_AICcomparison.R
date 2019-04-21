# Gates Dupont #
# SP19         #
# # # # # # # # 

"Model Comparison Table for ZI-GAMs"

library(mgcv)
library(plyr)
library(dplyr)
library(qpcR)

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

#------------------------------------------------MODELS------------------------------------------------

# DOT
#----Base model - Dot model----
f.Psi_dot__N_dot = list(
  maxFlock ~ 
    dummy,
  ~
    dummy)

# TIME
#----Base model - 1 Time Dependence----
f.Psi_Year__N_dot = list(
  maxFlock ~ 
    dummy,
  ~
    s(yr, k=11))

#----Base model - 2 Time Dependence----
f.Psi_dot__N_Year = list(
  maxFlock ~ 
    s(yr, k=11),
  ~
    dummy)

#----Base model - 1 2 Time Dependence----
f.Psi_Year__N_Year = list(
  maxFlock ~ 
    s(yr, k=11),
  ~
    s(yr, k=11))

# LOCATION
#----Base model - 1 Coordinates----
f.Psi_Coord__N_dot = list(
  maxFlock ~ 
    dummy,
  ~
    s(lat,long))

#----Base model - 2 Coordinates----
f.Psi_dot__N_Coord = list(
  maxFlock ~ 
    s(lat,long),
  ~
    dummy)

#----Base model - 1 2 Coordinates----
f.Psi_Coord__N_Coord = list(
  maxFlock ~ 
    s(lat,long),
  ~
    s(lat,long))

# EFFORT DAYS
#----Base model - 1 Effort Days----
f.Psi_Days__N_dot = list(
  maxFlock ~ 
    dummy,
  ~
    effortDays)

#----Base model - 2 Effort Days----
f.Psi_dot__N_Days = list(
  maxFlock ~ 
    effortDays,
  ~
    dummy)

#----Base model - 1 2 Effort Days----
f.Psi_Days__N_Days = list(
  maxFlock ~ 
    effortDays,
  ~
    effortDays)

# EFFORT HOURS
#----Base model - 1 Effort Hours----
f.Psi_Hours__N_dot = list(
  maxFlock ~ 
    dummy,
  ~
    effortHours)

#----Base model - 2 Effort Hours----
f.Psi_dot__N_Hours = list(
  maxFlock ~ 
    effortHours,
  ~
    dummy)

#----Base model - 1 2 Effort Hours----
f.Psi_Hours__N_Hours = list(
  maxFlock ~ 
    effortHours,
  ~
    effortHours)



#----Aggregating model formulas----
model.formulas = list(
  f.Psi_dot__N_dot, 
  # TIME
  f.Psi_Year__N_dot,
  f.Psi_dot__N_Year,
  f.Psi_Year__N_Year,
  # LOCATION
  f.Psi_Coord__N_dot,
  f.Psi_dot__N_Coord,
  f.Psi_Coord__N_Coord,
  # EFF DAYS
  f.Psi_Days__N_dot,
  f.Psi_dot__N_Days,
  f.Psi_Days__N_Days,
  # EFF HOURS
  f.Psi_Hours__N_dot,
  f.Psi_dot__N_Hours,
  f.Psi_Hours__N_Hours)

model.names = c(
  "Psi_dot__N_dot",
  # TIME
  "Psi_Year__N_dot",
  "Psi_dot__N_Year",
  "Psi_Year__N_Year",
  # LOCATION
  "Psi_Coord__N_dot",
  "Psi_dot__N_Coord",
  "Psi_Coord__N_Coord",
  # EFF DAYS
  "Psi_Days__N_dot",
  "Psi_dot__N_Days",
  "Psi_Days__N_Days",
  # EFF HOURS
  "Psi_Hours__N_dot",
  "Psi_dot__N_Hours",
  "Psi_Hours__N_Hours")

model.fits = vector("list", 13)

#----Running models in parallel----
library(doParallel)
cores = detectCores()
cl = makeCluster(cores[1]-1)
registerDoParallel(cl)

model.fits = foreach(i=1:13, .packages=c("mgcv")) %dopar% {
  #for(i in 1){    
  assign(model.names[[i]], 
         gam(formula = model.formulas[[i]], family = ziplss, gamma=1.4, data=pfw))
}

#----Assigning Names----
for(i in 1:13){
  assign(model.names[[i]],
         model.fits[[i]])
}

#----AIC Model Comparison table----
models.aic = AIC(
  Psi_dot__N_dot, 
  # TIME
  Psi_Year__N_dot,
  Psi_dot__N_Year,
  Psi_Year__N_Year,
  # LOCATION
  Psi_Coord__N_dot,
  Psi_dot__N_Coord,
  Psi_Coord__N_Coord,
  # EFF DAYS
  Psi_Days__N_dot,
  Psi_dot__N_Days,
  Psi_Days__N_Days,
  # EFF HOURS
  Psi_Hours__N_dot,
  Psi_dot__N_Hours,
  Psi_Hours__N_Hours)
models.weights = akaike.weights(models.aic$AIC)
models.aictab = cbind(models.aic,models.weights)
models.aictab=models.aictab[order(models.aictab["deltaAIC"]),]
View(models.aictab)

#----AIC Model Comparison table----
models.aic = AIC(
  Psi_dot__N_dot, 
  # TIME
  Psi_Year__N_dot,
  Psi_dot__N_Year,
  Psi_Year__N_Year,
  # LOCATION
  Psi_Coord__N_dot,
  Psi_dot__N_Coord,
  Psi_Coord__N_Coord,
  # EFF DAYS
  Psi_Days__N_dot,
  Psi_dot__N_Days,
  Psi_Days__N_Days,
  # EFF HOURS
  Psi_Hours__N_dot,
  Psi_dot__N_Hours,
  Psi_Hours__N_Hours)
models.weights = akaike.weights(models.aic$AIC)
models.aictab = cbind(models.aic,models.weights)
models.aictab=models.aictab[order(models.aictab["deltaAIC"]),]
View(models.aictab)

#----AIC Model Comparison table----
models.aic = AIC(
  Psi_dot__N_dot, 
  # TIME
  Psi_Year__N_dot,
  Psi_dot__N_Year,
  Psi_Year__N_Year)
models.weights = akaike.weights(models.aic$AIC)
models.aictab = cbind(models.aic,models.weights)
models.aictab.t=models.aictab[order(models.aictab["deltaAIC"]),]
View(models.aictab.t)

#----AIC Model Comparison table----
models.aic = AIC(
  Psi_dot__N_dot, 
  # LOCATION
  Psi_Coord__N_dot,
  Psi_dot__N_Coord,
  Psi_Coord__N_Coord)
models.weights = akaike.weights(models.aic$AIC)
models.aictab = cbind(models.aic,models.weights)
models.aictab.xy=models.aictab[order(models.aictab["deltaAIC"]),]
View(models.aictab.xy)

#----AIC Model Comparison table----
models.aic = AIC(
  Psi_dot__N_dot, 
  # EFF DAYS
  Psi_Days__N_dot,
  Psi_dot__N_Days,
  Psi_Days__N_Days)
models.weights = akaike.weights(models.aic$AIC)
models.aictab = cbind(models.aic,models.weights)
models.aictab.days=models.aictab[order(models.aictab["deltaAIC"]),]
View(models.aictab.days)

#----AIC Model Comparison table----
models.aic = AIC(
  Psi_dot__N_dot, 
  # EFF HOURS
  Psi_Hours__N_dot,
  Psi_dot__N_Hours,
  Psi_Hours__N_Hours)
models.weights = akaike.weights(models.aic$AIC)
models.aictab = cbind(models.aic,models.weights)
models.aictab.hrs=models.aictab[order(models.aictab["deltaAIC"]),]
View(models.aictab.hrs)


View(rbind(models.aictab.days, models.aictab.hrs,
      models.aictab.t, models.aictab.xy))
