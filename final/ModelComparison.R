# Gates Dupont #
# SP19         #
# # # # # # # # 

"Model Comparison Table for ZI-GAMs"


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

#------------------------------------------------MODELS------------------------------------------------

#----Running models in parallel----
library(doParallel)
cores = detectCores()
cl = makeCluster(cores[1]-1)
registerDoParallel(cl)

#----Let's go----
models = vector("list", 1000)
fits = vector("list", 1000)
traj.data = vector("list", 1000)

#...FOREACH...

#----Base model - Dot model----
Psi_dot__N_dot = gam(list(
  maxFlock ~ 
    dummy,
  ~
    dummy),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - 1 Time Dependence----
Psi_Year__N_dot = gam(list(
  maxFlock ~ 
    s(yr, k=11),
  ~
    dummy),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - 2 Time Dependence----
Psi_dot__N_Year = gam(list(
  maxFlock ~ 
    dummy,
  ~
    s(yr, k=11)),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - 1 2 Time Dependence----
Psi_Year__N_Year = gam(list(
  maxFlock ~ 
    s(yr, k=11),
  ~
    s(yr, k=11)),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - 1 Coordinates----
Psi_Coord__N_dot = gam(list(
  maxFlock ~ 
    s(lat,long),
  ~
    dummy),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - 2 Coordinates----
Psi_dot__N_Coord = gam(list(
  maxFlock ~ 
    dummy,
  ~
    s(lat,long)),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - 1 2 Coordinates----
Psi_Coord__N_Coord = gam(list(
  maxFlock ~ 
    s(lat,long),
  ~
    s(lat,long)),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - Effort Days----
Psi_Days__N_Days = gam(list(
  maxFlock ~ 
    effortDays,
  ~
    effortDays),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - Effort Days----
Psi_Days__N_Days = gam(list(
  maxFlock ~ 
    effortDays,
  ~
    effortDays),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - Effort Days----
Psi_Days__N_Days = gam(list(
  maxFlock ~ 
    effortDays,
  ~
    effortDays),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - Effort Hours----
Psi_Hours__N_Hours = gam(list(
  maxFlock ~ 
    effortHours,
  ~
    effortHours),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - Effort in both----
Psi_Effort__N_Effort = gam(list(
  maxFlock ~ 
    effortDays + effortHours,
  ~
    effortDays + effortHours),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - Effort no Count----
Psi_Effort__N_dot = gam(list(
  maxFlock ~ 
    dummy,
  ~
    effortDays + effortHours),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Base model - Effort no Detect----
Psi_dot__N_Effort = gam(list(
  maxFlock ~ 
    effortDays + effortHours,
  ~
    dummy),
  family = ziplss,
  gamma = 1.4,
  data = pfw)

#----Full model----
Psi_full__N_full = gam(list(
  maxFlock ~ 
    effortHours +
    effortDays +
    s(day, k=5) +
    s(yr, k=11) +
    s(lat,long),
  ~
    effortHours +
    effortDays +
    s(day, k=5) +
    s(yr, k=11) +
    s(lat,long)),
  family = ziplss,
  gamma = 1.4,
  data = pfw)


#----AIC Model Comparison table----
models.aic = AIC(Psi_Hab__N_Hab_noAG, Psi_Hab__N_Hab_noFOR, Psi_Hab__N_Hab_noOPEN,
                 Psi_Hab__N_Hab_noURB, Psi_Hab__N_Hab_noWAT, Psi_Hab__N_Hab_noWET,Psi_Hab__N_Hab_noSEA,
                 Psi_Hours__N_Hours, Psi_Days__N_Days, Psi_Coord__N_Coord, Psi_Year__N_Year, Psi_dot__N_dot, 
                 Psi_dot__N_Hab, Psi_Hab__N_dot,
                 Psi_Effort__N_Effort, Psi_Effort__N_dot, Psi_dot__N_Effort,
                 Psi_full__N_full_noRE, Psi_full__N_full)
models.weights = akaike.weights(models.aic$AIC)
models.aictab = cbind(models.aic,models.weights)
models.aictab=models.aictab[order(models.aictab["deltaAIC"]),]
View(models.aictab)
