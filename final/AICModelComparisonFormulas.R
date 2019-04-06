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
#------------------------------------------------MODELS------------------------------------------------

# DOT
#----Base model - Dot model----
Psi_dot__N_dot = list(
  maxFlock ~ 
    dummy,
  ~
    dummy)

# TIME
#----Base model - 1 Time Dependence----
Psi_Year__N_dot = list(
  maxFlock ~ 
    dummy,
  ~
    s(yr, k=11))

#----Base model - 2 Time Dependence----
Psi_dot__N_Year = list(
  maxFlock ~ 
    s(yr, k=11),
  ~
    dummy)

#----Base model - 1 2 Time Dependence----
Psi_Year__N_Year = list(
  maxFlock ~ 
    s(yr, k=11),
  ~
    s(yr, k=11))

# LOCATION
#----Base model - 1 Coordinates----
Psi_Coord__N_dot = list(
  maxFlock ~ 
    dummy,
  ~
    s(lat,long))

#----Base model - 2 Coordinates----
Psi_dot__N_Coord = list(
  maxFlock ~ 
    s(lat,long),
  ~
    dummy)

#----Base model - 1 2 Coordinates----
Psi_Coord__N_Coord = list(
  maxFlock ~ 
    s(lat,long),
  ~
    s(lat,long))

# EFFORT DAYS
#----Base model - 1 Effort Days----
Psi_Days__N_dot = list(
  maxFlock ~ 
    dummy,
  ~
    effortDays)

#----Base model - 2 Effort Days----
Psi_dot__N_Days = list(
  maxFlock ~ 
    effortDays,
  ~
    dummy)

#----Base model - 1 2 Effort Days----
Psi_Days__N_Days = list(
  maxFlock ~ 
    effortDays,
  ~
    effortDays)

#----Base model - 1 Effort Hours----
Psi_Hours__N_dot = list(
  maxFlock ~ 
    dummy,
  ~
    effortHours)

#----Base model - 2 Effort Hours----
Psi_dot__N_Hours = list(
  maxFlock ~ 
    effortHours,
  ~
    dummy)

#----Base model - 1 2 Effort Hours----
Psi_Hours__N_Hours = list(
  maxFlock ~ 
    effortHours,
  ~
    effortHours)

model.names = c("Psi_dot__N_dot",
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
