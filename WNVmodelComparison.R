library(mgcv)

#pfw = read.csv("~/Desktop/WNV2/PFW_amecro_zerofill_landcover.csv")
#pfwct = pfw[pfw$state=="NJ",]

#----Class conversion for ziplss gam, put this up higher----
if(F){
  pfwct$maxFlock = as.numeric(pfwct$maxFlock)
  pfwct$effortDays = as.numeric(pfwct$effortDays)
  pfwct$effortHours = as.numeric(pfwct$effortHours)
  pfwct$yr = as.factor(pfwct$yr)
  pfwct$dummy = 1
}

#------------Models------------

#--------ROUND 1--------

#----Base model - Dot model----
Psi_dot__N_dot = gam(list(
  maxFlock ~ 
    dummy,
  ~
    dummy),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Time Dependence----
Psi_Year__N_Year = gam(list(
  maxFlock ~ 
    s(yr, k=11),
  ~
    s(yr, k=11)),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Coordinates----
Psi_Coord__N_Coord = gam(list(
  maxFlock ~ 
    s(lat,long),
  ~
    s(lat,long)),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Effort Days----
Psi_Days__N_Days = gam(list(
  maxFlock ~ 
    effortDays,
  ~
    effortDays),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Effort Hours----
Psi_Hours__N_Hours = gam(list(
  maxFlock ~ 
    effortHours,
  ~
    effortHours),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Habitat - noSea----
Psi_Hab__N_Hab_noSEA = gam(list(
  maxFlock ~
    s(AG, k=5) + s(FOR, k=5) +
    s(OPEN, k=5) + s(URB, k=5) +
    s(WAT, k=5) + s(WET, k=5),
  ~
    s(AG, k=5) + s(FOR, k=5) +
    s(OPEN, k=5) + s(URB, k=5) +
    s(WAT, k=5) + s(WET, k=5)),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Habitat - noWET----
Psi_Hab__N_Hab_noWET = gam(list(
  maxFlock ~
    s(AG, k=5) + s(FOR, k=5) +
    s(OPEN, k=5) + s(URB, k=5) +
    s(WAT, k=5) + s(SEA, k=5),
  ~
    s(AG, k=5) + s(FOR, k=5) +
    s(OPEN, k=5) + s(URB, k=5) +
    s(WAT, k=5) + s(SEA, k=5)),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Habitat - noWAT----
Psi_Hab__N_Hab_noWAT = gam(list(
  maxFlock ~
    s(AG, k=5) + s(FOR, k=5) +
    s(OPEN, k=5) + s(URB, k=5) +
    s(WET, k=5) + s(SEA, k=5),
  ~
    s(AG, k=5) + s(FOR, k=5) +
    s(OPEN, k=5) + s(URB, k=5) +
    s(WET, k=5) + s(SEA, k=5)),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Habitat - noURB----
Psi_Hab__N_Hab_noURB = gam(list(
  maxFlock ~
    s(AG, k=5) + s(FOR, k=5) +
    s(OPEN, k=5) + s(WAT, k=5) +
    s(WET, k=5) + s(SEA, k=5),
  ~
    s(AG, k=5) + s(FOR, k=5) +
    s(OPEN, k=5) + s(WAT, k=5) +
    s(WET, k=5) + s(SEA, k=5)),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Habitat - noOPEN----
Psi_Hab__N_Hab_noOPEN = gam(list(
  maxFlock ~
    s(AG, k=5) + s(FOR, k=5) +
    s(URB, k=5) + s(WAT, k=5) +
    s(WET, k=5) + s(SEA, k=5),
  ~
    s(AG, k=5) + s(FOR, k=5) +
    s(URB, k=5) + s(WAT, k=5) +
    s(WET, k=5) + s(SEA, k=5)),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Habitat - noFOR----
Psi_Hab__N_Hab_noFOR = gam(list(
  maxFlock ~
    s(AG, k=5) + s(OPEN, k=5) +
    s(URB, k=5) + s(WAT, k=5) +
    s(WET, k=5) + s(SEA, k=5),
  ~
    s(AG, k=5) + s(OPEN, k=5) +
    s(URB, k=5) + s(WAT, k=5) +
    s(WET, k=5) + s(SEA, k=5)),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Habitat - noAG----
Psi_Hab__N_Hab_noAG = gam(list(
  maxFlock ~
    s(FOR, k=5) + s(OPEN, k=5) +
    s(URB, k=5) + s(WAT, k=5) +
    s(WET, k=5) + s(SEA, k=5),
  ~
    s(FOR, k=5) + s(OPEN, k=5) +
    s(URB, k=5) + s(WAT, k=5) +
    s(WET, k=5) + s(SEA, k=5)),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Hab and detection/count - no Count----
Psi_dot__N_Hab = gam(list(
  maxFlock ~
    dummy,
  ~
    s(AG, k=5) + s(FOR, k=5) +
    s(OPEN, k=5) + s(URB, k=5) +
    s(WAT, k=5) + s(WET, k=5)),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Hab and detection/count - no Detect----
Psi_Hab__N_dot = gam(list(
  maxFlock ~
    s(AG, k=5) + s(FOR, k=5) +
    s(OPEN, k=5) + s(URB, k=5) +
    s(WAT, k=5) + s(WET, k=5),
  ~
    dummy),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Effort in both----
Psi_Effort__N_Effort = gam(list(
  maxFlock ~ 
    effortDays + effortHours,
  ~
    effortDays + effortHours),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Effort no Count----
Psi_Effort__N_dot = gam(list(
  maxFlock ~ 
    dummy,
  ~
    effortDays + effortHours),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Base model - Effort no Detect----
Psi_dot__N_Effort = gam(list(
  maxFlock ~ 
    effortDays + effortHours,
  ~
    dummy),
  family = ziplss,
  gamma = 1.4,
  data = pfwct)

#----Full model - no Random Effects----
date()
Psi_full__N_full_noRE = gam(list(
  maxFlock ~ 
    effortHours +
    effortDays +
    s(yr, k=11) +
    s(AG, k=5) + s(FOR, k=5) + 
    s(OPEN, k=5) + s(URB, k=5) + 
    s(WAT, k=5) + s(WET, k=5) + 
    s(lat,long),
  ~
    effortHours +
    effortDays +
    s(yr, k=11) +
    s(AG, k=5) + s(FOR, k=5) + 
    s(OPEN, k=5) + s(URB, k=5) + 
    s(WAT, k=5) + s(WET, k=5) + 
    s(yr, k=11) +
    s(lat,long)),
  family = ziplss,
  gamma = 1.4,
  data = pfwct) # k=6
date() # running without by=yr works

#----Full model----
date()
Psi_full__N_full = gam(list(
  maxFlock ~ 
    effortHours +
    effortDays +
    s(yr, k=11) +
    s(AG, k=5) + s(FOR, k=5) + 
    s(OPEN, k=5) + s(URB, k=5) + 
    s(WAT, k=5) + s(WET, k=5) + 
    s(lat,long) +
    s(locID, bs="re"),
  ~
    effortHours +
    effortDays +
    s(yr, k=11) +
    s(AG, k=5) + s(FOR, k=5) + 
    s(OPEN, k=5) + s(URB, k=5) + 
    s(WAT, k=5) + s(WET, k=5) + 
    s(yr, k=11) +
    s(lat,long) +
    s(locID, bs="re")),
  family = ziplss,
  gamma = 1.4,
  data = pfwct) # k=6
date()

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
