# Gates Dupont #
# 2018         #
################

# Land cover: http://www.cec.org/tools-and-resources/map-files/land-cover-2010-landsat-30m

#----Setup----
if(T){
  setwd("~/Desktop/WNV2")
}

#----Importing PFW data----
if(T){
  raw.pfw = read.csv("PFW_SPECIES_zerofill.csv")
  # rm(list=setdiff(ls(), "raw.pfw"))
}

#----Libraries----
if(T){
  library(rgdal)
  library(proj4)
  library(sp)
  library(dplyr)
  library(mgcv) 
  library(ggplot2)
  library(raster)
}

#----Converting to duplicate data----
if(T){
  rawData = raw.pfw
}

#----Formatting effort----
if(T){
  rawData[is.na(rawData)] <- 0 # Makes all the NA cells be filled with 0
  rawData$effortDaysNumerical = rawData$DAY1_AM + rawData$DAY1_PM + rawData$DAY2_AM + rawData$DAY2_PM # Summing half days
  
  # Assiging Effort Hours to categorical levels ###
  idx<- rawData$EFFORT_HRS_ATLEAST == "0"
  rawData$obshrs[idx] <- "x"
  idx<- rawData$EFFORT_HRS_ATLEAST == "0.001"
  rawData$obshrs[idx] <- 1
  idx<- rawData$EFFORT_HRS_ATLEAST == "1"
  rawData$obshrs[idx] <- 2
  idx<- rawData$EFFORT_HRS_ATLEAST == "4"
  rawData$obshrs[idx] <- 3
  idx<- rawData$EFFORT_HRS_ATLEAST == "4.001"
  rawData$obshrs[idx] <- 3
  idx<- rawData$EFFORT_HRS_ATLEAST == "8.001"
  rawData$obshrs[idx] <- 4
}

#----Relevant data only----
if(T){
  locID = rawData$LOC_ID
  yr = rawData$FW_Year
  maxFlock = rawData$nseen
  lat = rawData$LATITUDE
  long = rawData$LONGITUDE
  effortHours = rawData$obshrs # empty = x, 0.001 = 1, 1 = 2, 4 = 3, 8+ = 4
  effortDays = rawData$effortDaysNumerical
  state = rawData$StatProv
  
  #Final product:
  dfEffHD = data.frame(locID, yr, maxFlock, lat, long, state, effortDays, effortHours) # just the necessary data
}

#----Removing observations pre-1995----
if(T){
  dfR91 = dfEffHD[dfEffHD$yr != 'PFW_1991',]
  dfR92 = dfR91[dfR91$yr != 'PFW_1992',]
  dfR93 = dfR92[dfR92$yr != 'PFW_1993',]
  dfR94 = dfR93[dfR93$yr != 'PFW_1994',]
  dfR95 = dfR94[dfR94$yr != 'PFW_1995',]
  dfR95 = droplevels(dfR95) # Dropping unused levels
}

#----Removing empty effort----
if(T){
  dfEffRx = subset(dfR95, effortHours != "x") # excluding blank data
  dfDaysR0 = subset(dfEffRx, effortDays != 0) # excluding blank data
}

#----Removing highcounts----
if(T){
  dfPwrUsrs0 = subset(dfDaysR0, maxFlock < 51)
}

#----Selecting power users----
if(T){
  ###Cleaning up the data with conditionals###
  # 1) Only include LocIDs active for at least 3 years ###
  # 2) Only include LocIDs with at least 10 checklists during those years ###
  yearly_list_count = table(dfPwrUsrs0$locID, dfPwrUsrs0$yr) # creates a table showing number of observations at each location each year.
  row_sums = rowSums(yearly_list_count >= 10) # rows where there are at least 10 observations
  threeyears = which(row_sums >=3) # for rows with 10 obs over at least 3 years
  newIDs = names(threeyears) # just setting a new variable name
  #Final product:
  dfPwrUsrs = dfPwrUsrs0[dfPwrUsrs0$locID %in% newIDs,] 
}

#----Data calculations----
if(F){
  # Selecting only regular users
  percent.users.rm = round(100*length(unique(dfPwrUsrs$locID))/length(unique(dfDaysR0$locID)),2)
  count.users.rm = length(dfPwrUsrs$locID)
  paste("After selecting for regular users, only ",percent.users.rm,"% of the users remained, resulting in ", count.users.rm, " remaining observations", sep="")
  # Removed high counts
  count.rm = length(dfDaysR0$locID)-length(dfPwrUsrs$locID)
  percent.count.rm = round(100*(count.rm/length(dfPwrUsrs$locID)),2) 
  paste("Erroneous counts accounted for ", percent.count.rm, "% of the data.", sep="")
  paste("After all data cleaning, there were", length(dfPwrUsrs$locID),"observations remaining.")
} # Check this!

#----Renaming main dataframe----
if(T){
  df4Uniq = data.frame(dfPwrUsrs$locID, dfPwrUsrs$yr, 
                       dfPwrUsrs$lat, dfPwrUsrs$long, 
                       dfPwrUsrs$effortDays, dfPwrUsrs$effortHours, 
                       dfPwrUsrs$maxFlock, dfPwrUsrs$state) #this makes a new df so I can effectively use unique()
  pfw = unique(df4Uniq)
  colnames(pfw) = c('locID', 'yr', 'lat', 'long', 'effortDays', 'effortHours', 'maxFlock', 'state')
  pfw$yr = as.numeric(gsub("[^0-9\\.]", "", pfw$yr))
}

#----Loading NLCD files----
file_name='~/Desktop/WNV2/LandCoverExtract/nlcdData/na_landcover_2010_30m/na_landcover_2010_30m/NA_NALCMS_LC_30m_LAEA_mmu12_urb05/NA_NALCMS_LC_30m_LAEA_mmu12_urb05.tif' 
nlcd=raster(file_name)
legend = read.csv('~/Desktop/WNV2/LandCoverExtract/nlcd_2010_30m_metadata_legend.csv')

#----Unique PFW locs----
u.pfw = unique(pfw[c("long","lat","locID")])

#----Converting PFW data to SPDF----
coords = cbind(u.pfw$long, u.pfw$lat)
sp = SpatialPoints(coords)
spdf = SpatialPointsDataFrame(coords=sp, data=u.pfw)
proj4string(spdf) = CRS("+init=epsg:4326")
spdf = spTransform(spdf, CRS(proj4string(nlcd)))

#----Trying to create a Velox raster----
#library(velox)
#date()
#vx.nlcd = velox(nlcd)
#date()

#----Extract Land Cover----
date()
lclu.ext = extract(nlcd, spdf, buffer = 1000)
date()

#----Calculating Proportional Land Cover----
#lclu.classes = sapply(lclu.ext, FUN = function(x) tabulate(as.vector(x), 19))
#lclu.classes = 100 * (lclu.classes / colSums(lclu.classes))

date()
if(T){
  rm(prop.lc.df)
  prop.lc.df = data.frame(0:19)
  for(i in 1:length(lclu.ext)){
    rm(empty.pr.lc)
    rm(lc.raw)
    empty.pr.lc = data.frame(Var1=0:19, prop=rep(0,20))
    lc.raw = as.data.frame(table(unlist(lclu.ext[[i]])))
    lc.raw$prop = lc.raw$Freq/sum(lc.raw$Freq)
    empty.pr.lc$prop[match(lc.raw$Var1, empty.pr.lc$Var1)] <- lc.raw$prop
    proportion.lc = data.frame(empty.pr.lc$prop)
    prop.lc.df = cbind(prop.lc.df, proportion.lc)
  }
  prop.lc.df = prop.lc.df[,-1]
  prop.lc.df = t(prop.lc.df)
  rownames(prop.lc.df) = NULL
  colnames(prop.lc.df) = as.character(c(0:19))
  colnames(prop.lc.df)[1:20] = as.character(legend$ClassType)
}
date()

#----Flipping classes from rows to columns----
#transpose.step = as.data.frame(t(lclu.classes))
#names(transpose.step)[1:19] = as.character(c(1:19))
#colnames(transpose.step) = as.character(legend$ClassType)

#----Combining the lclu classes product with original data----
LU = cbind(u.pfw, prop.lc.df)
rownames(LU) = NULL # re-index for looping

#----Consolidating land classes----
LU$AG = LU$Cropland
LU$FOR = LU$`Temperate or sub-polar broadleaf deciduous forest`+
  LU$`Temperate or sub-polar needleleaf forest`+
  LU$`Tropical or sub-tropical broadleaf deciduous forest`+
  LU$`Tropical or sub-tropical broadleaf evergreen forest`+
  LU$` Mixed forest`
LU$OPEN = LU$`Barren lands`+
  LU$`Temperate or sub-polar grassland`+
  LU$` Tropical or sub-tropical grassland`+
  LU$` Temperate or sub-polar shrubland`+
  LU$` Tropical or sub-tropical shrubland`
LU$URB = LU$Urban
LU$WAT = LU$Water
LU$WET = LU$Wetland
LU$SEA = LU$Ocean

#----Reassigning land classes to dove data----
pfw$AG = rep(0,length(pfw$locID))
pfw$FOR = rep(0,length(pfw$locID))
pfw$OPEN = rep(0,length(pfw$locID))
pfw$URB = rep(0,length(pfw$locID))
pfw$WAT = rep(0,length(pfw$locID))
pfw$WET = rep(0,length(pfw$locID))
pfw$SEA = rep(0,length(pfw$locID))

date()
for(i in 1:length(LU$locID)){
  pfw[pfw$locID==LU$locID[i],][,9:15]=LU[i,24:30]
}
date()

#--------------------------------------------State Input--------------------------------------------
if(T){
  State_In = "NJ"
}

#----Selecting a state/region----
if(T){
  pfwct = pfw[pfw$state == State_In,]
  pfwct$state = NULL
}

#----Class conversion for ziplss gam, put this up higher----
if(T){
  pfwct$maxFlock = as.numeric(pfwct$maxFlock)
  pfwct$effortDays = as.numeric(pfwct$effortDays)
  pfwct$effortHours = as.numeric(pfwct$effortHours)
}

#----Two-step Zero-Infalted Poisson Generalized Additive Model----
date()
if(T){
  species.zigam = gam(list(
    maxFlock ~ 
      s(yr, k=11) +
      s(locID, bs="re") +
      s(lat,long),
    ~
      effortHours +
      effortDays +
      s(AG, k=5) + s(FOR, k=5) + 
      s(OPEN, k=5) + s(URB, k=5) + 
      s(WAT, k=5) + s(WET, k=5) + 
      s(SEA, k=5) +
      s(yr, k=11) +
      s(locID, bs="re") +
      s(lat,long)),
    family = ziplss,
    gamma = 1.4,
    data = pfwct) # k=6
  # +s(locID, bs="re")
}
date()

#----Predictions from ziplss (2-step ZI) GAM----
if(T){
  nnn.pred <- length(unique(pfwct$yr))
  xxx <- seq(from=1996,
             to = 2017,
             length = nnn.pred)
  
  # Testing dataset
  traj.data <- data.frame(
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = mean(pfwct$effortDays),
    effortHours = mean(pfwct$effortHours),
    AG = mean(pfwct$AG),
    FOR = mean(pfwct$FOR),
    OPEN = mean(pfwct$OPEN),
    URB = mean(pfwct$URB),
    WAT = mean(pfwct$WAT),
    WET = mean(pfwct$WET),
    SEA = mean(pfwct$SEA)
  )
  
  pred.test<- stats::predict(
    species.zigam,
    newdata = traj.data, type="response", interval= "confidence",se.fit= TRUE)
}

#----Loading CDC Data by State----
if(T){
  cdcWNV = read.csv("~/Desktop/WNV2/WestNileVirusCDC.csv") # 2015 sum is incorrect, error on cdc site, explore further
  id1 = which(colnames(cdcWNV)=="X1996")
  id2 = which(colnames(cdcWNV)=="X2017")
  #colnames(cdcWNV)[id1:id2] = c(1996:2017)
  
  WNVstate = as.numeric(cdcWNV[cdcWNV$StateCode == State_In,c(id1:id2)])
  
  plot_data = data.frame(Year=c(1996:2017), Count=pred.test$fit, CountSE=pred.test$se.fit, Cases=WNVstate)
}

#----Plot by Region----
if(T){
  ymax.vals = c()
  ymin.vals = c()
  for(i in 1:22){
    ymax.vals = c(ymax.vals, plot_data$Count[i] + plot_data$CountSE[i]*1.96)
    ymin.vals = c(ymin.vals, plot_data$Count[i] - plot_data$CountSE[i]*1.96)
  }
  ymin = min(ymin.vals) - (0.75*(max(ymax.vals)-min(ymin.vals)))
  ymax = max(ymax.vals) + (0.15*(max(ymax.vals)-min(ymin.vals)))
    
  par(mar=c(5, 4, 2, 5))
  plot(plot_data$Count~plot_data$Year, pch=20, col="black", 
       frame.plot=T, main=paste0("American Crow - ", State_In),
       xlab = "Year", ylab = "Relative Avian Abundance", lwd=2.5,
       ylim=c(ymin, ymax))
  lines(pred.test$fit~xxx,lwd=2, col="black")
  lines(pred.test$fit+1.96*pred.test$se.fit ~ xxx,lwd=1, col="gray40", lty=1)
  lines(pred.test$fit-1.96*pred.test$se.fit ~ xxx,lwd=1, col="gray40", lty=1)
  abline(v = 1999, lty=2, lwd = 2, col = "gray35")
  text(1999,min(plot_data$Count),"WNV Introduction", cex=0.7, pos=4 ,srt=90, offset=0.5)
  legend("topright",legend=c("Birds","Humans"), bty="n", col=c("black","gray"), lty=c(1,NA), cex=0.7, lwd=c(2,NA), pch=c(20,15))
  par(new=TRUE)
  plot(plot_data$Cases~plot_data$Year, type="h", lwd=13, lend=1, col="gray",
       frame.plot=F, axes=F, xlab="", ylab="",
       ylim=c(0, max(plot_data$Cases)*1.2))
  axis(4, las=1)
  mtext('Human Cases', 4, 3.5)
  par(new=TRUE)
  plot(plot_data$Count~plot_data$Year, pch=20, col="black", 
       frame.plot=T, main="",
       xlab = "", ylab = "", lwd=2.5,
       ylim=c(ymin, ymax))
  lines(pred.test$fit~xxx,lwd=2,col="black")
  lines(pred.test$fit+1.96*pred.test$se.fit ~ xxx,lwd=1, col="gray40", lty=1)
  lines(pred.test$fit-1.96*pred.test$se.fit ~ xxx,lwd=1, col="gray40", lty=1)
  abline(v = 1999, lty=2, lwd = 2, col = "gray35")
  text(1999,min(plot_data$Count),"WNV Introduction", cex=0.7, pos=4 ,srt=90, offset=0.5)
}


#----Checking locations of PFW location----
library(leaflet)
leaflet(u.pfw[8919,]) %>% addTiles() %>% 
  addCircleMarkers(radius=0.7, popup = u.pfw$locID[8919])

match("L77028", spdf@data$locID)

t = c()
f = c()
for(i in 1:length(lclu.ext)){
  if(0 %in% lclu.ext[[i]]){
    t=c(t, 1)
  }else{f=c(f, 0)}
}
length(t)/length(f)
