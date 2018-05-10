# Gates Dupont #
# 2018         #
################

species.zigam = species.zigam.intx
pfw = read.csv("~/Desktop/WNV2/PFW_amecro_zerofill_landcover.csv")
pfwct = pfw[pfw$state == "NJ",]

State_In = "NJ"

xxx <- seq(from=1996,
           to = 2017)

#----Trajectory Data - min/max----
if(T){
  #----Trajectiory Data - avg----
  traj.avg <- data.frame(
    locID = "L28176",
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
  
  pred.avg<- stats::predict(
    species.zigam,
    newdata = traj.avg, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)")) 
  
  #----Trajectiory Data - AG----
  traj.data.AG <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = max(pfwct$AG),
    FOR = min(pfwct$FOR),
    OPEN = min(pfwct$OPEN),
    URB = min(pfwct$URB),
    WAT = min(pfwct$WAT),
    WET = min(pfwct$WET),
    SEA = min(pfwct$SEA)
  )
  
  pred.AG<- stats::predict(
    species.zigam,
    newdata = traj.data.AG, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  #----Trajectiory Data - FOR----
  traj.data.FOR <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = max(pfwct$FOR),
    OPEN = min(pfwct$OPEN),
    URB = min(pfwct$URB),
    WAT = min(pfwct$WAT),
    WET = min(pfwct$WET),
    SEA = min(pfwct$SEA)
  )
  
  pred.FOR<- stats::predict(
    species.zigam,
    newdata = traj.data.FOR, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  
  #----Trajectiory Data - OPEN----
  traj.data.OPEN <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = min(pfwct$FOR),
    OPEN = max(pfwct$OPEN),
    URB = min(pfwct$URB),
    WAT = min(pfwct$WAT),
    WET = min(pfwct$WET),
    SEA = min(pfwct$SEA)
  )
  
  pred.OPEN<- stats::predict(
    species.zigam,
    newdata = traj.data.OPEN, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  #----Trajectiory Data - URB----
  traj.data.URB <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = min(pfwct$FOR),
    OPEN = min(pfwct$OPEN),
    URB = max(pfwct$URB),
    WAT = min(pfwct$WAT),
    WET = min(pfwct$WET),
    SEA = min(pfwct$SEA)
  )
  
  pred.URB<- stats::predict(
    species.zigam,
    newdata = traj.data.URB, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  #----Trajectiory Data - WAT----
  traj.data.WAT <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = min(pfwct$FOR),
    OPEN = min(pfwct$OPEN),
    URB = min(pfwct$URB),
    WAT = max(pfwct$WAT),
    WET = min(pfwct$WET),
    SEA = min(pfwct$SEA)
  )
  
  pred.WAT<- stats::predict(
    species.zigam,
    newdata = traj.data.WAT, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  
  #----Trajectiory Data - WET----
  traj.data.WET <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = min(pfwct$FOR),
    OPEN = min(pfwct$OPEN),
    URB = min(pfwct$URB),
    WAT = min(pfwct$WAT),
    WET = max(pfwct$WET),
    SEA = min(pfwct$SEA)
  )
  
  pred.WET<- stats::predict(
    species.zigam,
    newdata = traj.data.WET, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  
  #----Trajectiory Data - SEA----
  traj.data.SEA <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = min(pfwct$FOR),
    OPEN = min(pfwct$OPEN),
    URB = min(pfwct$URB),
    WAT = min(pfwct$WAT),
    WET = min(pfwct$WET),
    SEA = max(pfwct$SEA)
  )
  
  pred.SEA<- stats::predict(
    species.zigam,
    newdata = traj.data.SEA, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  #----Pulling LCLU predictions into df----
  LandLines = data.frame(avg.fit  = pred.avg$fit,   avg.se  =  pred.avg$se.fit,
                         AG.fit   = pred.AG$fit,    AG.se   =  pred.AG$se.fit,
                         FOR.fit  = pred.FOR$fit,   FOR.se  =  pred.FOR$se.fit,
                         OPEN.fit = pred.OPEN$fit,  OPEN.se =  pred.OPEN$se.fit,
                         URB.fit  = pred.URB$fit,   URB.se  =  pred.URB$se.fit,
                         WAT.fit  = pred.WAT$fit,   WAT.se  =  pred.WAT$se.fit,
                         WET.fit  = pred.WET$fit,   WET.se  =  pred.WET$se.fit,
                         SEA.fit  = pred.SEA$fit,   SEA.se  =  pred.SEA$se.fit)
  
}


#----Trajectory Data - 0/1----
if(F){
  #----Trajectiory Data - avg----
  traj.avg <- data.frame(
    locID = "L28176",
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
  
  pred.avg<- stats::predict(
    species.zigam,
    newdata = traj.avg, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)")) 
  
  #----Trajectiory Data - AG----
  traj.data.AG <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = 1,
    FOR = min(pfwct$FOR),
    OPEN = min(pfwct$OPEN),
    URB = min(pfwct$URB),
    WAT = min(pfwct$WAT),
    WET = min(pfwct$WET),
    SEA = min(pfwct$SEA)
  )
  
  pred.AG<- stats::predict(
    species.zigam,
    newdata = traj.data.AG, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  #----Trajectiory Data - FOR----
  traj.data.FOR <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = 1,
    OPEN = min(pfwct$OPEN),
    URB = min(pfwct$URB),
    WAT = min(pfwct$WAT),
    WET = min(pfwct$WET),
    SEA = min(pfwct$SEA)
  )
  
  pred.FOR<- stats::predict(
    species.zigam,
    newdata = traj.data.FOR, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  
  #----Trajectiory Data - OPEN----
  traj.data.OPEN <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = min(pfwct$FOR),
    OPEN = 1,
    URB = min(pfwct$URB),
    WAT = min(pfwct$WAT),
    WET = min(pfwct$WET),
    SEA = min(pfwct$SEA)
  )
  
  pred.OPEN<- stats::predict(
    species.zigam,
    newdata = traj.data.OPEN, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  #----Trajectiory Data - URB----
  traj.data.URB <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = min(pfwct$FOR),
    OPEN = min(pfwct$OPEN),
    URB = 1,
    WAT = min(pfwct$WAT),
    WET = min(pfwct$WET),
    SEA = min(pfwct$SEA)
  )
  
  pred.URB<- stats::predict(
    species.zigam,
    newdata = traj.data.URB, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  #----Trajectiory Data - WAT----
  traj.data.WAT <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = min(pfwct$FOR),
    OPEN = min(pfwct$OPEN),
    URB = min(pfwct$URB),
    WAT = 1,
    WET = min(pfwct$WET),
    SEA = min(pfwct$SEA)
  )
  
  pred.WAT<- stats::predict(
    species.zigam,
    newdata = traj.data.WAT, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  
  #----Trajectiory Data - WET----
  traj.data.WET <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = min(pfwct$FOR),
    OPEN = min(pfwct$OPEN),
    URB = min(pfwct$URB),
    WAT = min(pfwct$WAT),
    WET = 1,
    SEA = min(pfwct$SEA)
  )
  
  pred.WET<- stats::predict(
    species.zigam,
    newdata = traj.data.WET, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  
  #----Trajectiory Data - SEA----
  traj.data.SEA <- data.frame(
    locID = "L28176",
    yr = xxx,
    lat = mean(pfwct$lat),
    long = mean(pfwct$long),
    effortDays = 4,
    effortHours = 4,
    AG = min(pfwct$AG),
    FOR = min(pfwct$FOR),
    OPEN = min(pfwct$OPEN),
    URB = min(pfwct$URB),
    WAT = min(pfwct$WAT),
    WET = min(pfwct$WET),
    SEA = 1
  )
  
  pred.SEA<- stats::predict(
    species.zigam,
    newdata = traj.data.SEA, type="response", interval= "confidence",
    se.fit= TRUE, exclude=c("s(locID)", "s(lat,long)"))
  
  #----Pulling LCLU predictions into df----
  LandLines = data.frame(avg.fit  = pred.avg$fit,   avg.se  =  pred.avg$se.fit,
                         AG.fit   = pred.AG$fit,    AG.se   =  pred.AG$se.fit,
                         FOR.fit  = pred.FOR$fit,   FOR.se  =  pred.FOR$se.fit,
                         OPEN.fit = pred.OPEN$fit,  OPEN.se =  pred.OPEN$se.fit,
                         URB.fit  = pred.URB$fit,   URB.se  =  pred.URB$se.fit,
                         WAT.fit  = pred.WAT$fit,   WAT.se  =  pred.WAT$se.fit,
                         WET.fit  = pred.WET$fit,   WET.se  =  pred.WET$se.fit,
                         SEA.fit  = pred.SEA$fit,   SEA.se  =  pred.SEA$se.fit)
  
}

#----Loading CDC Data by State----
if(T){
  cdcWNV = read.csv("~/Desktop/WNV2/WestNileVirusCDC.csv")
  id1 = which(colnames(cdcWNV)=="X1996")
  id2 = which(colnames(cdcWNV)=="X2017")
  
  WNVstate = as.numeric(cdcWNV[cdcWNV$StateCode == "NJ",c(id1:id2)])
  
  LandLines = cbind(Year=c(1996:2017),  Cases=WNVstate, LandLines)
}

#----Loading Colors----
avg.col.l = rgb(0, 0, 0,0.1)
AG.col.l = rgb(0.9, 0.68, 0.4,0.1)
FOR.col.l = rgb(0.0, 0.24, 0.0,0.1)
OPEN.col.l = rgb(0.70, 0.54,0.20,0.1)
URB.col.l = rgb(0.86, 0.13, 0.15,0.1)
WAT.col.l = rgb(0.3, 0.44, 0.64,0.1)
WET.col.l = rgb(0.42, 0.64, 0.54,0.1)
SEA.col.l = rgb(0.30, 0.40,0.60,0.1)

avg.col.f = rgb(0, 0, 0,1)
AG.col.f = rgb(0.9, 0.68, 0.4,1)
FOR.col.f = rgb(0.0, 0.24, 0.0,1)
OPEN.col.f = rgb(0.70, 0.54,0.20,1)
URB.col.f = rgb(0.86, 0.13, 0.15,1)
WAT.col.f = rgb(0.3, 0.44, 0.64,1)
WET.col.f = rgb(0.42, 0.64, 0.54,1)
SEA.col.f = rgb(0.30, 0.40,0.60,1)


#----Adding proportion to AVG----
avg.fit.p = c()
for(i in 1:length(LandLines$avg.fit)){
  avg.fit.p = c(avg.fit.p, LandLines$avg.fit[i]/LandLines$avg.fit[1])
}
LandLines$avg.fit.p = avg.fit.p

#----Adding proportion to AG----
AG.fit.p = c()
for(i in 1:length(LandLines$AG.fit)){
  AG.fit.p = c(AG.fit.p, LandLines$AG.fit[i]/LandLines$AG.fit[1])
}
LandLines$AG.fit.p = AG.fit.p

#----Adding proportion to FOR----
FOR.fit.p = c()
for(i in 1:length(LandLines$FOR.fit)){
  FOR.fit.p = c(FOR.fit.p, LandLines$FOR.fit[i]/LandLines$FOR.fit[1])
}
LandLines$FOR.fit.p = FOR.fit.p

#----Adding proportion to OPEN----
OPEN.fit.p = c()
for(i in 1:length(LandLines$OPEN.fit)){
  OPEN.fit.p = c(OPEN.fit.p, LandLines$OPEN.fit[i]/LandLines$OPEN.fit[1])
}
LandLines$OPEN.fit.p = OPEN.fit.p

#----Adding proportion to URB----
URB.fit.p = c()
for(i in 1:length(LandLines$URB.fit)){
  URB.fit.p = c(URB.fit.p, LandLines$URB.fit[i]/LandLines$URB.fit[1])
}
LandLines$URB.fit.p = URB.fit.p

#----Adding proportion to WAT----
WAT.fit.p = c()
for(i in 1:length(LandLines$WAT.fit)){
  WAT.fit.p = c(WAT.fit.p, LandLines$WAT.fit[i]/LandLines$WAT.fit[1])
}
LandLines$WAT.fit.p = WAT.fit.p

#----Adding proportion to WET----
WET.fit.p = c()
for(i in 1:length(LandLines$WET.fit)){
  WET.fit.p = c(WET.fit.p, LandLines$WET.fit[i]/LandLines$WET.fit[1])
}
LandLines$WET.fit.p = WET.fit.p

#----Adding proportion to SEA----
SEA.fit.p = c()
for(i in 1:length(LandLines$SEA.fit)){
  SEA.fit.p = c(SEA.fit.p, LandLines$SEA.fit[i]/LandLines$SEA.fit[1])
}
LandLines$SEA.fit.p = SEA.fit.p

#----Plotting----
#----Plot by Region----
if(T){
  # Setting up plot
  par(mar=c(5, 4, 2, 5))
  plot(NULL,frame.plot=T, main=paste0("American Crow - ", State_In),
       xlab = "Year", ylab = "Relative Avian Abundance", lwd=2.5,
       ylim=c(0, 1.25), xlim=c(1996,2017))
  
  # Adding WNV incidence (Intro & Human)
  abline(v = 1999, lty=2, lwd = 2, col = "gray")
  text(1999,0.4,"WNV Introduction", col="gray", cex=0.7, pos=4 ,srt=90, offset=0.5)
  par(new=TRUE)
  plot(LandLines$Cases~LandLines$Year, type="h", lwd=13, lend=1, col="gray",
       frame.plot=F, axes=F, xlab="", ylab="",
       ylim=c(0, max(LandLines$Cases)*1.2))
  axis(4, las=1)
  mtext('Human Cases', 4, 3.5)
  
  # Plotting by AVG 
  par(new=TRUE)
  plot(LandLines$avg.fit.p~LandLines$Year,
       ylim=c(0, 1.25), xlim=c(1996,2017),
       ylab="", xlab="", pch=20, col=avg.col.f)
  lines(LandLines$avg.fit.p~LandLines$Year,lwd=2,col=avg.col.f)
  
  # Plotting by AG
  par(new=TRUE)
  plot(LandLines$AG.fit.p~LandLines$Year,
       ylim=c(0, 1.25), xlim=c(1996,2017),
       ylab="", xlab="", pch=20, col=AG.col.f)
  lines(LandLines$AG.fit.p~LandLines$Year,lwd=2,col=AG.col.f)
  
  # Plotting by FOR
  par(new=TRUE)
  plot(LandLines$FOR.fit.p~LandLines$Year,
       ylim=c(0, 1.25), xlim=c(1996,2017),
       ylab="", xlab="", pch=20, col=FOR.col.f)
  lines(LandLines$FOR.fit.p~LandLines$Year,lwd=2,col=FOR.col.f)
  
  # Plotting by OPEN
  par(new=TRUE)
  plot(LandLines$OPEN.fit.p~LandLines$Year,
       ylim=c(0, 1.25), xlim=c(1996,2017),
       ylab="", xlab="", pch=20, col=OPEN.col.f)
  lines(LandLines$OPEN.fit.p~LandLines$Year,lwd=2,col=OPEN.col.f)
  
  # Plotting by URB
  par(new=TRUE)
  plot(LandLines$URB.fit.p~LandLines$Year,
       ylim=c(0, 1.25), xlim=c(1996,2017),
       ylab="", xlab="", pch=20, col=URB.col.f)
  lines(LandLines$URB.fit.p~LandLines$Year,lwd=2,col=URB.col.f)
  
  # Plotting by WAT
  par(new=TRUE)
  plot(LandLines$WAT.fit.p~LandLines$Year,
       ylim=c(0, 1.25), xlim=c(1996,2017),
       ylab="", xlab="", pch=20, col=WAT.col.f)
  lines(LandLines$WAT.fit.p~LandLines$Year,lwd=2,col=WAT.col.f)
  
  # Plotting by WET
  par(new=TRUE)
  plot(LandLines$WET.fit.p~LandLines$Year,
       ylim=c(0, 1.25), xlim=c(1996,2017),
       ylab="", xlab="", pch=20, col=WET.col.f)
  lines(LandLines$WET.fit.p~LandLines$Year,lwd=2,col=WET.col.f)
  
  # Plotting by SEA
  par(new=TRUE)
  plot(LandLines$SEA.fit.p~LandLines$Year,
       ylim=c(0, 1.25), xlim=c(1996,2017),
       ylab="", xlab="", pch=20, col=SEA.col.f)
  lines(LandLines$SEA.fit.p~LandLines$Year,lwd=2,col=SEA.col.f)
  
  legend("topright",legend=c("AVG","URB","WET","WAT","FOR","OPEN","AG","SEA","Human"), bty="n",
         col=c(avg.col.f,URB.col.f,WET.col.f,WAT.col.f,
               FOR.col.f,OPEN.col.f,AG.col.f,SEA.col.f,"gray"), 
         lty=c(NA), cex=0.5, lwd=c(NA), pch=c(15,15))
}