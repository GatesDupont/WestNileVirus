#----Predictions from ziplss (2-step ZI) GAM----
if(T){
  nnn.pred <- length(unique(pfwct$yr))
  xxx <- seq(from=1996,
             to = 2017,
             length = nnn.pred)
}

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

#----Loading CDC Data by State----
if(T){
  cdcWNV = read.csv("~/Desktop/WNV2/WestNileVirusCDC.csv")
  id1 = which(colnames(cdcWNV)=="X1996")
  id2 = which(colnames(cdcWNV)=="X2017")
  
  WNVstate = as.numeric(cdcWNV[cdcWNV$StateCode == State_In,c(id1:id2)])
  
  LandLines = cbind(Year=c(1996:2017),  Cases=WNVstate, LandLines)
}

#----Loading Colors----
avg.col.o = rgb(0, 0, 0,0.5)
AG.col.o = rgb(0.9, 0.68, 0.4,0.5)
FOR.col.o = rgb(0.0, 0.24, 0.0,0.5)
OPEN.col.o = rgb(0.70, 0.54,0.20,0.5)
URB.col.o = rgb(0.86, 0.13, 0.15,0.5)
WAT.col.o = rgb(0.3, 0.44, 0.64,0.5)
WET.col.o = rgb(0.42, 0.64, 0.54,0.5)
SEA.col.o = rgb(0.30, 0.40,0.60,0.5)

avg.col.f = rgb(0, 0, 0,1)
AG.col.f = rgb(0.9, 0.68, 0.4,1)
FOR.col.f = rgb(0.0, 0.24, 0.0,1)
OPEN.col.f = rgb(0.70, 0.54,0.20,1)
URB.col.f = rgb(0.86, 0.13, 0.15,1)
WAT.col.f = rgb(0.3, 0.44, 0.64,1)
WET.col.f = rgb(0.42, 0.64, 0.54,1)
SEA.col.f = rgb(0.30, 0.40,0.60,1)

#----Plot by Region----
if(T){
  ymax.vals = c()
  ymin.vals = c()
  for(i in 1:22){
    ymax.vals = c(ymax.vals, LandLines$avg.fit[i] + LandLines$avg.se[i]*1.96)
    ymin.vals = c(ymin.vals, LandLines$avg.fit[i] - LandLines$avg.se[i]*1.96)
  }
  ymin = min(ymin.vals) - (0.75*(max(ymax.vals)-min(ymin.vals)))
  ymax = max(ymax.vals) + (0.15*(max(ymax.vals)-min(ymin.vals)))
  
  # Setting up plot
  par(mar=c(5, 4, 2, 5))
  plot(NULL,frame.plot=T, main=paste0("American Crow - ", State_In),
       xlab = "Year", ylab = "Relative Avian Abundance", lwd=2.5,
       ylim=c(ymin, ymax), xlim=c(1996,2017))
  
  # Adding WNV incidence (Intro & Human)
  abline(v = 1999, lty=2, lwd = 2, col = "gray")
  text(1999,min(LandLines$avg.fit),"WNV Introduction", col="gray", cex=0.7, pos=4 ,srt=90, offset=0.5)
  par(new=TRUE)
  plot(LandLines$Cases~LandLines$Year, type="h", lwd=13, lend=1, col="gray",
       frame.plot=F, axes=F, xlab="", ylab="",
       ylim=c(0, max(LandLines$Cases)*1.2))
  axis(4, las=1)
  mtext('Human Cases', 4, 3.5)
  
  # Plotting avg
  par(new=TRUE)
  plot(LandLines$avg.fit~LandLines$Year, pch=20, col=avg.col.f, 
       frame.plot=T, main="",
       xlab = "", ylab = "", lwd=2.5,
       ylim=c(ymin, ymax))
  polygon(c(1996:2017,rev(1996:2017)),
          c(LandLines$avg.fit+1.96*LandLines$avg.se,rev(LandLines$avg.fit-1.96*LandLines$avg.se)),
          col=avg.col.o, border=NA)
  lines(LandLines$avg.fit~xxx,lwd=2,col=avg.col.f)

  # Plotting AG
  par(new=TRUE)
  plot(LandLines$AG.fit~LandLines$Year, pch=20, col=AG.col.f, 
       frame.plot=T, main="", cex=0.5,
       xlab = "", ylab = "", lwd=2.5,
       ylim=c(ymin, ymax))
  polygon(c(1996:2017,rev(1996:2017)),
          c(LandLines$AG.fit+1.96*LandLines$AG.se,rev(LandLines$AG.fit-1.96*LandLines$AG.se)),
          col=AG.col.o, border=NA)
  lines(LandLines$AG.fit~xxx,lwd=2,col=AG.col.f)
  
  # Plotting FOR
  par(new=TRUE)
  plot(LandLines$FOR.fit~LandLines$Year, pch=20, col=FOR.col.f, 
       frame.plot=T, main="", cex=0.5,
       xlab = "", ylab = "", lwd=2.5,
       ylim=c(ymin, ymax))
  polygon(c(1996:2017,rev(1996:2017)),
          c(LandLines$FOR.fit+1.96*LandLines$FOR.se,rev(LandLines$FOR.fit-1.96*LandLines$FOR.se)),
          col=FOR.col.o, border=NA)
  lines(LandLines$FOR.fit~xxx,lwd=2,col=FOR.col.f)
  
  # Plotting OPEN
  par(new=TRUE)
  plot(LandLines$OPEN.fit~LandLines$Year, pch=20, col=OPEN.col.f, 
       frame.plot=T, main="", cex=0.5,
       xlab = "", ylab = "", lwd=2.5,
       ylim=c(ymin, ymax))
  polygon(c(1996:2017,rev(1996:2017)),
          c(LandLines$OPEN.fit+1.96*LandLines$OPEN.se,rev(LandLines$OPEN.fit-1.96*LandLines$OPEN.se)),
          col=OPEN.col.o, border=NA)
  lines(LandLines$OPEN.fit~xxx,lwd=2,col=OPEN.col.f)
  
  # Plotting URB
  par(new=TRUE)
  plot(LandLines$URB.fit~LandLines$Year, pch=20, col=URB.col.f, 
       frame.plot=T, main="", cex=0.5,
       xlab = "", ylab = "", lwd=2.5,
       ylim=c(ymin, ymax))
  polygon(c(1996:2017,rev(1996:2017)),
          c(LandLines$URB.fit+1.96*LandLines$URB.se,rev(LandLines$URB.fit-1.96*LandLines$URB.se)),
          col=URB.col.o, border=NA)
  lines(LandLines$URB.fit~xxx,lwd=2,col=URB.col.f)

  # Plotting WAT
  par(new=TRUE)
  plot(LandLines$WAT.fit~LandLines$Year, pch=20, col=WAT.col.f, 
       frame.plot=T, main="", cex=0.5,
       xlab = "", ylab = "", lwd=2.5,
       ylim=c(ymin, ymax))
  polygon(c(1996:2017,rev(1996:2017)),
          c(LandLines$WAT.fit+1.96*LandLines$WAT.se,rev(LandLines$WAT.fit-1.96*LandLines$WAT.se)),
          col=WAT.col.o, border=NA)
  lines(LandLines$WAT.fit~xxx,lwd=2,col=WAT.col.f)
  
  # Plotting WET
  par(new=TRUE)
  plot(LandLines$WET.fit~LandLines$Year, pch=20, col=WET.col.f, 
       frame.plot=T, main="", cex=0.5,
       xlab = "", ylab = "", lwd=2.5,
       ylim=c(ymin, ymax))
  polygon(c(1996:2017,rev(1996:2017)),
          c(LandLines$WET.fit+1.96*LandLines$WET.se,rev(LandLines$WET.fit-1.96*LandLines$WET.se)),
          col=WET.col.o, border=NA)
  lines(LandLines$WET.fit~xxx,lwd=2,col=WET.col.f)
  
  # Plotting SEA
  par(new=TRUE)
  plot(LandLines$SEA.fit~LandLines$Year, pch=20, col=SEA.col.f, 
       frame.plot=T, main="", cex=0.5,
       xlab = "", ylab = "", lwd=2.5,
       ylim=c(ymin, ymax))
  polygon(c(1996:2017,rev(1996:2017)),
          c(LandLines$SEA.fit+1.96*LandLines$SEA.se,rev(LandLines$SEA.fit-1.96*LandLines$SEA.se)),
          col=SEA.col.o, border=NA)
  lines(LandLines$SEA.fit~xxx,lwd=2,col=SEA.col.f)
  
  legend("topright",legend=c("AVG","URB","WET","WAT","FOR","OPEN","AG","SEA","Human"), bty="n",
         col=c(avg.col.f,URB.col.f,WET.col.f,WAT.col.f,
               FOR.col.f,OPEN.col.f,AG.col.f,SEA.col.f,"gray"), 
         lty=c(NA), cex=0.5, lwd=c(NA), pch=c(15,15))
}