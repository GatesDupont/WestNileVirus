lc.seq = seq(min(pfwct$WET), max(pfwct$WET), by=0.001)

testing = data.frame(
  long = mean(pfwct$long),
  lat = mean(pfwct$lat),
  locID = "L28176",
  yr = 2004,
  effortDays = 4,
  effortHours = 4,
  AG = min(pfwct$AG),
  FOR = min(pfwct$FOR),
  OPEN = min(pfwct$OPEN),
  URB = min(pfwct$URB),
  WAT = min(pfwct$WAT),
  WET = lc.seq,
  SEA = min(pfwct$SEA)
)

pred.lclu<- stats::predict(
  species.zigam,
  newdata = testing, type="response", interval= "confidence",
  se.fit= T, exclude=c("s(locID)", "s(lat,long)")) 
error_top = pred.lclu$fit+1.96*pred.lclu$se.fit
error_bottom = pred.lclu$fit-1.96*pred.lclu$se.fit

plot(pred.lclu$fit ~ lc.seq, type="l", lwd=2, 
     ylim=c(min(error_bottom), max(error_top)),
     ylab="Predicted Crow Abundance", xlab="Proportion Landcover",
     main="Wetland", col=WET.col.f)
polygon(c(lc.seq,rev(lc.seq)),
        c(error_top,rev(error_bottom)),
        col=WET.col.o, border=NA)
