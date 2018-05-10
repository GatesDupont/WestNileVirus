#----Function for yearly lat/long predictions----
latlong.pred.yr = function(year){
  if(exists("sp.pred.year"))rm(sp.pred.year)
  if(exists("pred_qp"))rm(pred_qp)
  sp.pred.year = data.frame(
    locID = "L28176",
    yr = as.numeric(year),
    lat = grid@coords[,"x2"],
    long = grid@coords[,"x1"],
    effortDays = max(pfwct$effortDays),
    effortHours = max(as.numeric(pfwct$effortHours)),
    AG = mean(pfwct$AG),
    FOR = mean(pfwct$FOR),
    OPEN = mean(pfwct$OPEN),
    URB = mean(pfwct$URB),
    WAT = mean(pfwct$WAT),
    WET = mean(pfwct$WET),
    SEA = mean(pfwct$SEA)
  )
  pred_qp = predict(species.zigam, sp.pred.year, type="response", exclude="s(locID)", se.fit=T)
  pred_qp = data.frame(cbind(x = grid@coords[,"x1"], y = grid@coords[,"x2"], z = pred_qp$se.fit))
  return(pred_qp)
}

#----Predicting over grid by year----
nj.yr.raster = list()
nj.yr.yr = c()
for(i in 1996:2017){
  nj.yr.raster = append(nj.yr.raster, rasterFromXYZ(latlong.pred.yr(i)))
  nj.yr.yr = c(nj.yr.yr, i)
}

frames = 22

for(i in 1:frames){
  # creating a name for each plot file with leading zeros
  if (i < 10) {name = paste("~/Desktop/WNV_visuals/SpatialError/000",i,"plot.png",sep="")}
  if (i < 100 && i >= 10) {name = paste("~/Desktop/WNV_visuals/SpatialError/00",i,"plot.png", sep="")}
  if (i >= 100) {name = paste("~/Desktop/WNV_visuals/SpatialError/0", i,"plot.png", sep="")}
  
  #saves the plot as a .png file in the working directory
  png(name, width=8, height=9, units = "in", res=200)
  plot(nj.yr.raster[[i]], col=colorRampPalette(c("yellow", "red"))(7), zlim=c(0,0.45),
       main=paste0("Standard Error, ", as.character(1995+i)), axes=F, frame.plot=F, box=F)
  points(pfwct[pfwct$yr==as.numeric(1995+i),]$long, pfwct[pfwct$yr==as.numeric(1995+i),]$lat, 
         pch=20, cex=0.75, col="black")
  dev.off()
}

