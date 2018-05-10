# Gates Dupont #
# 2018         #
################
library(raster)

#----Loading GAM for predictions----
if(T){
  load("~/Desktop/WNV2/species.zigam.intx.RData")
  species.zigam = species.zigam.intx
}

#----Loading NLCD files----
file_name='~/Desktop/WNV2/LandCoverExtract/nlcdData/na_landcover_2010_30m/na_landcover_2010_30m/NA_NALCMS_LC_30m_LAEA_mmu12_urb05/NA_NALCMS_LC_30m_LAEA_mmu12_urb05.tif' 
nlcd=raster(file_name)
legend = read.csv('~/Desktop/WNV2/LandCoverExtract/nlcd_2010_30m_metadata_legend.csv')

#----Load some spatial data. Administrative Boundary----
us <- raster::getData('GADM', country = 'US', level = 1)
nj <- us[us$NAME_1 == "New Jersey",]

#----Create a grid of points within the bbox of the SpatialPolygonsDataFrame---- 
# nj with decimal degrees as map units
grid <- makegrid(nj, cellsize = 0.002) # cellsize in map units!
# I can have 25102222 total points

#----Making Spatial Data from grid----
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(nj)))
grid <- grid[nj, ]

#----Initializing Prediction SPDF----
nj.lc.pred = data.frame(
  locID = rep("L28176", length(grid@coords)/2),
  yr = rep(2004, length(grid@coords)/2),
  effortDays = rep(4, length(grid@coords)/2),
  effortHours = rep(4, length(grid@coords)/2)
)
nj.lc.pred = data.frame(grid@coords, nj.lc.pred)

#----Converting to nlcd projection for extract----
nj.lc.pred.spdf = SpatialPointsDataFrame(coords = nj.lc.pred[,c("x1","x2")], data= nj.lc.pred)
crs(nj.lc.pred.spdf) =  crs(nj)
nj.lc.pred.spdf = spTransform(nj.lc.pred.spdf, CRS(proj4string(nlcd)))
nlcd.nj = crop(nlcd, nj.lc.pred.spdf)

#----Extracting Land Cover from test points----
date()
nj.lclu.ext = extract(nlcd.nj, nj.lc.pred.spdf, buffer = 1000)
date() # Less than 30 seconds

#----Checking for NAs in lulc extract----
na.c = c()
for(i in 1:length(empty.pr.lc$Var1)){
  if(anyNA(lc.raw$Var1[i]) == T){
    na.c = c(na.c, i)
  }
}

#----Calculating proportional landcover----
date()
if(T){
  if(exists("prop.lc.df")){rm(prop.lc.df)}
  prop.lc.df = data.frame(0:19)
  for(i in 1:length(nj.lclu.ext)){
    if(exists("empty.pr.lc")){rm(empty.pr.lc)}
    if(exists("lc.raw")){rm(lc.raw)}
    empty.pr.lc = data.frame(Var1=0:19, prop=rep(0,20))
    lc.raw = as.data.frame(table(unlist(nj.lclu.ext[[i]])))
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

#----Combining the lclu classes product with original data----
LU = cbind(nj.lc.pred, prop.lc.df)
rownames(LU) = NULL # re-index for looping

#----Consolidating land classes------
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

#----Prediction dataset----
traj.lclu = LU[,c(1:6,27:33)]
colnames(traj.lclu)[1:2] = c("long","lat")

#----Yearly predictions----
year = 2004
{
  #----Assigning Year----
  traj.lclu[,"yr"] = year
  
  #----Taking predcitions----
  pred.lclu<- stats::predict(
    species.zigam,
    newdata = traj.lclu, type="response", interval= "confidence",
    se.fit= F, exclude="s(locID)") 
  pred_qp = data.frame(cbind(x = traj.lclu$long, y = traj.lclu$lat, z = pred.lclu))
  
  #----Playing with plot colors----
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=colorRampPalette(c("black", "darkorange4", "darkorange", "white"))(100),
       frame.plot=F, box=F, axes=F, main=as.character(year), 
       par(bg = 'black', col="white", col.lab="white", col.main="white", col.axis="white", col.sub="white")) 
  dev.off()
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=colorRampPalette(c("darkgreen", "yellow", "red"))(100),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=colorRampPalette(c("red", "yellow"))(100),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=colorRampPalette(c("black", "darkgreen", "yellow", "red"))(100),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=colorRampPalette(c("black", "darkgreen", "yellow", "red"))(100),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=colorRampPalette(c("black", "blue2", "green2", "yellow2"))(100),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 

  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=colorRampPalette(c("black", "steelblue4", "steelblue2", "white"))(100),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=colorRampPalette(c("black", "red", "orange", "yellow"))(100),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=colorRampPalette(c("black", "steelblue3", "white", "coral"))(100),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=colorRampPalette(c("black", "white"))(100),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=c(rep("black",17),colorRampPalette(rev(brewer.pal(n = 9, name = "RdBu")))(100)),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=c(rep("navy",17),colorRampPalette(c("blue", "beige"))(100)),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 
  
  #----Plotting species niche model by year----
  plot(rasterFromXYZ(pred_qp), col=colorRampPalette(rev(brewer.pal(n = 9, name = "RdBu")))(100),
       frame.plot=F, box=F, axes=F, main=as.character(year)) 

}

#----Make GIF----
if(F){
  frames = 22
  
  for(i in 1:frames){
    # creating a name for each plot file with leading zeros
    if (i < 10) {name = paste("~/Desktop/WNV_visuals/YearNicheModel/000",i,"plot.png",sep="")}
    if (i < 100 && i >= 10) {name = paste("~/Desktop/WNV_visuals/YearNicheModel/00",i,"plot.png", sep="")}
    if (i >= 100) {name = paste("~/Desktop/WNV_visuals/YearNicheModel/0", i,"plot.png", sep="")}
    
    #saves the plot as a .png file in the working directory
    #----Yearly predictions----
    year = 1995+i
    #----Assigning Year----
    traj.lclu[,"yr"] = year
    #----Taking predcitions----
    pred.lclu<- stats::predict(
      species.zigam,
      newdata = traj.lclu, type="response", interval= "confidence",
      se.fit= F, exclude="s(locID)") 
    pred_qp = data.frame(cbind(x = traj.lclu$long, y = traj.lclu$lat, z = pred.lclu))
    png(name, width=8, height=9, units = "in", res=200)
    #----Plotting species niche model by year----
    plot(rasterFromXYZ(pred_qp), col=colorRampPalette(c("red", "yellow"))(100),
         frame.plot=F, box=F, axes=F, main=as.character(year), zlim=c(0,3.9)) 
    dev.off()
  }
}

#----Plotting species niche model change by year----
c.rgb = c()
for(i in 1:length(legend$Red)){
  c.rgb = c(c.rgb, rgb(legend$Red[i], legend$Green[i], legend$Blue[i]))
}
breaks = c()
for(i in -1:19){
  breaks=c(breaks,i+0.5)
}
plot(nlcd.nj, col=c.rgb, breaks=breaks, legend=F,
     frame.plot=F, box=F, axes=F, main="") 
legend(2160000,-80000,legend=c(as.character(legend$ClassType)),fill=c.rgb, cex=0.55, bty="n", par(xpd=TRUE))

par(mfrow=c(1,2))
plot(nlcd.nj, col=c.rgb, breaks=breaks, legend=F,
     frame.plot=F, box=F, axes=F, main="") 
legend(2160000,-80000,legend=c(as.character(legend$ClassType)),fill=c.rgb, cex=0.55, bty="n", par(xpd=TRUE))
plot((nj.2004-nj.1999)/nj.1999, col=colorRampPalette(c("red", "orange", "yellow", "green","blue"))(100),
     frame.plot=F, box=F, axes=F, main="(2004-1999)/1999") 

#----Using levelplot to plot nlcd more easily----
library(rasterVis)
levelplot(nlcd.nj,col.regions=c.rgb, labels=c(as.character(legend$ClassType)))
