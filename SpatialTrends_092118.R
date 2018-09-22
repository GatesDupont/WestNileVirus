# Gates Dupont #
# 2018         #
# # # # # # # #

library(tidyverse)
library(lubridate)
library(foreach)
library(velox)
library(raster)
library(sp)
library(rgeos)
library(RColorBrewer)
library(sp)


#### LOADING PFW & NLCD DATA ####

#----Loading required files----
file_name='~/WNV F18/LandCoverExtract/nlcdData/na_landcover_2010_30m/na_landcover_2010_30m/NA_NALCMS_LC_30m_LAEA_mmu12_urb05/NA_NALCMS_LC_30m_LAEA_mmu12_urb05.tif' 
nlcd=raster(file_name)
legend = read.csv('~/WNV F18/LandCoverExtract/nlcd_2010_30m_metadata_legend.csv')

#----Loading and subsetting PFW data----
pfw = read.csv('~/WNV F18/PFW_amecro_zerofill_landcover.csv')

states = c("ME","NH","VT","MA", "RI", "CT", "NY", "PA", "NJ")
pfw = pfw[pfw$state %in% states,]
pfw = droplevels(pfw)


#### PREPARING DATA FOR MODELS ####

#----Convert PFW coordinates to laea for model training----
coords = cbind(pfw$long, pfw$lat)
sp = SpatialPoints(coords)
spdf = SpatialPointsDataFrame(coords=sp, data=pfw)
proj4string(spdf) = CRS("+init=epsg:4326")
spdf = spTransform(spdf, CRS("+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=40 +units=m"))
pfw = as.data.frame(spdf)
pfw$long = pfw$coords.x1
pfw$lat = pfw$coords.x2
pfw$coords.x2 = NULL
pfw$coords.x1 = NULL

#---Presence/Absence----
zeros = pfw[pfw$maxFlock < 1,]
ones = pfw[pfw$maxFlock > 0,]
zeros$pa = 0
ones$pa = 1
pfw=rbind(zeros,ones)

#----Clearing some space----
rm(coords, sp, spdf, states, zeros, ones)

#----Splitting into training and testing----
set.seed(4797) 
sample = sample.split(pfw$X, SplitRatio = .75)
train = subset(pfw, sample == TRUE)
test  = subset(pfw, sample == FALSE)


#### RUNNING MODELS #####

#----Run Random Forest regression model for presence/absence----
if(F){
  model <- pa ~ yr + lat + long + effortDays + effortHours  + AG + FOR + OPEN + URB + WAT + WET + SEA
  rf1 <- randomForest(model, data=train) # About 2 hours to run
}

#----Running binomial gam for presence/absence----
date()
{
  binomial.gam = gam(pa ~  
                       effortHours + effortDays +
                       s(AG, k=5) + s(OPEN, k=5) + s(URB, k=5) + 
                       s(WAT, k=5) + s(WET, k=5) + s(SEA, k=5) +
                       s(yr, k=11) + s(lat,long) + s(locID, bs="re"), 
                     family = binomial, data=train)
} 
date()

evaluate(test[test$pa != 0,], test[test$pa == 0,], binomial.gam)

#### GENERATING PREDICTION GRID ####

#----Generating prediction grid -- COORDINATES----
states.full = c("Maine", "New Hampshire", "Vermont", "Massachusetts",
                "Rhode Island", "Connecticut", "New York", "Pennsylvania",
                "New Jersey")


# load some spatial data. Administrative Boundary
us = raster::getData('GADM', country = 'US', level = 1)
st.contour <- us[us$NAME_1 %in% states.full,]

# check the CRS to know which map units are used
st.contour = spTransform(st.contour, CRS("+proj=laea +x_0=0 +y_0=0 +lon_0=-74 +lat_0=40 +units=m"))

# Create a grid of points within the bbox of the SpatialPolygonsDataFrame 
# nj with decimal degrees as map units
grid <- makegrid(st.contour, cellsize = 1000) # cellsize in map units! "(+units=)
# Right now this is twice the sampling distance.
# Check to make sure this agrees with the original extraction.

# grid is a data.frame. To change it to a spatial data set we have to
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(st.contour)))
date() ; grid <- grid[st.contour, ] ; date()


#### EXTRACTING P-LAND ####

#----Cropping raster to see if that helps----
grid.r = spTransform(grid, crs(nlcd))
grid.r@bbox = matrix(c(1550000.0, 2550000.0, -434099.8, 722809.3), 2,2, byrow=T)


nlcd.crop = crop(nlcd, grid.r)
nlcd.vx <- velox(stack(nlcd.crop))

#----Extracting raster values----
spol <- gBuffer(grid.r, width=500, byid=TRUE)
spdf <- SpatialPolygonsDataFrame(spol, data.frame(id=1:length(spol)), FALSE)
ex.mat <- nlcd.vx$extract(spdf)
print(ex.mat)

#----Calculating proportional landcover----
date()
if(T){
  if(exists("prop.lc.df")){rm(prop.lc.df)}
  prop.lc.df = data.frame(0:19)
  for(i in 1:length(ex.mat)){
    if(exists("empty.pr.lc")){rm(empty.pr.lc)}
    if(exists("lc.raw")){rm(lc.raw)}
    empty.pr.lc = data.frame(Var1=0:19, prop=rep(0,20))
    lc.raw = as.data.frame(table(unlist(ex.mat[[i]])))
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
View(prop.lc.df)


#----Combining the lclu classes product with original data----
LU = cbind(grid@coords, prop.lc.df)
LU = as.data.frame(LU)
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


#### FINAL ASSEMBLY OF PREDICTIONS DATA ####

#----Assembling predictions data frame----
traj.lclu = LU[,c(1:2,23:29)]
colnames(traj.lclu)[1:2] = c("long","lat")


st.pred = data.frame(
  locID = rep("L28176", length(grid@coords)/2),
  yr = rep(2004, length(grid@coords)/2),
  effortDays = rep(4, length(grid@coords)/2),
  effortHours = rep(4, length(grid@coords)/2)
)
st.pred = cbind(st.pred, traj.lclu)


#### GENERATING SPATIAL PREDICTIONS BY YEAR ####

#----Yearly predictions----
year = 2004
{
  #----Assigning Year----
  st.pred[,"yr"] = year
  
  #----Taking predcitions----
  pred.lclu<- stats::predict(
    binomial.gam,
    newdata = st.pred, type="response", interval= "confidence",
    se.fit= F, exclude="s(locID)") 
  pred_qp.after = data.frame(cbind(x = st.pred$long, y = st.pred$lat, z = pred.lclu))
}

year = 1999
{
  #----Assigning Year----
  st.pred[,"yr"] = year
  
  #----Taking predcitions----
  pred.lclu<- stats::predict(
    binomial.gam,
    newdata = st.pred, type="response", interval= "confidence",
    se.fit= F, exclude="s(locID)") 
  pred_qp.before = data.frame(cbind(x = st.pred$long, y = st.pred$lat, z = pred.lclu))
}


#### FINAL PRODUCT ####

#----Plotting spatial trend----
plot(rasterFromXYZ(pred_qp.after)-rasterFromXYZ(pred_qp.before), col=rev(colorRampPalette(c("gray90", "red"))(100)),
     frame.plot=F, box=F, axes=F, main="2004-1999")
