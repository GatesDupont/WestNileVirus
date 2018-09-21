# Gates DUpont #
# 2018         #
# # # # # # # #

library(sp)
library(rgdal)
library(raster)
library(maps)
library(mgcv)


#----Preparing the PFW data----

pfw = read.csv('~/WNV F18/PFW_amecro_zerofill_landcover.csv')

states = c("ME","NH","VT","MA", "RI", "CT", "NY", "PA", "NJ")
pfw = pfw[pfw$state %in% states,]
pfw = droplevels(pfw)


#----Generating the grid----

states.full = c("Maine", "New Hampshire", "Vermont", "Massachusetts",
                "Rhode Island", "Connecticut", "New York", "Pennsylvania",
                "New Jersey")

# load some spatial data. Administrative Boundary
us <- getData('GADM', country = 'US', level = 1)
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
grid <- grid[st.contour, ]

#----Convert PFW coordinates to laea----
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

#----Run GAM model for abundance----
date()
if(T){
  st.gam = gam(list(
    maxFlock ~ 
      s(yr, k=11) +
      s(locID, bs="re") +
      s(lat,long, k=5),
    ~
      effortHours +
      effortDays +
      s(AG, k=5) + s(FOR, k=5) + 
      s(OPEN, k=5) + s(URB, k=5) + 
      s(WAT, k=5) + s(WET, k=5) + 
      s(SEA, k=5) +
      s(yr, k=11) +
      s(locID, bs="re") +
      s(lat,long, k=5)),
    family = ziplss,
    gamma = 1.4,
    data = pfw) # k=6
}
date()

#----Generate prediction data frame----

#----predict() for 2004----

#----predict() for 1999----

#----Convert to rasters and subtract----

#----Plot spatial trend----
