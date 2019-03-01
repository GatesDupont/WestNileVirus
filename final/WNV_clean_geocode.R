# Gates Dupont #
# 2019         #
################

#----Libraries----
library(rgdal)
library(proj4)
library(sp)
library(dplyr)
library(mgcv) 
library(ggplot2)
library(raster)
library(stringr)
library(rgeos)

#----Species input for files----
species = "blujay"

#----Setup----
setwd("~/WNV SP19")

#----Importing PFW data----
if(T){
  pfw.raw = read.csv(paste0("PFW_",species,".csv"))
  # rm(list=setdiff(ls(), "pfw.raw"))
}

#----Converting to duplicate data for preservation----
pfw.ne = pfw.raw

#----Importing geocode locations----
ref.locs = read.csv("pfwfinal00.csv")
for(i in 1:8){
  ref.locs = rbind(ref.locs, read.csv(paste0("pfwfinal0",as.character(i),".csv")))
} # 1+8 files

#----Removing postal code IDs----
ref.locs = droplevels(ref.locs[!(ref.locs$LOC_METHOD == "postal"),])

#----Selecting states----
states = c('ME', 'NH', 'VT', 'MA', 'RI', 'CT', 'NY', 'PA', 'NJ')
pfw.ne = pfw.ne[pfw.ne$StatProv %in% states,]

#----Preparing to merge with geocoded locs----
pfw.ne$season = as.integer(str_sub(pfw.ne$FW_Year, start= -4))
pfw.ne$lat = NA; pfw.ne$long = NA
plot(as.numeric(table(pfw.ne$FW_Year))[-1], type="l", ylim = c(0, 43000)) # check

#----Merging geocoded locations----
u.locs = ref.locs %>% 
  distinct(ID, FW_YEAR, .keep_all = T)
geo.pfw = merge.data.frame(x = pfw.ne, y = u.locs,
                           by.x = c("ID", "season") , by.y = c("ID", "FW_YEAR"),
                           all.x = T)
lines(as.numeric(table(geo.pfw$season)), col="red") # check

#----Keeping and combining good coords----
c.zip = str_detect(str_to_lower(geo.pfw$ENTRY_TECHNIQUE), "post")==F # entry technique didnt use zipcode
c.coord = !is.na(geo.pfw$LATITUDE.y) # coordinates aren't missing
c.9699 = geo.pfw$season %in% c(1996:1999) # years 1996-1999
c.0008 = geo.pfw$season %in% c(2000:2008) # years 2000-2008
c.0918 = geo.pfw$season %in% c(2009:2018) # years 2009-2018

# Keep rows if...
c1 = c.9699 # the season is between 1996 and 1999
c2 = c.0008 & c.coord # the season is between 2000 and 2008
                              # without new coordinates missing
c3 = c.0918 & c.zip # the season was after 2008
                    # and the location wasn't submitted by zipcode

geo.pfw = geo.pfw[(c1 | c2 | c3),]
lines(as.numeric(table(geo.pfw$season)), col="black") # check

# Adding good locs to new lat/long columns
geo.pfw$LATITUDE.y[is.na(geo.pfw$LATITUDE.y)] = geo.pfw$LATITUDE.x[is.na(geo.pfw$LATITUDE.y)]
geo.pfw$LONGITUDE.y[is.na(geo.pfw$LONGITUDE.y)] = geo.pfw$LONGITUDE.x[is.na(geo.pfw$LONGITUDE.y)]

#----Formatting effort----
pfw.ne = geo.pfw
pfw.ne[pfw.ne==""] = NA # blank to NA
pfw.ne = pfw.ne %>%
  filter(complete.cases(.[,c(1:2, 12:16, 26, 29:30, 42:43)])) %>% # remove NAs 1, 36, 25, 43:44, 15, 29
  mutate(effortDaysNumerical = rowSums(.[,12:15])) %>% # Summing half days
  mutate(obshrs=
           recode(EFFORT_HRS_ATLEAST,
                  "0.001"=1,"1"=2,"4"=3,"4.001"=3,"8.001"=4)) %>% # recoding effort
  dplyr::select(lat=42, long=43, yr=2, day=29, maxFlock=26,
                effortHours=47, effortDays=46,locID=1) %>% # selecting relevant data
  droplevels() # check this

dfEffHD=droplevels(pfw.ne)

#----Keeping only points from within region of interest----

# Generating polygon
states.full = c("Maine", "New Hampshire", "Vermont", "Massachusetts",
                "Rhode Island", "Connecticut", "New York", "Pennsylvania",
                "New Jersey")

# Load state polygons
us = raster::getData('GADM', country = 'US', level = 1) # get admin. boundaries
st.contour = us[us$NAME_1 %in% states.full,] # keep only predefined states

# Extending buffer to preserve coastal locs
st.contour = spTransform(x=st.contour, CRSobj=CRS("+init=epsg:32662"))
st.contour.buffer = gBuffer(st.contour, width = 10000) 
st.contour = spTransform(st.contour.buffer, CRSobj=CRS("+init=epsg:4326"))

# Converting pfw data to sp
coordinates(dfEffHD)= ~long+lat # converting from f to spdf
crs(dfEffHD) = CRS("+init=epsg:4326") # adding crs format

# Selecting only points in the polygon (this takes a while)
dfEffHD = dfEffHD[st.contour,] 
dfEffHD = as.data.frame(dfEffHD) # convert back to df from spdf

#----Selecting power users----
yearly_list_count = table(dfEffHD$locID, dfEffHD$yr) # creates a table showing number of observations at each location each year.
row_sums = rowSums(yearly_list_count >= 10) # rows where there are at least 10 observations
threeyears = which(row_sums >=3) # for rows with 10 obs over at least 3 years
newIDs = names(threeyears) # just setting a new variable name
# Final product:
df = dfEffHD[dfEffHD$locID %in% newIDs,] 

#----Exporting file----
write.csv(df, paste0("PFW_", species, "_clean_geocode.csv"))
