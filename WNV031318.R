#----Libraries----
if(T){
  library(rgdal)
  library(proj4)
  library(sp)
  library(raster)
  library(dplyr)
  library(RColorBrewer)
  library(classInt)
  library(mgcv) 
  library(gamm4)
  library(lme4)
  library(predictmeans)
  library(ggplot2)
}

#----Importing PFW data----
if(F){
  raw.pfw = read.csv("C:/Users/itloaner/Desktop/WNV/UpdatedData/PFW_amecro_zerofill.csv") # BLJA and AMCR the same?
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
  rawData$obshrs[idx] <- "D"
  idx<- rawData$EFFORT_HRS_ATLEAST == "1"
  rawData$obshrs[idx] <- "C"
  idx<- rawData$EFFORT_HRS_ATLEAST == "4"
  rawData$obshrs[idx] <- "B"
  idx<- rawData$EFFORT_HRS_ATLEAST == "4.001"
  rawData$obshrs[idx] <- "B"
  idx<- rawData$EFFORT_HRS_ATLEAST == "8.001"
  rawData$obshrs[idx] <- "A"
}

#----Relevant data only----
if(T){
  locID = rawData$LOC_ID
  yr = rawData$FW_Year
  maxFlock = rawData$nseen
  lat = rawData$LATITUDE
  long = rawData$LONGITUDE
  effortHours = rawData$obshrs # empty = x, 0.001 = D, 1 = C, 4 = B, 8+ = A
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

#----Removing empty effort and high counts----
if(T){
  dfRhigh = subset(dfR95, maxFlock < 49) #exclude high counts (over 50 birds)
  dfEffRx = subset(dfRhigh, effortHours != "x") # excluding blank data
  dfDaysR0 = subset(dfEffRx, effortDays != 0) # excluding blank data
}

#----Caclulate percent removed by exluding high counts----
if(F){
  100*(1-(length(dfRhigh$maxFlock))/(length(dfR95$maxFlock)))
}

#----Selecting power users----
if(T){
  ###Cleaning up the data with conditionals###
  # 1) Only include LocIDs active for at least 3 years ###
  # 2) Only include LocIDs with at least 10 checklists during those years ###
  yearly_list_count = table(dfDaysR0$locID, dfDaysR0$yr) # creates a table showing number of observations at each location each year.
  row_sums = rowSums(yearly_list_count >= 10) # rows where there are at least 10 observations
  threeyears = which(row_sums >=3) # for rows with 10 obs over at least 3 years
  newIDs = names(threeyears) # just setting a new variable name
  #Final product:
  dfPwrUsrs = dfDaysR0[dfDaysR0$locID %in% newIDs,] 
}

#----Renaming main dataframe----
if(T){
  df4Uniq = data.frame(dfPwrUsrs$locID, dfPwrUsrs$yr, dfPwrUsrs$lat, dfPwrUsrs$long, dfPwrUsrs$effortDays, dfPwrUsrs$effortHours, dfPwrUsrs$maxFlock) #this makes a new df so I can effectively use unique()
  pfw = unique(df4Uniq)
  names(pfw)[1] = 'locID'
  names(pfw)[2] = 'yr'
  names(pfw)[3] = 'lat'
  names(pfw)[4] = 'long'
  names(pfw)[5] = 'effortDays'
  names(pfw)[6] = 'effortHours'
  names(pfw)[7] = 'maxFlock'
}

#----Duplicate dataframe to conserve data, remove later---- 
if(T){
  toSPDF = pfw
}

#----Spatially formatting main dataframe----
if(T){
  # Tom Auer CRS"  +init=epsg:4326
  xy <- toSPDF[,c(3,4)]
  SPDF <- SpatialPointsDataFrame(coords = toSPDF[,c("long", "lat")], data = toSPDF,
                                 proj4string = CRS("+init=epsg:4326")) 
}

#----Importing BCR shape file, no state borders----
if(T){
  shp = shapefile("C:/Users/itloaner/Desktop/BCR/BCR_Terrestrial_master_International.shx")
  BCRs = spTransform(shp, CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

#----Impoting BCR shapefile with states----
if(T){
  stateshp = shapefile("C:/Users/itloaner/Desktop/BCR/BCR_Terrestrial_master.shx")
  StateProv = spTransform(stateshp, CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

#----Checking for same projections (ask about this)----
if(T){
  isTRUE(proj4string(BCRs) == proj4string(SPDF))
}

#----Overlay: spatially joining attributes (BCR & pts) by location----
if(T){
  unattachedBCR <- over(SPDF, BCRs[,"BCRNAME"])
  SPDF$BCR <- NA # This is to avoid replacement row length issue (+/-1)
  SPDF$BCR <- unattachedBCR$BCRNAME
}

#----Convert from spdf to dataframe with BCRs----
if(T){
  dfWithBCRS = as.data.frame(SPDF)
}

#----Assigning Hawaii and BadBCRs----
if(T){
  idx<- dfWithBCRS$BCR == "NOT RATED"
  dfWithBCRS$BCR[idx] <- "HAWAII"
  
  idx<- is.na(dfWithBCRS$BCR)
  dfWithBCRS$BCR[idx] <- "BadBCR"
}

#----Plotting the Bad BCRs----
if(F){
  # dfNAs = dfWithBCRS[dfWithBCRS$BCR == "BadBCR",] # Accounts for about 0.6 % of the data
  # qplot(
  #   y = dfNAs$lat,
  #   x = dfNAs$long,
  #   data = dfNAs,
  #   color = dfNAs$BCR
  # )
  # 
  # # Plotting all BCR data
  # qplot(
  #   y = dfWithBCRS$lat,
  #   x = dfWithBCRS$long,
  #   data = dfWithBCRS,
  #   color = dfWithBCRS$BCR
  # )
}

#----Removing pts with BadBCRs----
if(T){
  dfCleanBCR = dfWithBCRS[dfWithBCRS$BCR != "BadBCR",] # Removing the points that are plotted too close to coastline.
  dfCheckN = dfCleanBCR[dfCleanBCR$maxFlock > 0, ]
}

#----Removing data versions----
if(T){
  rm(dfCleanBCR, dfWithBCRS, unattachedBCR, 
     SPDF, stateshp,StateProv,xy,toSPDF,pfw,df4Uniq,
     rawData, dfEffHD,dfR91,dfR92,dfR93,dfR94,dfR95)
}

#----Dataframe for model testing - select BCRs----
if(T){
  #bcr.regions = dfCheckN[dfCheckN$BCR == "NEW ENGLAND/MID-ATLANTIC COAST"|dfCheckN$BCR == "PIEDMONT"|dfCheckN$BCR == "SOUTHEASTERN COASTAL PLAIN"|dfCheckN$BCR == "ATLANTIC NORTHERN FOREST"|dfCheckN$BCR == "APPALACHIAN MOUNTAINS"|dfCheckN$BCR == "LOWER GREAT LAKES/ ST. LAWRENCE PLAIN",]
  bcr.regions = dfCheckN[dfCheckN$BCR == "NEW ENGLAND/MID-ATLANTIC COAST",]
  regionData = bcr.regions
  #unique(regionData$BCR)
  regionData$log.maxFlock = log(regionData$maxFlock)
  
  
  qplot(regionData$lat, regionData$long, data = regionData, color = regionData$maxFlock, size = regionData$maxFlock)
}

#----Interval plot color pallette----
if(F){
  pal = brewer.pal(5, "Reds")
  q5 = classIntervals(regionData$maxFlock, n=5, style = "quantile")
  q5Colours = findColours(q5,pal)
  plot(regionData$lat, regionData$long, col = q5Colours, pch = 19, axes = T, cex = 0.3, main = "maxFlock")
  legend("topleft", fill = attr(q5Colours, "palette"),
         legend = names(attr(q5Colours, "table")),bty = "n")
}

#----Plotting pfw data by lat/lon----
if(F){
  plot(maxFlock~lat, data = regionData, main = "maxFlock by Lat")
  lines(supsmu(regionData$lat, newEngland$maxFlock),col=2,lwd=2)
  
  plot(maxFlock~long, data = regionData, main = "maxFlock by Lat")
  lines(supsmu(regionData$long, newEngland$maxFlock),col=2,lwd=2)
}

#----First GAM----
if(F){
  maxFlock.gam = gam(maxFlock~s(lat,long),data = regionData)
  summary(maxFlock.gam)
}

#----Deviance smoothing----
if(F){
  dev.rss = c()
  kval = c()
  for(i in seq(10,130, by = 10)){
    dev.rss = c(dev.rss, deviance(gam(maxFlock~s(lat,long,k=i), data=regionData)))
    kval = c(kval, i)
  }
  plot(kval, dev.rss, xlab = "Parameters", ylab = "Deviance (RSS)", pch=15, main = "Smoothing Parameter Guide")
}

#----AIC smoothing----
if(F){
  dev.rss = c()
  kval = c()
  for(i in seq(10,130, by = 10)){
    dev.rss = c(dev.rss, AIC(gam(maxFlock~s(lat,long,k=i), data=regionData)))
    kval = c(kval, i)
  }
  plot(kval, dev.rss, xlab = "Parameters", ylab = "AIC", pch=15, main = "Smoothing Parameter Guide")
}

#----GAM k=120----
if(F){
  xy.maxFlock.gam = gam(log.maxFlock ~ s(lat,long ,k=120) + effortDays + effortHours, data = regionData)
  xy.maxFlock.pred = predict(xy.maxFlock.gam,se.fit=T)
  summary(xy.maxFlock.gam)
}

#----GAM k=120 predictions----
if(F){
  maxFlock.120.gam.pred = data.frame(
    x = regionData$lat,
    y = regionData$long,
    pred = fitted(xy.maxFlock.gam))
  head(maxFlock.120.gam.pred)
  coordinates(maxFlock.120.gam.pred) = c("x","y")
}

#----GAM k=120 pred. plot----
if(F){
  pal = brewer.pal(5,"Reds")
  q5 = classIntervals(maxFlock.120.gam.pred$pred, n=5, style = "quantile")
  q5Colours = findColours(q5, pal)
  plot(maxFlock.120.gam.pred, col=q5Colours,pch=19,cex=0.7,axes=T,main="GAM k=120")
  legend("topleft", fill=attr(q5Colours, "palette"),
         legend = names(attr(q5Colours,"table")),cex=0.7,bty="n")
}

#----Kriging----
if(F){
  library(gstat)
  
  xy2 <- regionData[,c(3,4)]
  regionData.spdf <- SpatialPointsDataFrame(coords = regionData[,c("long", "lat")], data = regionData,
                                 proj4string = CRS("+init=epsg:4326")) 
  
  logMf.vario = variogram(log.maxFlock~1,regionData.spdf)
  plot(logMf.vario, pch=20,col=1,cex=2)
  logMf.fit = fit.variogram(logMf.vario,
                            vgm(psill=10,"Sph",range=1.0,nugget=2))
  plot(logMf.vario,logMf.fit,pch=20,col=2,cex=2,lwd=3,main="Log maxFlock Variogram")
  #... see HW5 6700
}

#----lm model selection----
if(F){
  lm1 = lm(maxFlock~yr+lat+long+effortDays+effortHours, data=regionData)
  lm2 = lm(maxFlock~yr+lat*long+effortDays+effortHours, data=regionData)
  lm3 = lm(maxFlock~yr+lat+long+effortHours, data=regionData)
  lm4 = lm(maxFlock~yr+lat+long+effortDays, data=regionData)
  lm5 = lm(maxFlock~yr+lat+effortDays+effortHours, data=regionData)
  lm6 = lm(maxFlock~yr+long+effortDays+effortHours, data=regionData)
  lm7 = lm(maxFlock~lat+long+effortDays+effortHours, data=regionData)
  lm8 = lm(maxFlock~yr+lat+long+effortDays*effortHours, data=regionData)
  lm9 = lm(maxFlock~yr+lat+long+effortDays*effortHours, data=regionData)
  mdls = AIC(lm1,lm3,lm4,lm5,lm6,lm7)
  (best = mdls[mdls$AIC == min(mdls$AIC),])
  mdls = AIC(lm1,lm2,lm8,lm9) # Adding the model with the interaction term
  (best = mdls[mdls$AIC == min(mdls$AIC),])
}

#----lm and predictions----
if(F){
  pfw.lm = lm(maxFlock~yr+lat*long+effortDays+effortHours, data=regionData)
  summary(pfw.lm)
  
  # Predicted values by year
  pfw.lm.pred = predictmeans(model = pfw.lm, modelterm = "yr", plot = F, newwd = F)
  
  # Predicted means and standard error
  pfw.lm.pred.pmeans = as.data.frame(pfw.lm.pred$`Predicted Means`)
  pfw.lm.pred.pse = as.data.frame(pfw.lm.pred$`Standard Error of Means`)
  
  # Pulling output
  pfw.lm.pred.yr = as.character(pfw.lm.pred.pmeans$yr)
  pfw.lm.pred.means = pfw.lm.pred.pmeans$Freq
  pfw.lm.pred.se = pfw.lm.pred.pse$Freq
  pfw.lm.pred.df = data.frame(pfw.lm.pred.yr, pfw.lm.pred.means, pfw.lm.pred.se)
}

#----Plotting predicted means from lm----
if(F){
  pfw.lm.pred.df$pfw.lm.pred.yr = as.numeric(gsub(".*_","",pfw.lm.pred.df$pfw.lm.pred.yr))
  ggplot(pfw.lm.pred.df, aes(factor(pfw.lm.pred.yr), pfw.lm.pred.means)) +
    geom_point(color = 'red') +
    geom_errorbar(aes(ymin = pfw.lm.pred.means - pfw.lm.pred.se, 
                      ymax = pfw.lm.pred.means + pfw.lm.pred.se)) +
    geom_line(aes(x=factor(pfw.lm.pred.yr), y=pfw.lm.pred.means, group=1), 
              linetype='dotted') +
    ggtitle('New England/Mid-Atlantic Coasts') + theme(axis.title.x = element_text(color = 'blue'), axis.title.y = element_text(color = 'blue')) +
    labs(x = 'Project FeederWatch', y = 'Maximum Flock') +
    theme(panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(colour = 'aliceblue'),
          panel.grid.minor = element_line(colour = 'white'),
          plot.title = element_text(face = 'bold', hjust = 0.5, family = 'sans'))
}

#----Plotting BCR data----
if(F){
  qplot(
    y = dfCheckN$lat,
    x = dfCheckN$long,
    data = dfCheckN,
    color = dfCheckN$BCR
  ) + labs(x = "Longitude", y = "Latitude", color = "Legend")
}

#----Cropping to show miss-IDs----
if(F){
  e = extent(-126, -114, 32, 42.5)
  cp = crop(SPDF[SPDF@data$maxFlock>0,], e)
  cs = crop(StateProv, e)
  plot(cs)
  points(cp, col = "blue", pch = 20)
}

#----Plotting regions bbox----
if(T){
  library(ggplot2)
  regionSPDF = regionData
  coordinates(regionSPDF) = c("lat","long")
  bbox = data.frame(bbox(regionSPDF))
  box = data.frame(maxlat = bbox$max[1], minlat = bbox$min[1], maxlong = bbox$max[2], minlong = bbox$min[2], id="1")
  fortBCR = fortify(BCRs)
  ggplot() +
    geom_polygon(data=fortBCR, aes(x=long, y=lat, group=group), color="black", fill="white") +
    geom_rect(data = box, aes(xmin=minlong, xmax = maxlong, ymin=minlat, ymax=maxlat), color="red", fill="transparent")
}

#----spplot of data----
if(T){
  spplot(regionSPDF, zcol="maxFlock", colorkey=T,cex=2*regionSPDF$maxFlock/max(regionSPDF$maxFlock))
}

#----Exploring data by variable----
if(F){
  # Days
  ggplot() +
    geom_point(aes(x=effortDays,y=maxFlock),data=regionData)
  # Hours
  ggplot() +
    geom_point(aes(x=effortHours,y=maxFlock),data=regionData)
  #Year
  ggplot() +
    geom_point(aes(x=yr,y=maxFlock),data=regionData)
  # Latitude
  ggplot() +
    geom_point(aes(x=lat.1,y=maxFlock),data=regionData)
  # Longitude
  ggplot() +
    geom_point(aes(x=long.1,y=maxFlock),data=regionData@data)
}

#----OLS regression----
if(F){
  rownames(regionData) = NULL
  #m.ols = lm()
}

#----Data exploration w Regression----
if(F){
  lm = lm(log.maxFlock~yr+lat+long+effortDays+effortHours, data=regionData)
  summary(lm)
  
  par(mfrow = c(1,2))
  plot(lm, which = c(1,2))
  par(mfrow = c(1,1))
}

#----LAND USE----
if(F){
  
  # Check working directory
  
  #----Importing LCLU data as raster----
  file_name='Downloads/na_landcover_2010_30m/na_landcover_2010_30m/NA_NALCMS_LC_30m_LAEA_mmu12_urb05/NA_NALCMS_LC_30m_LAEA_mmu12_urb05.tif' 
  nlcd=raster(file_name)
  
  #----Importing nlcd legend metdata----
  legend = read.csv("nlcd_2010_30m_metadata_legend.csv")
  
  # Switching latlon to lonlat
  o <- c(4,3,1,2,5:(length(colnames(regionData))))
  test0 = regionData[,o]
  #test = test0[1:3,]
  test = test0
  
  coordinates(test) = c("long", "lat")
  
  proj4string(test) = CRS('+proj=longlat +datum=WGS84')
  
  # Transform CRS of points to match that of NLCD
  tp = spTransform(test, CRS(proj4string(nlcd))) # Do these need to be switched
  
  #----Extracting the land cover classes----
  date()
  lclu.ext = extract(nlcd, tp, buffer = 1000)
  date()
  #View(lclu.ext)
  
  #----Calculating proportions----
  lclu.classes = sapply(lclu.ext, function(x) tabulate(x, 19))
  lclu.classes = 100 * (lclu.classes / colSums(lclu.classes))
  
  #----Flipping classes from rows to columns----
  transpose.step = as.data.frame(t(lclu.classes))
  names(transpose.step)[1:19] = as.character(c(1:19))
  colnames(transpose.step) = as.character(legend$ClassType)
  
  #----Combining the lclu classes product with original data----
  final = cbind(test@data,transpose.step)
  View(final)
} # This works, but will take 7 hrs to run and cause R to abort


