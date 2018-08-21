# Gates Dupont                    #
# 2018                            #
# Adapted from Crase et al., 2012 #
# # # # # # # # # # # # # # # # # #

# http://www.ecography.org/sites/ecography.org/files/appendix/e7138.pdf

#----Loading libraries----
library(raster) # for focal calculations
library(glmmADMB) # zero-inflated, mixed effects generalized linear model.
library(qpcR) # for AIC model comparison

#----Loading and prepping PFW data----
pfw = read.csv("~/Desktop/WNV2/PFW_amecro_zerofill_landcover.csv")
pfwST = pfw[pfw$state == "MA",]
xy = cbind(pfwST$long, pfwST$lat) # extract xy coords from data

#----Generating an empty raster----
rast_ac = raster(ncol=min(pfwST$lat)+max(pfwST$lat), nrow = min(pfwST$long)+max(pfwST$long), 
                 ymn = min(pfwST$lat), ymx = max(pfwST$lat), xmn = min(pfwST$long), xmx = max(pfwST$long)) # extent of region of interest
res(rast_ac) = 0.025

#----Fill new raster with bird counts----
rast_ac[cellFromXY(rast_ac, xy)] = pfwST[,8]

#----Check raster and points----
plot(rast_ac)
points(xy, col="black", pch=20, cex=0.2)

#----Calculate the autocovariate via a focal operation----
focal_response_rast = focal(rast_ac, w=matrix(1,3,3), fun=mean, na.rm=TRUE)
#focal_response_rast = focal(rast_ac, w = matrix(1/9, nc=3, nr=3), fun = mean, na.rm = TRUE)
plot(focal_response_rast)      
ac_vect = extract(focal_response_rast, xy) 
ac_vect[is.na(ac_vect)] <- 0
pfwST = cbind(pfwST, ac_vect)

#----Running models----
glmmadmb.zi.nb.re = glmmadmb(maxFlock~effortDays+effortHours+lat+long+yr+(1|locID), data=pfwST,
                             zeroInflation = T, family="nbinom")
glmmadmb.zi.nb.re.ac = glmmadmb(maxFlock~effortDays+effortHours+lat+long+yr+ac_vect+(1|locID), data=pfwST,
                             zeroInflation = T, family="nbinom")

#----Model comparison table----
models.aic = AIC(glmmadmb.zi.nb.re, glmmadmb.zi.nb.re.ac)
models.weights = akaike.weights(models.aic$AIC)
models.aictab = cbind(models.aic,models.weights)
models.aictab=models.aictab[order(models.aictab["deltaAIC"]),]
View(models.aictab)

