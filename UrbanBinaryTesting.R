# Gates Dupont #
# Sept 2018    #
# # # # # # # #

library(glmmADMB)
library(qpcR)

#----Loading the pfw data----
pfw = read.csv('~/Desktop/WNV2/PFW_amecro_zerofill_landcover.csv')
#pfw = pfw[pfw$state=="MA",]

#----Converting habitat to binary for urban/not-urban----
# Making a new column in the df
pfw$urban = pfw$URB

# Separating by urban value
zeros = pfw[pfw$urban < 0.5,]
ones = pfw[pfw$urban >= 0.5,]

# Transforming to binry
zeros$urban = 0
ones$urban = 1

# Recombining the data
pfw = rbind(zeros, ones)

#----Running a glm----

# No random effects to start.
glm.zi.nb.add = glmmadmb(maxFlock~effortDays+effortHours+lat+long+yr+urban, data=pfw,
                         zeroInflation = T, family="nbinom")
glm.zi.nb.col = glmmadmb(maxFlock~effortDays+effortHours+lat+long+yr:urban, data=pfw,
                         zeroInflation = T, family="nbinom")
glm.zi.nb.ast = glmmadmb(maxFlock~effortDays+effortHours+lat+long+yr*urban, data=pfw,
                         zeroInflation = T, family="nbinom")

# Model summary
summary(glm.zi.nb.add)
summary(glm.zi.nb.col)
summary(glm.zi.nb.ast)

#----Full model comparison table----
models.aic = AIC(glm.zi.nb.add, glm.zi.nb.col, glm.zi.nb.ast)
models.weights = akaike.weights(models.aic$AIC)
models.aictab = cbind(models.aic,models.weights)
models.aictab=models.aictab[order(models.aictab["deltaAIC"]),]
View(models.aictab)

#----Generating model predictions from the top model----
# Create a testing set
testing = rbind(
  data.frame(
    long = mean(pfw$long),
    lat = mean(pfw$lat),
    locID = "L28176",
    yr = 1996:2017,
    effortDays = 4,
    effortHours = 4,
    urban = 0
  ),
  data.frame(
    long = mean(pfw$long),
    lat = mean(pfw$lat),
    locID = "L28176",
    yr = 1996:2017,
    effortDays = 4,
    effortHours = 4,
    urban = 1
  )
)

# Calling predict
predictions = predict(
  glm.zi.nb.ast,
  newdata = testing, interval= "confidence", se.fit = TRUE, 
  exclude=c("locID")) 

# Adding year and error columns
predictions = data.frame(fit = predictions$fit$fit, 
                         se.fit = predictions$se.fit,
                         year = c(1996:2017,1996:2017))

# Separating into urban (1) and non-ubran (0)
predictions0 = predictions[1:22,]
predictions1 = predictions[23:44,]

error_top0 = predictions0$fit+1.96*predictions0$se.fit
error_bottom0 = predictions0$fit-1.96*predictions0$se.fit

error_top1 = predictions1$fit+1.96*predictions1$se.fit
error_bottom1 = predictions1$fit-1.96*predictions1$se.fit

#----Plotting predictions----
plot(predictions0$fit, type="l", lwd=1, col="red",
     main = "Corvus Population Trends in MA", xlab="Year", ylab="Predicted Abundance",
     frame=F, xaxt = "n", ylim=c(-40,40))
axis(side=1, at=c(1,5,10,15,20,22), labels=c(1996,2000,2005,2010,2015,2017))
#lines(error_top0 ~ c(1:22),lwd=2, col="blue")
#lines(error_bottom0 ~ c(1:22),lwd=2, col="blue")
polygon(c(1:22,rev(1:22)),
        c(error_top0,rev(error_bottom0)),
        col=rgb(1,0,0, 0.2), border=NA)
lines(predictions1$fit, lwd=3, col="blue")
polygon(c(1:22,rev(1:22)),
        c(error_top1,rev(error_bottom1)),
        col=rgb(0,0,1, 0.2), border=NA)
legend("topright", legend=c("Urban", "Rural"), lty=1, lwd=2, col=c("red","blue"))
