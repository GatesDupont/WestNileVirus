# Gates Dupont #
# 2018         #

library(glmmADMB)
library(glmmTMB)
library(lme4)
library(qpcR)

#----Loading data----
if(T){
  pfw=read.csv("~/Desktop/WNV2/PFW_amecro_zerofill_landcover.csv")
  pfwct=pfw[pfw$state == "NJ",]
}

#----GLM: Test Poisson vs Negative Binomial----
glm.pois = glm(maxFlock~effortDays+effortHours+lat+long+yr, data=pfwct, family="poisson")
glm.nb = glm.nb(maxFlock~effortDays+effortHours+lat+long+yr, data=pfwct)
AIC(glm.pois, glm.nb) # Negative binomial fits better

#----GLM: Test Zero-inflation----
glmmTMB.nb2.re = glmmTMB(maxFlock~effortDays+effortHours+lat+long+yr+(1|locID), data=pfwct, 
                            ziformula=~0, family=list(family="nbinom2",link="log"))
glmmTMB.zi.nb2.re = glmmTMB(maxFlock~effortDays+effortHours+lat+long+yr+(1|locID), data=pfwct, 
                            ziformula=~1, family=list(family="nbinom2",link="log"))
AIC(glmmTMB.nb2.re, glmmTMB.zi.nb2.re) # Zero-inflated performs better

#----GLM Full: Test ADMB vs TMB----
glmmadmb.zi.nb.re = glmmadmb(maxFlock~effortDays+effortHours+lat+long+yr+(1|locID), data=pfwct,
                          zeroInflation = T, family="nbinom") # Lower AIC score than glmmTMB (dAIC = 98.68)
if(F){
  glmmTMB.zi.nb2.re = glmmTMB(maxFlock~effortDays+effortHours+lat+long+yr+(1|locID), data=pfwct, 
                              ziformula=~1, family=list(family="nbinom2",link="log")) # glmmTMB is the more up-to-date package
} # Already run above.
AIC(glmmadmb.zi.nb.re, glmmTMB.zi.nb2.re)

#----GLM: Test Random Effects----
AIC(glm.nb, glmmTMB.nb2.re)

#----Full model comparison table----
models.aic = AIC(glm.pois, glm.nb, glmmTMB.nb2.re, glmmTMB.zi.nb2.re, glmmadmb.zi.nb.re)
models.weights = akaike.weights(models.aic$AIC)
models.aictab = cbind(models.aic,models.weights)
models.aictab=models.aictab[order(models.aictab["deltaAIC"]),]
View(models.aictab)

# Top model == Zero-inflated negative binomial model with random effects
#       From glmmADMB, although glmmTMB is more up to date.
