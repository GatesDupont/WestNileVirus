# Gates Dupont #
# Sept 2018    #
# # # # # # # #

library(glmmADMB)
library(qpcR)

#----Loading the pfw data----
pfw = read.csv('~/Desktop/WNV2/PFW_amecro_zerofill_landcover.csv')
pfw = pfw[pfw$state=="MA",]

years = 1999:2004
pfw = pfw[pfw$yr %in% years,]

#----Converting habitat to binary for urban/not-urban----
# Making a new column in the df
pfw$urban = pfw$URB

# Separating by urban value
zeros = pfw[pfw$urban <= 0.4,]
ones = pfw[pfw$urban >= 0.6,]

#----Running a glm for NON-URBAN----
glm.nU = glmmadmb(maxFlock~effortDays+effortHours+lat+long+yr+(1|locID), data=zeros,
                  zeroInflation = T, family="nbinom")

#----Running a glm for URBAN----
glm.U = glmmadmb(maxFlock~effortDays+effortHours+lat+long+yr+(1|locID), data=ones,
                 zeroInflation = T, family="nbinom")

#----Exploring models----
summary(glm.nU) # 1.1% decline by yr, sig p val ||| 1.9% decline with re
summary(glm.U) # 3.6% decline by yr, sig p val ||| 2.6% decline with re
