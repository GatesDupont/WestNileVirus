# Gates Dupont #
# Sept 2018    #
# # # # # # # #

library(glmmADMB)
library(qpcR)
library(ggplot2)
library(dplyr)

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

#----NON-URBAN model prediction----
# Create a testing set
testing.nU = data.frame(
    long = mean(pfw$long),
    lat = mean(pfw$lat),
    locID = "L28176",
    yr = years,
    effortDays = 4,
    effortHours = 4)

# Calling predict
predictions.nU = predict(
  glm.nU,
  newdata = testing.nU, interval= "confidence", exclude=c("locID")) 
predictions.nU = data.frame(Year = years, Fit = predictions.nU$fit, Habitat = "Non-urban")

#----URBAN model prediction----
# Create a testing set
testing.U = data.frame(
  long = mean(pfw$long),
  lat = mean(pfw$lat),
  locID = "L28176",
  yr = years,
  effortDays = 4,
  effortHours = 4)

# Calling predict
predictions.U = predict(
  glm.U,
  newdata = testing.U, interval= "confidence", exclude=c("locID"))
predictions.U = data.frame(Year = years, Fit = predictions.U$fit, Habitat="Urban")

#----Aggregating predictions----
predictions = rbind(predictions.nU, predictions.U)

#----Plotting linear models----
#tiff("~/Desktop/lm_urban_AMCR_MA.tiff", width = 6, height = 5, units = 'in', res = 300, compression = 'rle')
predictions %>%
  ggplot(aes(x=Year, y=Fit, colour=Habitat)) +
  geom_line(size=1.1) + ylim(0,1) +
  scale_color_manual(values=c("gray35", "darkorange2")) +
  theme_light() + labs(title = "Abundance Trends", 
                       subtitle="American Crow (Corvus brachyrhynchos), Massachusetts") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) +
  geom_text(x = 2002.5, y=0.55, colour = "gray35", angle = -10,
            label = paste(" \u03b2 = -0.04, p = 0.013 *")) +
  geom_text(x = 2002.5, y=0.34, colour = "darkorange2", angle=-23.5,
            label = paste(" \u03b2 = -0.09, p = 4.9e-07 ***"))
#dev.off()
