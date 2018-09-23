# Gates Dupont #
# Sept 2018    #
# # # # # # # #

library(glmmADMB)
library(qpcR)
library(ggplot2)
library(dplyr)

#----Preparing the pfw data----
# Load data
pfw = read.csv('~/Desktop/WNV2/PFW_amecro_zerofill_landcover.csv')

# Subsetting for states
states = c("NY", "PA", "NJ", "CT", "RI", "MA")
pfw = pfw[pfw$state %in% states,]

# Subsetting for years
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
summary(glm.nU) # beta and p vals
summary(glm.U)

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

# Percent decline
Decline = c()
for(i in 1:length(predictions.nU$Fit)){
  Decline = c(Decline, predictions.nU$Fit[i]/predictions.nU$Fit[1])
}
predictions.nU$Decline = -100*(1-Decline)

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

# Percent decline
Decline = c()
for(i in 1:length(predictions.U$Fit)){
  Decline = c(Decline, predictions.U$Fit[i]/predictions.U$Fit[1])
}
predictions.U$Decline = -100*(1-Decline)

#----Aggregating predictions----
predictions = rbind(predictions.nU, predictions.U)

#----Plotting linear models----
#tiff("~/Desktop/lm_urban_AMCR_Northeast_FullFrame.tiff", width = 6, height = 5, units = 'in', res = 300, compression = 'rle')
predictions %>%
  ggplot(aes(x=Year, y=Fit, colour=Habitat)) +
  geom_line(size=1.1) + ylim(0,0.6) +
  scale_color_manual(values=c("gray35", "darkorange2")) +
  theme_light() + labs(title = "Abundance Trends", 
                       subtitle="American Crow (Corvus brachyrhynchos), Northeastern U.S.") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) +
  geom_text(x = 2001.5, y=0.15, colour = "gray35", angle = -12,
            label = paste(" \u03b2 = -0.03, p = 1.2e-4 ***")) +
  geom_text(x = 2001.5, y=0.345, colour = "darkorange2", angle=-41,
            label = paste(" \u03b2 = -0.11, p = 2e-16 ***"))
#dev.off()

#----Plotting Linear Models Percent Decline----
#tiff("~/Desktop/lm_urban_AMCR_Northeast_Percent", width = 6, height = 5, units = 'in', res = 300, compression = 'rle')
predictions %>%
  ggplot(aes(x=Year, y=Decline, colour=Habitat)) +
  geom_line(size=1.1) + ylim(-100,0) +
  scale_color_manual(values=c("gray35", "darkorange2")) +
  theme_light() + labs(title = "Percent Abundance Trends", 
                       subtitle="American Crow (Corvus brachyrhynchos), Northeastern U.S.") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "italic")) +
  geom_text(x = 2002.2, y=-32.5, colour = "gray35", angle = -28.5,
            label = paste(" \u03b2 = -0.03, p = 1.2e-4 ***")) +
  geom_text(x = 2001.5, y=-52, colour = "darkorange2", angle=-42,
            label = paste(" \u03b2 = -0.11, p = 2e-16 ***"))
#dev.off()
