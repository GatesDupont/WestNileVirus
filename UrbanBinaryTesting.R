library(glmmADMB)


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
years = 2007:2014
pfw = pfw[pfw$yr %in% years,]

#----Generating presence/absence variable----
pfw$pa = 0L
presence = pfw[pfw$maxFlock > 0,]
absence = pfw[pfw$maxFlock == 0,]
presence$pa = 1
absence$pa = 0
pfw=rbind(presence, absence)

#----Converting habitat to binary for urban/not-urban----
# Making a new column in the df
pfw$urban = pfw$URB

# Separating by urban value
zeros = pfw[pfw$urban <= 0.4,]
ones = pfw[pfw$urban >= 0.6,]

#----Running a glm for NON-URBAN----
glm.nU = glmmadmb(pa~effortDays+effortHours+lat+long+yr+(1|locID), data=zeros,
                  zeroInflation = F, family="binomial")

#----Running a glm for URBAN----
glm.U = glmmadmb(pa~effortDays+effortHours+lat+long+yr+(1|locID), data=ones,
                 zeroInflation = F, family="binomial")

#----Plotting year coefficients----
df.nU = data.frame(confint(object = glm.nU, parm = "yr",level=0.95))
df.U = data.frame(confint(object = glm.U, parm = "yr",level=0.95))
df = rbind(df.nU, df.U)
df$group = c("Non-urban", "Urban")
df$coef = c(as.numeric(coef(glm.nU)["yr"]), as.numeric(coef(glm.U)["yr"]))
df = df[,c(1,4,2,3)]
colnames(df) = c("lower","coef","upper","group")

ggplot(df, aes(as.factor(group), coef)) + 
  theme_classic() +
  ggtitle("Comparison of Regression Coefficients",
          "American Crow percent reporting - Northeast U.S.") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face="italic")) +
  labs(y = "Coefficient for Year", x = "Habitat") +
  geom_hline(yintercept = 0, linetype="dashed", color="gray") +
  geom_label(label=c("No Change",""), color = "gray",
             nudge_y = 0.0377, nudge_x = 0.51, label.size=NA, ) +
  geom_point(color=c("#00BFC4","#F8766D"), lwd=2) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), lwd = 1.075,
                width=0.125, color=c("#00BFC4","#F8766D"))
beep(10)
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
