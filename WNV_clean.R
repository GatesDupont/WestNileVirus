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

#----Setup----
setwd("~/WNV SP18")

#----Importing PFW data----
if(F){
  raw.pfw = read.csv("PFW_amecro.csv")
  # rm(list=setdiff(ls(), "raw.pfw"))
}

#----Converting to duplicate data----
rawData = raw.pfw

#----Selecting states----
states = c('ME', 'NH', 'VT', 'MA', 'RI', 'CT', 'NY', 'PA', 'NJ')
rawData = rawData[rawData$StatProv %in% states,]

#----Formatting effort----
rawData[rawData==""] = NA # blank to NA
rawData = rawData %>%
  filter(complete.cases(.[,c(1,3,4,6,8:14,21:25)])) %>% # remove NAs
  mutate(effortDaysNumerical = rowSums(.[10:13])) %>% # Summing half days
  mutate(obshrs=
           recode(EFFORT_HRS_ATLEAST,
                                   "0.001"=1,"1"=2,"4"=3,"4.001"=3,"8.001"=4)) %>%
  #filter(str_detect(str_to_lower(ENTRY_TECHNIQUE), "post") == FALSE) %>% # removing post code autozips
  dplyr::select(locID=1, yr=9, maxFlock=25, lat=3, long=4,
                effortHours=37, effortDays=36, state=27) %>% # selecting relevant data
  filter(maxFlock < 51) %>%
  droplevels()

dfEffHD=droplevels(rawData)

#----Selecting power users----
yearly_list_count = table(dfEffHD$locID, dfEffHD$yr) # creates a table showing number of observations at each location each year.
row_sums = rowSums(yearly_list_count >= 10) # rows where there are at least 10 observations
threeyears = which(row_sums >=3) # for rows with 10 obs over at least 3 years
newIDs = names(threeyears) # just setting a new variable name
#Final product:
df = dfEffHD[dfEffHD$locID %in% newIDs,] 

#----Fixing year----
df$yr = as.numeric(gsub("[^0-9\\.]", "", df$yr))
