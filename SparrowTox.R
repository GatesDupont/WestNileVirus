# August 26, 2018 #
# Gates Dupont    #
# # # # # # # # # # 

# Modeling the relationship between sparrow toxicity 
# and proportion urban land cover form 1 km radius (or whatever)


# 1) Load your tox samples data (what I'm calling "SESP")
# 2) Load land cover data (probably a raster)
# 3) Use "extract()" command to extrand proportion land cover at each location
# 4) Assosciate those proportions back to the original tox samples data ("SESP")
# So each variable should be a column in the SESP data frame.
# Then...

#----Set up the linear model----
SayersLM = lm(tox~ ocean + saltmarsh + freshmarsh + forest + urban, 
              data=SESP) # Easiest to just not include coordinates lat/long for this part

#----Generate a variable for prediction intervals----
xAxisVar = seq(from = 0, to = 1, by=0.1) # This is an example of your variable of 
                                         # interest for prediction

#----Create a "new" data frame for predict command----
TrajectoryData = data.frame( # Hold all vars constant at mean except var. of interest.
  ocean = mean(ocean), # OR min()
  saltmarsh = mean(saltmarsh),
  freshwater = mean(freshwater),
  forest = mean(forest),
  urban = xAxisVar # This is where the 0-1 part ends up.
)

#----Tell R to make the predictions from the linear model with the new data----
SayersPredictions = predict(
  object = SayersLM, 
  newdata = TrajectoryData,
  type = "response",
  se.fit=TRUE
)

# SayersPredictions will be a data frame, with assosciated standard errors, 
# which will allow you to plot the predictions very easily with "plot()"
# If you have a problem, you can always just google how to plot data
# with standard errors in R.

