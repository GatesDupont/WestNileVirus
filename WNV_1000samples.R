# Gates Dupont #
# 2019         #
# # # # # # # #

library(mgcv)
library(plyr)
library(dplyr)

#----Load clean data----
pfw = read.csv("~/WNV SP18/WNV_clean.csv")
pfw = pfw[,-1]

#----Number of base year records----
base = as.numeric(table(pfw$yr)[1])

#----1000 samples----
sets = vector("list", 1000)
for(i in 1:1000){
  set.seed(i) # Set iterative seed for reproducibility
  sets[[i]] #
}

#----Splitting by year----
pfw.split = pfw %>%
  split(pfw$yr)

#----1000 samples----
sets = vector("list", 1000)
for(i in 1:1000){
  # Set iterative seed for reproducibility
  set.seed(i) 
  # Randomly selecting balanced samples
  equal.years = vector("list", 23)
  for(j in 1:23){
    equal.years[[j]] = sample_n(pfw.split[[j]], size=base, replace=F)
  }
  # Append to sets list
  sets[[i]] = bind_rows(equal.years)
}

#----Running models in parallel----
library(doParallel)
cores = detectCores()
cl = makeCluster(cores[1]-1)
registerDoParallel(cl)

#----Let's go----
models = vector("list", 50)
fits = vector("list", 50)
traj.data = vector("list", 50)

takes = foreach(i=1:50, .packages=c("mgcv")) %dopar% {
#for(i in 1){    
  #----ZI-GAM----
  models[[i]] = gam(list(
    maxFlock ~ 
      effortHours +
      effortDays +
      s(yr, k=11) +
      s(lat,long, k=5),
    ~
      effortHours +
      effortDays +
      s(yr, k=11) +
      s(lat,long, k=5)),
    family = ziplss,
    gamma = 1.4,
    data = sets[[i]])
  
  #----Trajectory dataset----
  traj.data[[i]] <- data.frame(
    yr = 1996:2018,
    lat = mean(sets[[i]]$lat),
    long = mean(sets[[i]]$long),
    effortDays = mean(sets[[i]]$effortDays),
    effortHours = mean(sets[[i]]$effortHours))
  
  #----Model predictions----
  fits[[i]] = as.numeric(predict(models[[i]], newdata = traj.data[[i]], type="response"))
}
stopCluster(cl)

#----Average of the iterations----
it.avg = colMeans(as.data.frame(do.call(rbind, takes)))

#----Plotting the iterations----
plot(takes[[1]], type="l", col="green2")
for(i in 2:50){
  lines(takes[[i]], col="green2")
}
points(as.numeric(it.avg), col="blue", pch=20, cex=2)

#----Calculating confidence intervals----
ints = vector("list", 23)
for(i in 1:23){
  ints[[i]] = quantile(test1[,i], probs=c(0.025, 0.975))
}

#----Aggregating plot data----
final = data.frame(yr=1996:2018, avg = it.avg, as.data.frame(do.call(rbind, ints)))
colnames(final)[3:4] = c("L", "U")

#----Plotting with confidence intervals----
require(plotrix)
plot(avg~yr, final, pch=20)
plotCI(x=final$yr, y=final$avg, ui=final$U, li=final$L,
       pch=20, ylab="Relative Abundance", xlab="Year",
       main="American Crows, Northeast US", bty="n")
