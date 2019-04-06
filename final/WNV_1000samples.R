# Gates Dupont #
# 2019         #
# # # # # # # #

"Bootstrapping 1000 ZI-GAMs"

library(mgcv)
library(plyr)
library(dplyr)

#----Load clean data----
pfw = read.csv("~/WNV SP19/PFW_amecro_clean_geocode.csv")
pfw = pfw[,-1]

#----Number of base year records----
base = as.numeric(table(pfw$yr)[1])
base = 2000

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
models = vector("list", 1000)
fits = vector("list", 1000)
traj.data = vector("list", 1000)

takes = foreach(i=1:1000, .packages=c("mgcv")) %dopar% {
  #for(i in 1){    
  #----ZI-GAM----
  models[[i]] = gam(list(
    maxFlock ~ 
      effortHours +
      effortDays +
      s(day, k=5) +
      s(yr, k=11) +
      s(lat,long),
    ~
      effortHours +
      effortDays +
      s(day, k=5) +
      s(yr, k=11) +
      s(lat,long)),
    family = ziplss,
    gamma = 1.4,
    data = sets[[i]])
  
  #----Trajectory dataset----
  traj.data[[i]] <- data.frame(
    yr = 1996:2018,
    lat = mean(sets[[i]]$lat),
    long = mean(sets[[i]]$long),
    day = 1,
    effortDays = mean(sets[[i]]$effortDays),
    effortHours = mean(sets[[i]]$effortHours))
  
  #----Model predictions----
  fits[[i]] = as.numeric(predict(models[[i]], newdata = traj.data[[i]], type="response"))
}
stopCluster(cl)

#----Average of the iterations----
takes.df = as.data.frame(do.call(rbind, takes))
it.avg = colMeans(as.data.frame(do.call(rbind, takes)))

#----Plotting the iterations----
plot(takes[[1]]~c(1996:2018), type="l", col=rgb(0,0,1,alpha=0.05), 
     frame.plot=F, ylim=c(0.75,1.35), xlab="Years", 
     ylab="Relative Abundance", main="American Crow, Northeast US")
for(i in 2:1000){
  lines(takes[[i]]~c(1996:2018), col=rgb(0,0,1,alpha=0.05))
}
lines(as.numeric(it.avg)~c(1996:2018), col="white", lwd=1, lty=2)
points(as.numeric(it.avg)~c(1996:2018), col="white", pch=20, cex=2)

#----Calculating confidence intervals----
ints = vector("list", 23)
for(i in 1:23){
  ints[[i]] = quantile(takes.df[,i], probs=c(0.025, 0.975))
}

#----Aggregating plot data----
final = data.frame(yr=1996:2018, avg = it.avg, as.data.frame(do.call(rbind, ints)))
colnames(final)[3:4] = c("L", "U")

#----Plotting with confidence intervals----
require(plotrix)
plotCI(x=final$yr, y=final$avg, ui=final$U, li=final$L,
       pch=20, ylab="Relative Abundance", xlab="Year",
       main="American Crows, Northeast US", bty="n")

#----Calculating Lambda 1999-2004----
lambda = ((takes.df$V9-takes.df$V4)/(takes.df$V9))/5
growth = data.frame(lambda = lambda)
quants = quantile(lambda, probs=c(0.025, 0.975))

#----Desnity plot of lambda 1999-2004----
ggplot(growth, aes(x=lambda)) + 
  geom_density(fill="gray50", color="white") +
  geom_vline(aes(xintercept=mean(lambda)), color="white",  size=1) +
  geom_vline(aes(xintercept=quants[1]), col="white", linetype="dashed", size=1.01) +
  geom_vline(aes(xintercept=quants[2]), col="white", linetype="dashed", size=1.01) +
  xlab(paste("Annual Growth Rate (\u03BB), 1999-2004 Average")) +
  ylab("Density") +
  theme_pubr()

#----Calculating Lambda 1999-2004----
lambda2 = ((takes.df$V13-takes.df$V4)/(takes.df$V19))/6
growth2 = data.frame(lambda2 = lambda2)
quants2 = quantile(lambda2, probs=c(0.025, 0.975))

#----Desnity plot of lambda 1999-2004----
ggplot(growth2, aes(x=lambda2)) + 
  geom_density(fill="gray50", color="white") +
  geom_vline(aes(xintercept=mean(lambda2)), color="white",  size=1) +
  geom_vline(aes(xintercept=quants2[1]), col="white", linetype="dashed", size=1.01) +
  geom_vline(aes(xintercept=quants2[2]), col="white", linetype="dashed", size=1.01) +
  xlab(paste("Annual Growth Rate (\u03BB), 2008-2014 Average")) +
  ylab("Density") +
  theme_pubr()

#----Calculating Lambda 1999-2018----
lambdaO = ((takes.df$V13-takes.df$V4)/(takes.df$V23))/6
growthO = data.frame(lambdaO = lambdaO)
quantsO = quantile(lambdaO, probs=c(0.025, 0.975))

#----Desnity plot of lambda 1999-2018----
ggplot(growthO, aes(x=lambdaO)) + 
  geom_density(fill="gray50", color="white") +
  geom_vline(aes(xintercept=mean(lambdaO)), color="white",  size=1) +
  geom_vline(aes(xintercept=quantsO[1]), col="white", linetype="dashed", size=1.01) +
  geom_vline(aes(xintercept=quantsO[2]), col="white", linetype="dashed", size=1.01) +
  xlab(paste("Average Annual Growth Rate (\u03BB) Since Disease Introduction")) +
  ylab("Density") +
  theme_pubr()
