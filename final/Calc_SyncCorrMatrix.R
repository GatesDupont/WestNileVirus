# Gates Dupont #
# SP19         #
# # # # # # # # 

"Correlation Matrix to detect instraspecific population syncrhony"


#----Loading model-predicted trends----
spp = c("SOSP", "WBNU", "MODO",
        "AMCR", "BLJA", "TUTI")
spt = vector("list", 6)
for(i in 1:length(spp)){
  spt[[i]] = read.csv(paste0(getwd(), "/WNV SP19/", spp[[i]], "_final.csv"))
}


#----Column-bindind species trends----
pops = data.frame("SOSP" = spt[[1]]$avg)
pops[,2:6] = 0
colnames(pops) = c("SOSP", "WBNU", "MODO", "AMCR", "BLJA", "TUTI")
for(i in 2:length(spp)){
  pops[,i] = spt[[i]]$avg
}
pops = pops[5:23,]


#----Correlation matrix----
cm = matrix(rep(0, 6*5), 6, 5)
colnames(cm) = c("WBNU", "MODO", "AMCR", "BLJA", "TUTI")
rownames(cm) = c("SOSP", "WBNU", "MODO", "AMCR", "BLJA", "TUTI")

#----Assigning rho---
for(i in 1:6){
  for(j in 2:6){
    cm[i,j-1] = as.numeric(cor.test(pops[,i], pops[,j], method="spearman")$estimate)
  }
}
cm[lower.tri(cm)] = 0L

#----Rho significance---
cm.sig = matrix(rep(0, 6*5), 6, 5)
colnames(cm.sig) = c("WBNU", "MODO", "AMCR", "BLJA", "TUTI")
rownames(cm.sig) = c("SOSP", "WBNU", "MODO", "AMCR", "BLJA", "TUTI")
for(i in 1:6){
  for(j in 2:6){
    cm.sig[i,j-1] = as.numeric(cor.test(pops[,i], pops[,j], method="spearman")$p.value)
  }
}
cm.sig[lower.tri(cm.sig)] = 0L

for(i in 1:30){
  if(cm.sig[i] < 0.05 & cm.sig[i] > 0){
    cm.sig[i] = TRUE
  }else{
    cm.sig[i] = FALSE
  }
}


#----Plotting rho----
library(raster)
plot(raster(cm), frame.plot=F, axes=F, 
     col=c(colorRampPalette(c("red", "white", "blue"))(100)), zlim=c(-.7,.7))
text(cm, digits=2, col="white")

#----Plotting sig----
plot(raster(cm.sig), frame.plot=F, axes=F, 
     col=c(colorRampPalette(c("white", "black"))(100)))

#----Plotting sig with sign of rho----
plot(raster(cm.sig*sign(cm)), frame.plot=F, axes=F, legend=F,
     col=c(colorRampPalette(c("red","white","blue"))(3)))
text(cm)



r = raster(ncol=6,nrow=5)
values(r) = as.numeric(cm)
plot(r)
