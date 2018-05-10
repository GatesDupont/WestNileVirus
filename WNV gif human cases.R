frames = 22

for(i in 1:frames){
  # creating a name for each plot file with leading zeros
  if (i < 10) {name = paste("~/Desktop/WNV_Visuals/HumanCases/000",i,"plot.png",sep="")}
  if (i < 100 && i >= 10) {name = paste("~/Desktop/WNV_Visuals/HumanCases/00",i,"plot.png", sep="")}
  if (i >= 100) {name = paste("~/Desktop/WNV_Visuals/HumanCases/0", i,"plot.png", sep="")}
  
  #saves the plot as a .png file in the working directory
  png(name, width=7, height=4.5, units = "in", res=200)
  par(mfrow=c(1,2))
  plot(nj.yr.raster[[i]], col=colorRampPalette(c("red", "yellow"))(7), zlim=c(0.5,3.5),
       main="Avian Abundance", axes=F, frame.plot=F, box=F, legend=F)
  plot(WNVstate[i], frame.plot=F, axes=F, cex=(WNVstate[i])*0.5, pch=20, col=rev(heat.colors(48))[WNVstate[i]], 
       ylab="", xlab="", main="Human Cases")
  mtext(as.character(1995+i), side = 3, line = -11, outer = TRUE)
  mtext("Red shows a greater decline in birds and greater number of human cases, while yellow shows fewer.",
        side = 3, line = -20, outer = TRUE, cex=0.75)
  dev.off()
}
