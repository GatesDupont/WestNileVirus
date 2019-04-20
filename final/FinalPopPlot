# Gates Dupont
# SP19
# 04/16/19

"Plotting final population size"

library(ggplot2)

spp = c("SOSP", "WBNU", "MODO",
        "AMCR", "BLJA", "TUTI")
spt = vector("list", 6)
for(i in 1:length(spp)){
  spt[[i]] = read.csv(paste0(getwd(), "/", spp[[i]], "_final.csv"))
}

t=23
# Time period of interest
# 1999 = 4
# 2005 = 10
# 2018 = 23

final.pop = ggplot() +
  xlim(50, 150) +
  theme_classic() +
  ggtitle("2005 Relative Abundnance") +
  theme(axis.title.y=element_blank(), 
        plot.title = element_text(hjust = 0.5,
                                  size = 20,
                                  face = "bold",
                                  colour="black"),
        axis.text.y = element_text(size=18, colour="gray20"),
        axis.title.x = element_text(size=15, colour="gray20"),
        axis.text.x = element_text(size=15, colour="gray20")) +
  geom_vline(xintercept = 100, linetype="dashed", color="gray70") +
  xlab(paste0("Percent of 1999 Relative Abundance")) +
  
  # Song Sparrow
  geom_point(aes(x=spt[[1]][10,3], y=1), lwd=3, colour = "gray30") +
  geom_errorbarh(aes(xmin=spt[[1]][t,4], xmax=spt[[1]][t,5], y=1),
                 height=0.3, lwd=1.35, colour = "gray30") +
  geom_text(size=5, aes(x=spt[[1]][t,3], y=1.4, fontface="italic", 
                          label=paste(round(spt[[1]][t,3], 0)))) +
  
  # White-breasted Nuthatch
  geom_point(aes(x=spt[[2]][t,3], y=2), lwd=3, colour = "gray30") +
  geom_errorbarh(aes(xmin=spt[[2]][t,4], xmax=spt[[2]][t,5], y=2),
                 height=0.3, lwd=1.35, colour = "gray30") +
  geom_text(size=5, aes(x=spt[[2]][t,3], y=2.4, fontface="italic", 
                          label=paste(round(spt[[2]][t,3], 0)))) +
  
  # Mourning Dove
  geom_point(aes(x=spt[[3]][t,3], y=3), lwd=3, colour = "gray30") +
  geom_errorbarh(aes(xmin=spt[[3]][t,4], xmax=spt[[3]][t,5], y=3),
                 height=0.3, lwd=1.35, colour = "gray30") +
  geom_text(size=5, aes(x=spt[[3]][t,3], y=3.4, fontface="italic", 
                          label=paste(round(spt[[3]][t,3], 0)))) +
  
  # Blue Jay
  geom_point(aes(x=spt[[5]][t,3], y=4), lwd=3, colour = "red") +
  geom_errorbarh(aes(xmin=spt[[5]][t,4], xmax=spt[[5]][t,5], y=4),
                 height=0.3, lwd=1.35, colour = "red") +
  geom_text(size=5, aes(x=spt[[5]][t,3], y=4.4, fontface="italic", 
                          label=paste(round(spt[[5]][t,3], 0)))) +
  
  # Tufted Titmouse
  geom_point(aes(x=spt[[6]][t,3], y=5), lwd=3, colour = "red") +
  geom_errorbarh(aes(xmin=spt[[6]][t,4], xmax=spt[[6]][t,5], y=5),
                 height=0.2, lwd=1.35, colour = "red") +
  geom_text(size=5, aes(x=spt[[6]][t,3], y=5.4, fontface="italic", 
                          label=paste(round(spt[[6]][t,3], 0)))) +
  
  # American Crow
  geom_point(aes(x=spt[[4]][t,3], y=6), lwd=3, colour = "red") +
  geom_errorbarh(aes(xmin=spt[[4]][t,4], xmax=spt[[4]][t,5], y=6),
                 height=0.2, lwd=1.35, colour = "red") +
  geom_text(size=5, aes(x=spt[[4]][t,3], y=6.4, fontface="italic", 
                          label=paste(round(spt[[4]][t,3], 0)))) +
  
  scale_y_continuous(breaks = 1:6,
                     labels = c("1" = "Song Sparrow", 
                                "2" = "White-breasted Nuhatch",
                                "3" = "Mourning Dove",
                                "4" = "Blue Jay", 
                                "5" = "Tufted Titmouse", 
                                "6" = "American Crow"))
print(final.pop)



if(F){ggsave(paste0("2005Pops", ".png"), final.pop, path=getwd(), dpi = 320, width = 10, height = 4)}
