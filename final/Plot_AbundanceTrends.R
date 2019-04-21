# Gates Dupont - WNV  #
# Combined pop figure #
# SP 2019             #
# # # # # # # # # # # #

spp = c("SOSP", "WBNU", "MODO",
        "AMCR", "BLJA", "TUTI")
spt = vector("list", 6)
for(i in 1:length(spp)){
  spt[[i]] = read.csv(paste0(getwd(), "/WNV SP19/", spp[[i]], "_final.csv"))
}

#----Plotting----
pop = ggplot() + 
  theme_pubr() + theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                       axis.title.x = element_text(size = 15, face = "bold"),
                       axis.title.y = element_text(size = 13, face = "bold")) +
  labs(title = "All Species", x = "Year", y = "Abundance Relative to '98-'99 Season") +
  geom_vline(xintercept = 1999.25, linetype = "dashed", 
             size=0.8, color = "gray80") +
  annotate("text", x = 1999.6, y = 143, label = "WNV Introduction", 
           colour = "gray60", angle = 90, size=5, fontface=2) +
  xlim(1999,2018) +
  
  # Song Sparrow
  geom_line(data=spt[[1]], aes(x=yr, y=avg), linetype = "solid", size=0.8, colour = "black") +
  geom_ribbon(data=spt[[1]], aes(x=yr, ymin=L,ymax=U),alpha=0.3, fill = "gray") +
  geom_point(data=spt[[1]], aes(x=yr, y=avg), size=3, colour="black") +
  geom_errorbar(data=spt[[1]], aes(x=yr, ymin=L, ymax=U), width=0.2, size=1.3, colour="black") +
  
  # White-breasted Nuthatch
  geom_line(data=spt[[2]], aes(x=yr, y=avg), linetype = "solid", size=0.8, colour = "black") +
  geom_ribbon(data=spt[[2]], aes(x=yr, ymin=L,ymax=U),alpha=0.3, fill = "gray") +
  geom_point(data=spt[[2]], aes(x=yr, y=avg), size=3, colour="black") +
  geom_errorbar(data=spt[[2]], aes(x=yr, ymin=L, ymax=U), width=0.2, size=1.3, colour="black") +
  
  # Mourning Dove
  geom_line(data=spt[[3]], aes(x=yr, y=avg), linetype = "solid", size=0.8, colour = "black") +
  geom_ribbon(data=spt[[3]], aes(x=yr, ymin=L,ymax=U),alpha=0.3, fill = "gray") +
  geom_point(data=spt[[3]], aes(x=yr, y=avg), size=3, colour="black") +
  geom_errorbar(data=spt[[3]], aes(x=yr, ymin=L, ymax=U), width=0.2, size=1.3, colour="black") +
  
  # American Cr
  geom_line(data=spt[[4]], aes(x=yr, y=avg), linetype = "solid", size=0.8, colour = "red") +
  geom_ribbon(data=spt[[4]], aes(x=yr, ymin=L,ymax=U),alpha=0.3, fill = "pink") +
  geom_point(data=spt[[4]], aes(x=yr, y=avg), size=3, colour="red") +
  geom_errorbar(data=spt[[4]], aes(x=yr, ymin=L, ymax=U), width=0.2, size=1.3, colour="red") +
  
  # Blue Jay
  geom_line(data=spt[[5]], aes(x=yr, y=avg), linetype = "solid", size=0.8, colour = "red") +
  geom_ribbon(data=spt[[5]], aes(x=yr, ymin=L,ymax=U),alpha=0.3, fill = "pink") +
  geom_point(data=spt[[5]], aes(x=yr, y=avg), size=3, colour="red") +
  geom_errorbar(data=spt[[5]], aes(x=yr, ymin=L, ymax=U), width=0.2, size=1.3, colour="red") +
  
  # Tufted Titmouse
  geom_line(data=spt[[6]], aes(x=yr, y=avg), linetype = "solid", size=0.8, colour = "red") +
  geom_ribbon(data=spt[[6]], aes(x=yr, ymin=L,ymax=U),alpha=0.3, fill = "pink") +
  geom_point(data=spt[[6]], aes(x=yr, y=avg), size=3, colour="red") +
  geom_errorbar(data=spt[[6]], aes(x=yr, ymin=L, ymax=U), width=0.2, size=1.3, colour="red")

pop

ggsave("AllSpp_test_solid.png", path = paste0(getwd(), "/WNV SP19"), pop, dpi = 320, width = 9, height = 6)
