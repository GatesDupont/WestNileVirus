library(ggplot2)
library(ggpubr)

growth = data.frame(
  amcr = lambda_amcr_0501,
  blja = lambda_blja_0502,
  tuti = lambda_tuti_0401
)

quants = t(data.frame(
  amcr = quantile(growth$amcr, probs=c(0.025, 0.975)),
  blja = quantile(growth$blja, probs=c(0.025, 0.975)),
  tuti = quantile(growth$tuti, probs=c(0.025, 0.975))
))

lambda = ggplot(growth) + 
  
  # American Crow
  geom_density(aes(x=amcr), fill="red", color="white", alpha=0.55) +
  geom_vline(aes(xintercept=mean(amcr)), color="white",  size=1) +
  geom_vline(aes(xintercept=quants[1,1]), col="white", linetype="dashed", size=1.01) +
  geom_vline(aes(xintercept=quants[1,2]), col="white", linetype="dashed", size=1.01) +
  geom_text(size=4, x = mean(growth$amcr), y = -3, color = "black", label="American Crow") +
  geom_text(size=3, x = mean(growth$amcr), y = 75, color = "black", 
            label=paste(round(mean(growth$amcr), 3))) +
  
  # Blue Jay
  geom_density(aes(x=blja), fill="red", color="white", alpha=0.55) +
  geom_vline(aes(xintercept=mean(blja)), color="white",  size=1) +
  geom_vline(aes(xintercept=quants[2,1]), col="white", linetype="dashed", size=1.01) +
  geom_vline(aes(xintercept=quants[2,2]), col="white", linetype="dashed", size=1.01) +
  geom_text(size=4, x = mean(growth$blja), y = -3, color = "black", label="Blue Jay") +
  geom_text(size=3, x = mean(growth$blja), y = 100, color = "black", 
            label=paste(round(mean(growth$blja), 3))) +
  
  # Tufted Titmouse
  geom_density(aes(x=tuti), fill="red", color="white", alpha=0.55) +
  geom_vline(aes(xintercept=mean(tuti)), color="white",  size=1) +
  geom_vline(aes(xintercept=quants[3,1]), col="white", linetype="dashed", size=1.01) +
  geom_vline(aes(xintercept=quants[3,2]), col="white", linetype="dashed", size=1.01) +
  geom_text(size=4, x = mean(growth$tuti), y = -3, color = "black", label="Tufted Titmouse") +
  geom_text(size=3, x = mean(growth$tuti), y = 155, color = "black", 
            label=paste(round(mean(growth$tuti), 3))) +
  
  # Formatting
  geom_vline(aes(xintercept=0), color="gray60", size=1, linetype="dotted") +
  ylim(-4, 155) +
  xlim(-0.11, 0.005) +
  xlab(paste("1 - Annual Growth Rate (\u03BB), Primary Period of Decline, Adjusted by Species")) +
  ylab("Density") +
  theme_pubr()

ggsave("GrowthRates.png", path = paste0(getwd(), "/WNV SP19"), lambda, dpi = 320, width = 12, height = 4)
