library(ggplot2)
library(ggpubr)

period = 1

if(period == 1){
  growth = data.frame(
    amcr = lambda_amcr_0501,
    blja = lambda_blja_0502,
    tuti = lambda_tuti_0401,
    wbnu = lambda_wbnu_0300,
    sosp = lambda_sosp_0301,
    modo = lambda_modo_0401
  )
  lab = "Primary Period of Decline"
  
}else if(period == 2){
  growth = data.frame(
    amcr = lambda_amcr_1408,
    blja = lambda_blja_1208,
    tuti = lambda_tuti_1308,
    wbnu = lambda_wbnu_0907,
    sosp = lambda_sosp_1109,
    modo = lambda_modo_1107
  )
  lab = "Secondary Period of Decline"
  
}else{
  stop("check 'period'")
}

quants = t(data.frame(
  amcr = quantile(growth$amcr, probs=c(0.025, 0.975)),
  blja = quantile(growth$blja, probs=c(0.025, 0.975)),
  tuti = quantile(growth$tuti, probs=c(0.025, 0.975)),
  wbnu = quantile(growth$wbnu, probs=c(0.025, 0.975)),
  sosp = quantile(growth$sosp, probs=c(0.025, 0.975)),
  modo = quantile(growth$modo, probs=c(0.025, 0.975))
))

simple_lambda = ggplot(growth) +
    xlim(-0.097, 0.017) +
    theme_classic() +
    theme(axis.title.y=element_blank()) +
    geom_vline(xintercept = 0, linetype="dashed", color="black") +
    xlab(paste0("1 - Annual Growth Rate (\u03BB), ", lab)) +
    
    # American Crow
    geom_point(aes(x=mean(amcr), y=1), lwd=2, colour = "red") +
    geom_errorbarh(aes(xmin=quants[1,1], xmax=quants[1,2], y=1),
                   height=0.2, lwd=1.05, colour = "red") +
    geom_text(size=2.5, aes(x=mean(amcr), y=1.25, fontface="italic", 
                  label=paste(round(mean(amcr), 3)))) +
    
    # Blue Jay
    geom_point(aes(x=mean(blja), y=2), lwd=2, colour = "red") +
    geom_errorbarh(aes(xmin=quants[2,1], xmax=quants[2,2], y=2),
                   height=0.2, lwd=1.05, colour = "red") +
    geom_text(size=2.5, aes(x=mean(blja), y=2.25, fontface="italic", 
                  label=paste(round(mean(blja), 3)))) +
    
    # Tufted Titmouse    
    geom_point(aes(x=mean(tuti), y=3), lwd=2, colour = "red") +
    geom_errorbarh(aes(xmin=quants[3,1], xmax=quants[3,2], y=3),
                   height=0.2, lwd=1.05, colour = "red") +
    geom_text(size=2.5, aes(x=mean(tuti), y=3.25, fontface="italic", 
                  label=paste(round(mean(tuti), 3)))) +
  
    # Mourning Dove
    geom_point(aes(x=mean(modo), y=4), lwd=2, colour = "gray40") +
    geom_errorbarh(aes(xmin=quants[6,1], xmax=quants[6,2], y=4),
                   height=0.2, lwd=1.05, colour = "gray40") +
    geom_text(size=2.5, aes(x=mean(modo), y=4.25, fontface="italic", 
                  label=paste(round(mean(modo), 3)))) +
    
    # White-breasted Nuthatch
    geom_point(aes(x=mean(wbnu), y=5), lwd=2, colour = "gray40") +
    geom_errorbarh(aes(xmin=quants[4,1], xmax=quants[4,2], y=5),
                   height=0.2, lwd=1.05, colour = "gray40") +
    geom_text(size=2.5, aes(x=mean(wbnu), y=5.25, fontface="italic", 
                  label=paste(round(mean(wbnu), 3)))) +
    
    # Song Sparrow
    geom_point(aes(x=mean(sosp), y=6), lwd=2, colour = "gray40") +
    geom_errorbarh(aes(xmin=quants[5,1], xmax=quants[5,2], y=6),
                   height=0.2, lwd=1.05, colour = "gray40") +
    geom_text(size=2.5, aes(x=mean(sosp), y=6.25, fontface="italic", 
                   label=paste(round(mean(sosp), 3)))) +
  
    scale_y_continuous(breaks = 1:6,
                       labels = c("1" = "American Crow", 
                                  "2" = "Blue Jay",
                                  "3" = "Tufted Titmouse",
                                  "4" = "Mourning Dove", 
                                  "5" = "White-breasted Nuthatch", 
                                  "6" = "Song Sparrow"))
simple_lambda


ggsave(paste0("SimpleLambda_", lab, ".png"), simple_lambda, 
       path = "~/Desktop", dpi = 320, width = 8, height = 4)
