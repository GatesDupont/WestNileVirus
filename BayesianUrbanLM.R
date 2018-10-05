# Gates Dupont           #
# F18 WNV Bayesian Urban #
# # # # # # # # # # # # #

#----Loading bayesian package----
library(R2jags)
library(ggplot2)

#------Setting seed for reproducibility----
set.seed(4797)

#----Preparing the pfw data----
# Load data
pfw = read.csv('~/WNV F18/PFW_amecro_zerofill_landcover.csv')

# Subsetting for states
states = c("NY", "PA", "NJ", "CT", "RI", "MA")
pfw = pfw[pfw$state %in% states,]

# Subsetting for years
years = 1999:2004
pfw = pfw[pfw$yr %in% years,]

#----Converting habitat to binary for urban/not-urban----
# Making a new column in the df
pfw$urban = pfw$URB

# Separating by urban value
rural = pfw[pfw$urban <= 0.4,]
urban = pfw[pfw$urban >= 0.6,]

#----Converting data for jags----
jags_rural <- with(rural, list(
  Count = maxFlock,
  Year = yr, 
  Hours = effortHours, 
  Days = effortDays, 
  #Lat = lat,
  #Long = long,
  #LocID = locID,
  N = length(maxFlock)))

#----Converting data for jags----
jags_urban <- with(urban, list(
  Count = maxFlock,
  Year = yr, 
  Hours = effortHours, 
  Days = effortDays, 
  #Lat = lat, 
  #Long = long,
  #LocID = locID,
  N = length(maxFlock)))


#----Rural Bayesian Model configuration---
lm_rural <- function(){

  # Priors:
  r.alpha ~ dnorm(0,0.01) # intercept
  r.beta1 ~ dnorm(0,0.01)
  r.beta2 ~ dnorm(0,0.01)
  r.beta3 ~ dnorm(0,0.01)
  r.sigma ~ dunif(0, 100) # standard deviation
  r.tau <- 1 / (r.sigma * r.sigma) # sigma^2 doesn't work in JAGS
  
  # Model structure
  for (i in 1:N){
    Count[i] ~ dnorm(r.mu[i], r.tau) # tau is precision (1 / variance)
    r.mu[i] <- r.alpha + r.beta1*Year[i] + r.beta2*Hours[i] + r.beta3*Days[i]
  }
}

#----Urban Bayesian model configuration---
lm_urban <- function(){
  
  # Priors:
  u.alpha ~ dnorm(0,0.01) # intercept
  u.beta1 ~ dnorm(0,0.01)
  u.beta2 ~ dnorm(0,0.01)
  u.beta3 ~ dnorm(0,0.01)
  u.sigma ~ dunif(0, 100) # standard deviation
  u.tau <- 1 / (u.sigma * u.sigma) # sigma^2 doesn't work in JAGS
  
  # Model structure
  for (i in 1:N){
    Count[i] ~ dnorm(u.mu[i], u.tau) # tau is precision (1 / variance)
    u.mu[i] <- u.alpha + u.beta1*Year[i] + u.beta2*Hours[i] + u.beta3*Days[i]
  }
  
  # Calculating the difference between year betas
  #diff_year_beta = r.beta2 - u.beta2
}

#----Initial values----
r.init_values <- function(){
  list(r.alpha = rnorm(1), r.beta1 = rnorm(1), r.beta2 = rnorm(1), r.beta3 = rnorm(1), r.sigma = runif(1))
}

u.init_values <- function(){
  list(u.alpha = rnorm(1), u.beta1 = rnorm(1), u.beta2 = rnorm(1), u.beta3 = rnorm(1), u.sigma = runif(1))
}

#----Parameters to save----
r.params <- c("r.alpha", "r.beta1", "r.beta2", "r.beta3", "r.sigma")
u.params <- c("u.alpha", "u.beta1", "u.beta2", "u.beta3", "u.sigma")

#----Fitting the bayesian models----
fit_rural <- jags(data = jags_rural, inits = r.init_values, parameters.to.save = r.params, model.file = lm_rural,
                n.chains = 3, n.iter = 10000, n.burnin = 2000, n.thin = 10, DIC = F)

fit_urban <- jags(data = jags_urban, inits = u.init_values, parameters.to.save = u.params, model.file = lm_urban,
                 n.chains = 3, n.iter = 10000, n.burnin = 2000, n.thin = 10, DIC = F)

#----Diagnostics of models----
print(fit_rural)
print(fit_urban)

traceplot(fit_rural, mfrow = c(3, 2), ask = F)
traceplot(fit_urban, mfrow = c(3, 2), ask = F)
dev.off()

r_mcmc <- as.mcmc(fit_rural)
plot(r_mcmc, mfrow=c(3,2))
u_mcmc <- as.mcmc(fit_urban)
plot(u_mcmc)

#----ggplot2 year beta distributions----
df = data.frame(beta = c(fit_rural$BUGSoutput$sims.list$r.beta1, fit_urban$BUGSoutput$sims.list$u.beta1),
                habitat = c(rep("rural", length(fit_rural$BUGSoutput$sims.list$r.beta1)),
                            rep("urban", length(fit_urban$BUGSoutput$sims.list$u.beta1))))

ggplot(df, aes(x=beta, fill=habitat, color=habitat)) +
  theme_dark() + labs(x="Beta Estimates", y = "Frequency",
                         title = "Comparison of Beta Distributions for Year") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=17),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")) +
  geom_vline(aes(xintercept=0), linetype="dashed", color = "gray", size=1.2) +
  geom_vline(aes(xintercept=fit_rural$BUGSoutput$mean$r.beta1), linetype="solid", color = "blue", size=1.2) +
  geom_vline(aes(xintercept=fit_urban$BUGSoutput$mean$u.beta1), linetype="solid", color = "red", size=1.2) +
  geom_density(alpha=0.25, size=1.2) +
  scale_color_manual(values = c("blue", "red")) +  scale_fill_manual(values = c("blue", "red"))
