# Gates Dupont    #
# April 22, 2019  #
# # # # # # # # # #

# 42837
library(mgcv)
library(tidyr)
library(stringr)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Simulating data as in original ziplss documentaiton #
# Wood et al. 2016                                    #
# http://dx.doi.org/10.1080/01621459.2016.1180986     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#----Simulating data----
f0 <- function(x) 2 * sin(pi * x); f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * 
  (10 * x)^3 * (1 - x)^10
n <- 500;set.seed(5)
x0 <- runif(n); x1 <- runif(n)
x2 <- runif(n); x3 <- runif(n)

## Simulate probability of potential presence...
eta1 <- f0(x0) + f1(x1) - 3
p <- binomial()$linkinv(eta1) 
y <- as.numeric(runif(n)<p) ## 1 for presence, 0 for absence

## Simulate y given potentially present (not exactly model fitted!)...
ind <- y>0
eta2 <- f2(x2[ind])/3
y[ind] <- rpois(exp(eta2),exp(eta2))

# # # # # # # # # # # #
# End simulated data  # 
# # # # # # # # # # # #

#----Pulling the data together----
df = data.frame(y, x0, x1, x2, x3)

# Full model, for reference
fullModel <- gam(list(
  
  y
  ~s(x0)+s(x1)+s(x2)+s(x3), # Count
  ~s(x0)+s(x1)+s(x2)+s(x3)), # Detection
  
  family=ziplss(), 
  data=df)

#----Model selection code----

# FUNCTION PARAMETERS
# Dataframe
# Index of covars of interest
  # Full model will have all of these covariates in both steps
  # All covariates will be smoothed

# MAKE MODEL FORMULAS
# Add dummy to data
# Dummy, one predictor, two predictors...
# Multi-covariate must have all combinations

# # # # # # # # # # # # # # # # # # # # # # #
# Reference eBird Best Practices            #
# http://strimas.com/ebird-best-practices/  #
# Johnston et al. 2019                      #
# # # # # # # # # # # # # # # # # # # # # # #

continuous_covs <- df %>% 
  select(2, 3, 4) %>% 
  names()

# create model formula for predictors
gam_formula_rhs <- str_glue("s({var}, k = {k})", 
                            var = continuous_covs, k = 4) %>% 
  str_flatten(collapse = " + ") %>% 
  str_glue(" ~ ", .,
           " + protocol_type + ",
           "s(time_observations_started, bs = \"cc\", k = {k})", 
           k = k_time) %>% 
  as.formula()

# model formula including response
gam_formula <- update.formula(observation_count ~ ., gam_formula_rhs)
gam_formula
