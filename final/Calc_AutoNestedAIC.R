# Gates Dupont    #
# April 24, 2019  #
# # # # # # # # # #

# 42837
library(mgcv)
library(tidyr)
library(stringr)
library(qpcR)

# Train on sets[[1]], 2000 obs/yr for 23 yrs (46000 obs)

df$dummy = 1

#----Making all possible combinations of full model----
count = expand.grid(
  count_days    = c("effortDays", "dummy"),
  count_hours   = c("effortHours", "dummy"),
  count_years   = c("s(yr, k=11)", "dummy"),
  count_latlon  = c("s(lat, long)", "dummy")
)

detect = expand.grid(
  detect_days    = c("effortDays", "dummy"),
  detect_hours   = c("effortHours", "dummy"),
  detect_years   = c("s(yr, k=11)", "dummy"),
  detect_latlon  = c("s(lat, long)", "dummy")
)

model.formulas = vector("list", 256)
k = 1
for (i in 1:length(count[, 1])) {
  a = as.character(unlist(count[i, ])) %>%
    str_flatten(collapse = " + ")
  
  for (j in 1:length(detect[, 1])) {
    b = as.character(unlist(detect[j, ])) %>%
      str_flatten(collapse = " + ")
    
    model.formulas[[k]] = list(formula(str_glue("maxFlock", " ~ ", a)),
                         formula(str_glue(" ~ ", b)))
    k = k + 1
  }
}

#----Generating model names----
count.names = expand.grid(
  count_days    = c("D", "."),
  count_hours   = c("H", "."),
  count_years   = c("Y", "."),
  count_latlon  = c("L", ".")
)

detect.names = expand.grid(
  detect_days    = c("D", "."),
  detect_hours   = c("H", "."),
  detect_years   = c("Y", "."),
  detect_latlon  = c("L", ".")
)

model.names = vector("list", 256)
k = 1
for (i in 1:length(count.names[, 1])) {
  a = as.character(unlist(count.names[i, ]))
  a = paste(a, collapse = "")
  
  for (j in 1:length(detect.names[, 1])) {
    b = as.character(unlist(detect.names[j, ]))
    b = paste(b, collapse = "")
    
    model.names[[k]] = paste(str_glue("N(", a, ")", "Phi(", b, ")"))
    k = k + 1
  }
}

#----Running models in parallel----
library(doParallel)
cores = detectCores()
cl = makeCluster(cores[1] - 1)
registerDoParallel(cl)

model.fits = foreach(i = 1:256, .packages = c("mgcv")) %dopar% {
  assign(model.names[[i]],
         gam(
           formula = model.formulas[[i]],
           family = ziplss,
           gamma = 1.4,
           data = df
         ))
}
stopCluster(cl)

#----Assigning Names----
for (i in 1:256) {
  assign(model.names[[i]],
         model.fits[[i]])
}

#----AIC Model Comparison table----
aic.call = paste0("`", model.names, "`") %>%
  str_flatten(., ", ") %>%
  paste0("AIC(", ., ")")
models.aic = eval(parse(text = aic.call))
models.weights = akaike.weights(models.aic$AIC)
models.aictab = cbind(models.aic, models.weights)
models.aictab = models.aictab[order(models.aictab["deltaAIC"]), ]
models.aictab$weights = round(models.aictab$weights, 4)
View(models.aictab)
