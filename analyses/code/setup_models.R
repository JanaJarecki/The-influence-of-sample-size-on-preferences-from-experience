# ==========================================================================
# Setup: Cognitive models
# Author: Jana B. Jarecki
# ==========================================================================
# library(cognitivemodels) # from github

## Options for parameter estimation
model_options <- list(
  lb = c(rp = 0, sigma = 0.0001), # par lower bound: alpha (rp) and sigma
  ub = c(sigma = 1),              # par upper bound
  solver = "solnp",               # optimization solver
  fit_args = list(                # type of likelihood function
    options = list(
      pdf = "truncnormal",
      a = 0,                      # lower bound of the trunc. normal dist.
      b = 1))                     # upper bound of the trunc. normal dist.
  )


## Set up the models -------------------------------------------------------
# RF: relative frequency model
# BVU: Bayesian value updating model
# BVU_d1: Bayesian value updating model with delta = 1
# Baseline: baseline model

# RF
RF <- function(dt, fix = list(rn = NA)) {
  M <- cognitivemodel(data = dt) +
    utility_pow_c(value_scaled ~ gamblex, fix = fix) +
    function(pred, data, par) {
      # re-transform the data onto the original scale
      y <- data$relfreq_x * pred
      rp <- par["rp"]
      y <- (sign(rp) * y)^((1/rp)*(rp!=0)) * exp(y)^(rp==0)
      y <- replace(y, y > data$gamblex, data$gamblex[y > data$gamblex])
      y / data$gamblex
    }
  fit(M, options = c(model_options))
  return(M)
}

# BVU Model
BVU <- function(dt, fix_bayes = list()) {
  M <- cognitivemodel(data = dt) +
    bayes_beta(~ count_x + count_0, format = "count", fix = fix_bayes) +
    utility_pow_c(value_scaled ~ gamblex, fix = list(rn = NA)) +
    function(pred, data, par) {
      # re-transform the data
      y <- data$pr_c * pred
      rp <- par["rp"]
      y <- (sign(rp) * y)^((1/rp)*(rp!=0)) * exp(y)^(rp==0)
      y / data$gamblex
    }
  fit(M, options = model_options)
  return(M)
}

# BVU Model with delta = 1 (Bayesian delta)
BVU_d1 <- function(dt) {
  BVU(dt = dt, fix_bayes = list(delta = NA))
}

# Baseline Model
Baseline <- function(dt) {
  model_options$lb <- model_options$lb["sigma"]
  baseline_mean_c(value_scaled ~ ., data = dt, options = model_options)
}