# Setup for cognitive modeling
# --------------------------------------------------------------------------
## Options for parameter (par) fitting and likelihood function
model_options <- list(
  lb = c(rp = 0, sigma = 0.0001), # par lower bound: alpha (rp) and sigma
  ub = c(sigma = 1),              # par upper bound
  fit_solver = "solnp",           # optimization solver
  fit_args = list(                # type of likelihood function
    options = list(pdf = "truncnormal", a = 0, b = 1))
  )
# Set up the models
# RF: relative frequency model
# BVU: Bayesian value updating model
# Baseline: baseline model
RF <- function(d, fix = list(rn = NA)) {
  start(data = d) %+%
    utility_pow(value_scaled ~ gamblex, fix = fix) %>%
    fun(function(pred, data, par) {
      y <- data$relfreq_x * pred
      rp <- par["rp"]
      y <- (sign(rp) * y)^((1/rp)*(rp!=0)) * exp(y)^(rp==0)
      y <- replace(y, y > data$gamblex, data$gamblex[y > data$gamblex])
      y / data$gamblex
    }) %>%
    end(options = model_options)
}
# BVU Model
BVU <- function(d, fix = list(rn = NA)) {
  start(data = d) %+%
    bayes_beta(~ count_x + count_0, format = "count") %+%
    utility_pow(value_scaled ~ gamblex, fix = fix) %>%
    fun(function(pred, data, par) {
      y <- data$pred_count_x * pred
      rp <- par["rp"]
      y <- (sign(rp) * y)^((1/rp)*(rp!=0)) * exp(y)^(rp==0)
      y / data$gamblex
    }) %>%
    end(options = model_options)
}
# Baseline Model
Baseline <- function(d) {
  start(data = d) %+%
  baseline_mean(value_scaled ~ ., type = "const", mode = "continuous") %>%
    end(options = model_options[-1])
}