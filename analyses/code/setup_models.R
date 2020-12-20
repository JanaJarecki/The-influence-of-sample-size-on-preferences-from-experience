# ==========================================================================
# Setup: Cognitive models
# Author: Jana B. Jarecki
# ==========================================================================
# library(cognitivemodels) # from janajarecki/github


## Options for parameter estimation ----------------------------------------
model_options <- list(
  lb = c(rp = -20, sigma = 0.0001), # par lower bound: alpha (rp) and sigma
  ub = c(          sigma = 0.50),   # par upper bound
  solver = c("grid", "solnp"),      # optimization solver
  fit_args = list(                  # type of likelihood function
      nbest = 3,
      nsteps = 5,
      pdf = "truncnormal",
      a = 0,                      # lower bound of the trunc. normal dist.
      b = 1                       # upper bound of the trunc. normal dist.
    ),
  solver_args = list(
    nsteps = 3,
    control = list(trace = 0), # print trace of solnp
    delta = 1.0e-3  # Relative step size in forward difference evaluation
  )                  
  )

## Set up the models -------------------------------------------------------
# RF: relative frequency model
# BVU: Bayesian value updating model
# BVU_d1: Bayesian value updating model with delta = 1
# Baseline: baseline model

# Reverse the power utility function
reverse_power <- function(x, rp) {
  if (rp==0) { return(exp(x)) }
  return((sign(rp) * x)^(1/rp))
}

# RF with weighting of the values (v) and no probability weighting (w)
RF <- function(dt, fix = c(gamman=1,gammap=1), w=NA, v="TK1992") {
  model_options$lb <- model_options$lb["sigma"] # only bound on sigma
  dt$gamble_y <- 0
  fix <- c(fix, c(lambda=1,beta=1,gamman=1))
  M <- cognitivemodel(data = dt) +
    cpt_c(value_scaled ~ gamblex + relfreq_x + gamble_y , weighting=w, value=v, fix = fix) +
    function(pred, data, par) {
      if ("alpha" %in% names(par)) {
        y <- reverse_power(pred, rp = par["alpha"])
      } else {
        y <- pred
      }      
      y <- replace(y, y > data$gamblex, data$gamblex[y > data$gamblex])
      y / data$gamblex # Scale to common range 0 - 1
    }
  fit(M, options = c(model_options))
  return(M)
}

# RF with only weighting of the probabilities (w) and no value weighting (v)
RF_w <- function(dt) {
  RF(dt = dt, fix = c(alpha=1), w = "TK1992", v = NA)
}

# RF with neither probability weighting (w) nor value weighting (v)
RF_wv <- function(dt) {
  RF(dt = dt, v = NA, w = NA)
}



# BVU Model
BVU <- function(dt, fix = NULL) {
  fixb <- fix[names(fix) %in% c("count_x", "count_0", "delta")]
  fixu <- fix[names(fix) %in% c("rp")]
  M <- cognitivemodel(data = dt) +
    bayes_beta_d(~ count_x + count_0, format = "count", fix = fixb, choicerule = "none", prior_sum = 4) +
    utility_pow_c(value_scaled ~ gamblex, fix = c(list(rn = NA), fixu)) +
    function(pred, data, par) {
      y <- data$pr_c * pred # pred = value prediction pr_c = subj probability
      # Value on original scale
      y <- reverse_power(y, rp = par["rp"])
      # Ceiling: scale down all values that exceed the max of the scale
      y <- replace(y, y > data$gamblex, data$gamblex[y > data$gamblex])
      y / data$gamblex # Scale to common range 0 - 1
    }
  fit(M, options = model_options)
  return(M)
}

BVU_a <- function(dt) {
  BVU(dt = dt, fix = list(rp = NA))
}

BVU_d <- function(dt) {
  BVU(dt = dt, fix = list(delta = 1))
}

BVU_p <- function(dt) {
  BVU(dt = dt, fix = list(count_x = 1, count_0 = 1))
}

BVU_pd <- function(dt) {
  BVU(dt = dt, fix = list(count_x = 1, count_0 = 1, delta = 1))
}

# BVU Model with delta = 1 (Bayesian delta)
BVU_ad <- function(dt) {
  BVU(dt = dt, fix = list(rp = NA, delta = 1))
}

# BVU Model with delta = 1 (Bayesian delta)
BVU_ap <- function(dt) {
  BVU(dt = dt, fix = list(rp = NA, count_x = 1, count_0 = 1))
}

# BVU Model with delta = 1 (Bayesian delta)
BVU_apd <- function(dt) {
  BVU(dt = dt, fix = list(rp = NA, count_x = 1, count_0 = 1, delta = 1))
}

# Baseline Model
BASE <- function(dt) {
  model_options$lb <- model_options$lb["sigma"] # only bound on sigma
  baseline_mean_c(value_scaled ~ ., data = dt, options = model_options)
}

# Function for cross-validation
do_cv <- function(model, train_data) {
  if (is.list(train_data)) train_data <- train_data[[1]]
  return(
    list(map(train_data$train, ~ model(dt = as.data.frame(.))))
  )
}