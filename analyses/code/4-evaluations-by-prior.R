# ==========================================================================
# Judgments by prior and sample size
# Pooled across both studies
# ==========================================================================
require(data.table)
require(brms)
options(contrasts = c("contr.sum", "contr.poly"))

# Load data of both studies
d <- rbind(
  fread("../../data/processed/study1.csv"),
  fread("../../data/processed/study2.csv"))
fits <- rbind(
  readRDS("study1_cognitive_models_fit.rds"),
  readRDS("study2_cognitive_models_fit.rds"))
fits <- dcast(fits, id ~ model, value.var = "fit")
setnames(fits, "baseline", "base")
d <- d[condition == "experience"]

# Goodness of model fit
weights <- fits[, as.data.table(cbind(model=c("base","rf","bvu"), anova(base[[1]], bvu[[1]], rf[[1]])[, c("wAIC"), drop=FALSE])), by = id]
weights <- dcast(weights, id ~ model, value.var = "wAIC")
weights[, winner := names(.SD)[which.max(.SD)], by = id]
# Merge
d <- d[weights[, c("id","winner")], on = "id"]
# Melt
fits <- melt(fits, 1, variable = "model", value = "fit")
# Obtain best-fitting parameters
parameter <- weights[, c("id", "winner")][fits, on = "id"][winner==model]
parameter <- parameter[, .(par = names(coef(fit[[1]])), val = coef(fit[[1]])), by = .(id, winner)]
d <- parameter[par == "count_x"][d, on = c("id", "winner")]
# z-standardize and scale value
d[, value_scaled := scale(value / gamblex), by = id]
# Factor for modeling
d[, id := factor(id)]
d[, samplesizecat_num := as.numeric(factor(samplesizecat, levels = c("xs", "s","m","l")))]
d[winner == "bvu", priorcat := cut(val, c(0,1,2))]
d[, priorx_cat := factor(priorcat, labels = c("zero-outcome", "gain", "no"), exclude = NULL)]
d[, gambletype := factor(gambletype, levels = c("p-bet", "$-bet"))]  

# Fit full model
fit_value_prior <- brm(
  formula = value_scaled ~ samplesizecat_num * priorx_cat * gambletype + (1 | id),
  data   = d,
  cores  = 3,
  save_all_pars = TRUE,
  iter = 5000,
  file = "study12_bayes_models_fit.rds")

# Fit model without priorx_cat
fit_value_noprior <- update(fit_value_prior,
  formula = ~ samplesizecat_num * gambletype + (1 | id),
  file = "study12_bayes_models_fit_noprior.rds")
BF_value_prior <- bayes_factor(fit_value_prior, fit_value_noprior)$bf
class(BF_value_prior) <- "BF"


plot(fit_value_prior)
hypothesis(fit_value_prior, "samplesizecat_num:priorx_cat1 > 0")
hypothesis(fit_value_prior, "samplesizecat_num:priorx_cat2 < 0")
hypothesis(fit_value_prior, "samplesizecat_num > 0")

#
# Analysis of confidenced
# --------------------------------------------------------------------------
d[, conf_scaled := scale(confidence), by = id]
d[is.na(conf_scaled), conf_scaled := 0]


# Fit full model
fit_value_prior <- brm(
  formula = conf_scaled ~ samplesizecat_num * winner * gambletype + (1 | id),
  data   = d,
  cores  = 3,
  save_all_pars = TRUE,
  #iter = 5000,
  file = "study12_bayes_models_fit_conf")

# Fit model without priorx_cat
fit_value_noprior <- update(fit_value_prior,
  save_all_pars = TRUE,
  formula = ~ samplesizecat_num * gambletype + (1 | id),
  file = "study12_bayes_models_fit_conf_noprior")
BF_conf_prior <- bayes_factor(fit_value_prior, fit_value_noprior, maxiter = 6000)$bf
class(BF_conf_prior) <- "BF"
