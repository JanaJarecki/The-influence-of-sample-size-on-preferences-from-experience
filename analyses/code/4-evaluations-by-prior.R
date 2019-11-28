# ==========================================================================
# Judgments by prior and sample size
# Pooled across both studies
# ==========================================================================
require(data.table)
require(BayesFactor)

# Options: set sum contrasts
#          to ease interpretastion of higher-order interactions
options(contrasts = c("contr.sum", "contr.poly"))

# Load and merge data of both studies
d <- rbind(
  fread("../../data/processed/study1.csv"),
  fread("../../data/processed/study2.csv"))
# Load and merge model fits of both studies
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

# Format variables as factors for the linear modeling
d[, id := factor(id)]
d[, samplesizecat := factor(samplesizecat, levels = c("xs", "s", "m", "l"), ordered = TRUE)]
d[, samplesizecat_num := as.numeric(samplesizecat)]
d[winner == "bvu", priorcat := cut(val, c(0,1,2))]
d[, priorx_cat := factor(priorcat, labels = c("zero-outcome", "gain", "no"), exclude = NULL)]
d[, gambletype := factor(gambletype, levels = c("p-bet", "$-bet"))]  

# ------------------------------------------------------------------------------
# # Fit full model with brms
# fit_value_prior <- brm(
#   formula = value_scaled ~ samplesizecat_num * priorx_cat * gambletype + (1 | id),
#   data   = d,
#   cores  = 3,
#   save_all_pars = TRUE,
#   iter = 5000,
#   file = "study12_bayes_models_fit.rds")

# # Fit model without priorx_cat
# fit_value_noprior <- update(fit_value_prior,
#   formula = ~ samplesizecat_num * gambletype + (1 | id),
#   file = "study12_bayes_models_fit_noprior.rds")
# ------------------------------------------------------------------------------

# ## Bayes factor of full model against null
# bfFull = generalTestBF(
#   formula = value_scaled ~ samplesizecat_num * priorx_cat * gambletype + id,
#   data = d,
#   whichRandom = "id",
#   neverExclude="id")
# bfFull <- recompute(bfFull, iterations = 500000)
# saveRDS(bfFull, "../modelfits/regression_evaluations_by_prior.rds")
bfFull <- readRDS("../modelfits/regression_evaluations_by_prior.rds")
# [15] samplesizecat_num + priorx_cat + samplesizecat_num:priorx_cat + gambletype + priorx_cat:gambletype + id 
# [13] priorx_cat + gambletype + priorx_cat:gambletype + id
BF_value_prior <- extractBF(bfFull[15] / bfFull[13], onlybf = TRUE)
class(BF_value_prior) <- "BF"
# [14] samplesizecat_num + priorx_cat + gambletype + priorx_cat:gambletype + id
# bfFull[15] / bfFull[14]


#
# Analysis of confidenced
# --------------------------------------------------------------------------
d[, conf_scaled := scale(confidence), by = id]
d[is.na(conf_scaled), conf_scaled := 0]
# Bayes factor of full model against null
# bfFull = generalTestBF(
#   formula = conf_scaled ~ samplesizecat_num * winner * gambletype + id,
#   data = d,
#   whichRandom = "id",
#   neverExclude = "id")
# # bfFull <- recompute(bfFull, iterations = 500000)
# saveRDS(bfFull,   "../modelfits/regression_confidence_by_prior.rds")
bfFull <- readRDS("../modelfits/regression_confidence_by_prior.rds")
# [15] samplesizecat_num + priorx_cat + samplesizecat_num:priorx_cat + gambletype + priorx_cat:gambletype + id
head(bfFull, 2)
BF_conf_prior <- extractBF(bfFull[15], onlybf = TRUE)
class(BF_conf_prior) <- "BF"