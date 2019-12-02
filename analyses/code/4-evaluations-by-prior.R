# ==========================================================================
# Judgments by prior and sample size
# Pooled across both studies
# ==========================================================================
# require(data.table)
# require(BayesFactor)

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

# Goodness of model fit
weights <- fits[, as.data.table(cbind(model=c("base","rf","bvu"), anova(base[[1]], bvu[[1]], rf[[1]])[, c("wAIC"), drop=FALSE])), by = id]
weights <- dcast(weights, id ~ model, value.var = "wAIC")
weights[, winner := names(.SD)[which.max(.SD)], by = id]
d <- d[weights[, c("id", "winner")], on = "id"]

# Exclude condition and participants
d <- d[condition == "experience"]
d <- d[!id %in% c("s05", "s19", "s24", "s38", "s42", "s47", "s54", "s67")]
d <- d[winner != "base"]
N <- d[, length(unique(id))]

# Obtain best-ftting parameter from fits and merge them in
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
d[, priorx_cat := factor(priorcat, labels = c("zero", "gain", "no"), exclude = NULL)]
d[, gambletype := factor(gambletype, levels = c("p-bet", "$-bet"))] 
d[, winner := factor(winner, levels = c("rf", "bvu"))]
setnames(d, c("samplesizecat_num", "priorx_cat", "gambletype", "winner"), c("ss", "prior", "type", "model")) 

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
#   formula = value_scaled ~ ss*type*prior + id,
#   data = d,
#   whichRandom = "id",
#   neverExclude = "id")
# # bfFull <- recompute(bfFull, iterations = 500000)
# saveRDS(bfFull, "../modelfits/regression_evaluations_by_prior.rds")
bfFull <- readRDS("../modelfits/regression_evaluations_by_prior.rds")
# The Bayes factor in favor of the full model
BF_value1 <- extractBF(bfFull["ss + type + prior + ss:prior + type:prior + id"] / bfFull["ss + type + prior + type:prior + id"], onlybf = TRUE)
# The Bayes factor in favor of the full model over a model without prior:type
BF_value2 <- extractBF(bfFull["ss + type + prior + ss:prior + type:prior + id"] / bfFull["ss + type + ss:type + prior + id"], onlybf = TRUE)
class(BF_value1) <-  class(BF_value2) <- "BF"


# A noninformative Jeffreys prior is placed on the variance of the normal population, while a Cauchy prior is placed on the standardized effect size. The rscale argument controls the scale of the prior distribution, with rscale=1 yielding a standard Cauchy prior. See the references below for more details.
# For the rscale argument, several named values are recognized: "medium", "wide", and "ultrawide". These correspond to r scale values of sqrt(2)/2, 1, and sqrt(2) respectively.

modal <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}


# fitValue <- lmBF(value_scaled ~ ss + type + prior + ss:prior + type:prior + id,
#   data = d,
#   whichRandom = "id")
# chains = posterior(fitValue, iterations = 10000)
# saveRDS(chains, "../modelfits/regression_evaluations_chains.rds")
chains <- readRDS("../modelfits/regression_evaluations_chains.rds")
format_mcmc_beta <- function(x) {paste0("$=", sprintf("%.2f", modal(x)), "$ (89\\% HDI $", paste(sprintf("%.2f", bayestestR::hdi(as.numeric(x))[-1]), collapse = ", "), "$)")}
# The marginal coefficient of p-bet x loss prior
cc <- rowSums(chains[, c("ss:prior-zero", "type:prior-p-bet.&.zero")])
b_pz <- format_mcmc_beta(cc)
# The marginal coefficient of p-bet x gain prior
cc <- rowSums(chains[, c("ss:prior-gain", "type:prior-p-bet.&.gain")])
b_pg <- format_mcmc_beta(cc)
# The marginal coefficient of p-bet x no prior
cc <- rowSums(chains[, c("ss:prior-no", "type:prior-p-bet.&.no")])
b_pn <- format_mcmc_beta(cc)
# The marginal coefficient of p-bet x loss prior
cc <- rowSums(chains[, c("ss:prior-zero", "type:prior-$-bet.&.zero")])
b_dz <- format_mcmc_beta(cc)
# The marginal coefficient of p-bet x gain prior
cc <- rowSums(chains[, c("ss:prior-gain", "type:prior-$-bet.&.gain")])
b_dg <- format_mcmc_beta(cc)
# The marginal coefficient of p-bet x no prior
cc <- rowSums(chains[, c("ss:prior-no", "type:prior-$-bet.&.no")])
b_dn <- format_mcmc_beta(cc)

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
# # # bfFull <- recompute(bfFull, iterations = 500000)
# saveRDS(bfFull,   "../modelfits/regression_confidence_by_prior.rds")
bfFull <- readRDS("../modelfits/regression_confidence_by_prior.rds")
# [15] samplesizecat_num + priorx_cat + samplesizecat_num:priorx_cat + gambletype + priorx_cat:gambletype + id
head(bfFull, 2)
BF_conf_prior <- extractBF(bfFull[15], onlybf = TRUE)
class(BF_conf_prior) <- "BF"