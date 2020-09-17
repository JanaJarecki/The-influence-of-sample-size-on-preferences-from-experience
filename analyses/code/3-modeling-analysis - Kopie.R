# ==========================================================================
# Analyze the results of the cogonitive model fiting
# ==========================================================================
pacman::p_load(data.table, purrr, modelr)
pacman::p_load_gh("janajarecki/cognitivemodels@development",
  "janajarecki/cognitiveutils")
source("setup_models.R")


## ---- load ----
# Load data -----------------------------------------------------------------
study <- 1
fit <- readRDS(sub("X", study ,"../fittedmodels/studyX_cognitive_models.rds"))


## ---- gof ----
# Goodness of model fit -----------------------------------------------------
models <- c("bvu", "rf", "base")
# BIC
bic <- fit[model %in% models][, .(gof = map_dbl(V2, function(x) stats::BIC(x$logLik()))), by = .(id, model)]

# AIC
aic <- fit[model %in% models][, .(gof = map_dbl(V2, function(x) stats::AIC(x$logLik()))), by = .(id, model)]

# AICweights
aicw <- aic[, .(model, w = akaike_weight(gof)), by = id]
weights <- dcast(aicw, id ~ model, value.var = "w")
weights[, winner := factor(which.max(.SD), 1:3, names(.SD)), by = id]
winners <- sort(table(weights$winner))
weights[, winner := relevel(winner, names(winners)[which.max(winners)])]

# save to results object
R <- readRDS("../../manuscript/results1.rds")
R$aic <- aic[, mean(gof), by = model][, setNames(V1, model)]
R$bic <- bic[, mean(gof), by = model][, setNames(V1, model)]
R$winners <- winners
saveRDS(R, file = "../../manuscript/results1.rds", version = 2, ascii = TRUE)

# plot
source("fig2.R")
ggsave("../figures/fig2-1.pdf", fig, w = 7, h = 3)




## ---- par ----
# Model parameter -------------------------------------------------------------
fits <- fits[weights[, c("id", "winner")], on = .NATURAL]
fits[, fit_winner := .SD[, which(names(.SD) == winner), with = FALSE], by = .(id, winner)]
parameter <- fits[, .(par = names(coef(fit_winner[[1]])), val = coef(fit_winner[[1]])), by = .(id, winner)]
dcast(parameter, winner ~ par, fun.aggregate = paste_msd)


# Prediction data
pred <- fits[, fit_winner[[1]]$predict(), by = id]
setnames(pred, "V1", "pred_scaled")
d[condition == "experience", pred_scaled := ..pred[, pred_scaled]]
d <- weights[, c("id", "winner")][d, on = "id"]
d[, pred := pred_scaled * gamblex]
d[, samplesizecat := factor(samplesizecat, levels = c("xs", "s", "m", "l", "--"), ordered = TRUE)]

r_conf_bym <- d[condition == "experience", cor(as.numeric(samplesizecat), confidence, method = "kendall"), by = .(id, winner)][, dcast(.SD, 1 ~ winner, paste_msd, label = TRUE)]

d <- parameter[par=="count_x", c("id", "val", "winner")][d, on = c("id", "winner")]

## Confidence ratings by best-fitting model
d[condition == "experience", mean(confidence), by = .(id, winner, samplesizecat)][, rank(V1), by = .(winner, id)]
d[condition == "experience",
  .(M = mean(confidence),
    SD = sd(confidence)), by = .(winner, samplesizecat)]


# Analysis of the Bayesian model with delta = 1 -------------------------------
  # Evidence-weights from log likelihood
ll[, w := cognitiveutils::akaike_weight(Median, "log"), by = id]
weights_d1 <- dcast(ll, id ~ model, value.var = "w")
weights_d1[, winner := names(.SD)[which.max(.SD)], by = id]
winners_d1 <- sort(table(weights_d1$winner))




aic_d1 <- fits[, lapply(.SD, function(x) AIC(x[[1]])), .SDcols =  c("base", "bvud1", "rf"), by = id]

# AIC-weights
weights_d1 <- copy(aic_d1)
weights_d1[, c("bvud1", "rf", "base") := as.list(cognitiveutils::akaike_weight(c(bvud1, rf, base))), by = id]
weights_d1[, winner := names(.SD)[which.max(.SD)], by = id] # winning models
weights_d1$winner <- factor(weights_d1$winner, levels = c("bvud1", "rf", "base"))
winners_d1 <- sort(table(weights_d1$winner))


# Parameter for appendix

par <- modelfit[,
  .(par=names(coef(fit[[1]])), value=coef(fit[[1]])),
  by=.(id,model)]
par[par=="rp", par := "alpha"]
par <- dcast(par, model ~ par, value.var = "value", fun.aggregate = mean)
par[]
BIC = modelfit[, .(BIC = BIC(fit[[1]]), LL = logLik(fit[[1]])), by = .(id,model)]
par[BIC[, .(BIC=mean(BIC), LL = mean(LL)), by=model]][order(BIC)]
