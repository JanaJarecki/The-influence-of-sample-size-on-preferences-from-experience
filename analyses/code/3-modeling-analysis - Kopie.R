# ==========================================================================
# Analyze the results of the cogonitive model fiting
# ==========================================================================
pacman::p_load(data.table, purrr, modelr)
pacman::p_load_gh("janajarecki/cognitivemodels@development")
source("setup_models.R")


## ---- load ----
# Load data -----------------------------------------------------------------
study <- 1
fit <- readRDS(sub("X", study ,"../fittedmodels/studyX_cognitive_models.rds"))
fit <- melt(fit, id = c("id", "cvfold"), var = "model", val = "cv")


## ---- gof ----
# Goodness of model fit -----------------------------------------------------
rmse <- fit[, as.list(summary(map2_dbl(cv[[1]], cvfold[[1]]$test, rmse))), by = .(model, id)]

rmse[model != "bvu_d1", model[which.min(Median)], by = id][, table(V1)]
rmse[model != "bvu_d1", model[which.min(Mean)], by = id][, table(V1)]
rmse[, mean(Median), by = variable]


ll <- fit[, as.list(summary(map2_dbl(cv[[1]], cvfold[[1]]$test, logLik))), by = .(model, id)]
ll[model != "bvu_d1", model[which.max(Median)], by = id][, table(V1)]

# Evidence-weights from rmse
ll[model != "bvu_d1", w := cognitiveutils::akaike_weight(Median, "log"), by = id]
weights <- dcast(ll[model != "bvu_d1"], id ~ model, value.var = "w")

# Evidence-weights from rmse
rmse[model != "bvu_d1", w := 1 - (Mean-min(Mean)) / sum(Mean-min(Mean)), by = id]
weights <- dcast(rmse[model != "bvu_d1"], id ~ model, value.var = "w")
weights[, winner := names(.SD)[which.max(.SD)], by = id]
weights[, winner := factor(winner, levels = c("bvu", "rf", "base"))]
winners <- sort(table(weights$winner))
source("fig2.R")


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
aic_d1 <- fits[, lapply(.SD, function(x) AIC(x[[1]])), .SDcols =  c("base", "bvud1", "rf"), by = id]

# AIC-weights
weights_d1 <- copy(aic_d1)
weights_d1[, c("bvud1", "rf", "base") := as.list(cognitiveutils::akaike_weight(c(bvud1, rf, base))), by = id]
weights_d1[, winner := names(.SD)[which.max(.SD)], by = id] # winning models
weights_d1$winner <- factor(weights_d1$winner, levels = c("bvud1", "rf", "base"))
winners_d1 <- sort(table(weights_d1$winner))