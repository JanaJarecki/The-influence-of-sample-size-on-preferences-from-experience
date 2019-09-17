# ==========================================================================
# Cognitive model fitting
# Author: Jana B. Jarecki
# ==========================================================================
library(data.table)
library(cogscimodels)


# Load data
d <- fread('../../data/processed/study1.csv', colClasses = list(character = "id"))

#
# Data preprocessing
# --------------------------------------------------------------------------
d <- d[condition == "experience"]
d[, relfreq_x := gamblep]
d[, count_x := round(gamblep * samplesize)]
d[, count_0 := samplesize - count_x]
## Rescale values to have a range of 0 - 1
d[, value_scaled := value / gamblex]

## Set up the models
model_options <- list(
  lb = c(rp = 0, sigma = 0.0001), # parameter lower bound: alpha (rp) and sigma
  ub = c(sigma = 1), # parameter upper bound
  fit_solver = "solnp", # solver
  fit_args = list(
    options = list(pdf = "truncnormal", a = 0, b = 1))
  )

# Set up the models
RF <- function(d) {
  start(data = d) %+%
    utility_pow(value_scaled ~ gamblex, fix = list(rn = NA)) %>%
    fun(function(pred, data, par) {
      y <- data$relfreq_x * pred
      rp <- par["rp"]
      y <- (sign(rp) * y)^((1/rp)*(rp!=0)) * exp(y)^(rp==0)
      y <- replace(y, y > data$gamblex, data$gamblex[y > data$gamblex])
      y / data$gamblex
    }) %>%
    end(options = model_options)
}
BVU <- function(d) {
  start(data = d) %+%
    bayes_beta(~ count_x + count_0, format = "count") %+%
    utility_pow(value_scaled ~ gamblex, fix = list(rn = NA)) %>%
    fun(function(pred, data, par) {
      y <- data$pred_count_x * pred
      rp <- par["rp"]
      y <- (sign(rp) * y)^((1/rp)*(rp!=0)) * exp(y)^(rp==0)
      y / data$gamblex
    }) %>%
    end(options = model_options)
}
# 3-parameter version of the BVU
BVU3 <- function(d) {
  start(data = d) %+%
    bayes_beta(~ count_x + count_0, fix = list(delta = 1), format = "count") %+%
    utility_pow(value_scaled ~ gamblex, fix = list(rn = NA)) %>%
    fun(function(pred, data, par) {
      y <- data$pred_count_x * pred
      rp <- par["rp"]
      y <- (sign(rp) * y)^((1/rp)*(rp!=0)) * exp(y)^(rp==0)
      y / data$gamblex
    }) %>%
    end(options = model_options)
}
Baseline <- function(d) {
  start(data = d) %+%
  baseline_mean(value_scaled ~ ., type = "const", mode = "continuous") %>%
    end(options = model_options[-1])
}
model_list <- list(bvu = BVU3, baseline = Baseline, rf = RF)


# Estimate mdoel parameters
# separately by-participant (by id)
ids <- unique(d$id)
ids_models <- lapply(ids,
  function(i) sapply(model_list, function(m) m(d[id == i])))
names(ids_models) <- ids

## Save fitted model object
saveRDS(ids_models, "../fitted_models/study1_fitted_models.Rds")


ggplot(weights, aes(x = 1, fill = factor(winner, levels = sort(names(table(weights$winner)))))) +
  geom_bar(position = "stack") 

lapply(ids_models, function(id) lapply(id, function(m) m$AIC()))
lapply(ids_models, function(id) lapply(id, logLik))
lapply(ids_models, function(id) lapply(id, coef))
pred <- rbindlist(lapply(ids_models, function(id) lapply(id, predict)), id = "id")


w_bounds <- c(0,0.166666667, 0.375, 0.80.967741935, 1)
weights[, .SD[winner]]

pred <- lapply(ids_models, function(id) lapply(id, predict))
pred <- rbindlist(pred)

d[, colnames(pred) := pred * gamblex]
dl <- melt(d, measure = colnames(pred), value= "pred", variable = "model")

ggplot(dl, aes(value, pred, color = model)) +geom_point() +facet_wrap(~id, scales = "free")  +theme(aspect = 1)



bestmodel[which(bestmodel[,2]>=0.166666667 & bestmodel[,2] <=0.375),3] = "weak"
bestmodel[which(bestmodel[,2]>=0.375 & bestmodel[,2] <=0.8),3] = "positiv"
bestmodel[which(bestmodel[,2]>=0.8 & bestmodel[,2] <=0.967741935),3] = "strong"
bestmodel[which(bestmodel[,2]>0.967741935),3] = "very.strong"
table(weights$winner)




M <- BVU_UNIF(d)

d1 <- d[, "value", drop = F]
d1$pred <- M$predict()[,1] * d1$gamblex

hist(M$predict()[,1])
hist(d$value)

ggplot(dd, aes(value, pred)) +geom_point()

d[, names(pred) := pred]

setnames(d, make.names(d))
library(ggplot2)
ggplot(melt(d, measure = c("relfr", "bvu", "bvud1", "baseline"), val = "pred", var = "model"), aes(x = gambleid)) +geom_point(data=d[!duplicated(trial)], color = "black", size = 4, aes(y=value_scaled), shape = 21) +geom_point(aes(y = pred, shape = model, color = model)) +facet_wrap(~factor(samplesizecat, levels = c("xs", "s", "m", "l")), nrow = 1)



# modelB <- function(ss, p, out, param, prior) {  
#   a <- prior[1] + ss*p
#   b <- prior[2] + ss*(1-p)
#   val <- ((a/(a+b))*(out^param[2]))^(1/param[2])
#   val[val>out] <- out
#   return(val)
# }

# modelF <- function(p, out, param, ...) {
#   val <- (p*(out^param[2]))^(1/param[2])
#   val[val>out] <- out
#   return(val)
# }

# LogL = function(data, param, valuefunct, prior = prior){
#   p = data$gamblep #data$p
#   ss = data$samplesize #data$SS
#   out = data$outcome #data$gain
#   response = data$value #data$rating
#   pred = valuefunct(p = p, ss = ss, out = out, param = param, prior)
#   ll = log(dtruncnorm(x = response/out, a = 0, b = 1,
#                       mean = pred/out, sd = param[1]))
#   ll[ll < -9999] = -9999999
#   ll[ll > 9999] = 9999999
#   return(ll) 
# }

# min2sumLL = function(data, param, valuefunct, prior){
#   LL = numeric();
#   for(i in 1:nrow(data)){
#     LL[i] = LogL(data = data[i,], param = param, valuefunct = valuefunct, prior = prior)
#   }
#   return((-2) * sum(LL))
# }

# sds <- seq(.00001, .3, .01)
# alphas <- seq(0.1, 3, .025)
# N <- length(unique(d$id))

# # Initialize result objects
# LL.mB_unif <- LL.mB_bi <- LL.mB_gain <- LL.mB_loss<- LL.mF <- array(NA, dim = c(length(sds), length(alphas), N))
# res.mF <- res.mB_bi <- res.mB_loss <- res.mB_gain <- data.frame(matrix(NA, N, 3, dimnames = list(NULL, c("min2sumLL", "param.sd", "param.alpha"))))


# # Run the models
# for(i in unique(d$id)) { 
#   print(Sys.time())
#   k <- d[id == i]
#   for(j in 1:length(sds)){
#     for(p in 1:length(alphas)){
#       LL.mB_unif[j,p,i] <- min2sumLL(data = k, param = c(sds[j], alphas[p]), prior = c(1,1), valuefunct = modelB)
#       LL.mB_bi[j,p,i] <- min2sumLL(data = k, param = c(sds[j], alphas[p]), prior = c(.5,.5), valuefunct = modelB)
#       LL.mB_gain[j,p,i] <- min2sumLL(data = k, param = c(sds[j], alphas[p]), prior = c(2-.01,.01), valuefunct = modelB)
#       LL.mB_loss[j,p,i] <- min2sumLL(data = k, param = c(sds[j], alphas[p]), prior = c(.01,2-.01), valuefunct = modelB)      
#       LL.mF[j,p,i] <- min2sumLL(data = k, param = c(sds[j], alphas[p]),  valuefunct = modelF)      
#     }
#   }
  
#   LL.mB_unif[LL.mB_unif == Inf] = NA
#   LL.mB_bi[LL.mB_bi == Inf] = NA
#   LL.mB_gain[LL.mB_gain == Inf] = NA
#   LL.mB_loss[LL.mB_loss == Inf] = NA
#   LL.mF[LL.mF == Inf] = NA
  
  
#   res.mB_unif$param.sd[i] <- sds[which(LL.mB_unif[,,i] == min(LL.mB_unif[,,i], na.rm = TRUE), arr.ind = TRUE)[,1]]
#   res.mB_unif$param.alpha[i]<- alphas[which(LL.mB_unif[,,i] == min(LL.mB_unif[,,i], na.rm = TRUE), arr.ind = TRUE)[,2]]
#   res.mB_unif$min2sumLL[i] <- min(LL.mB_unif[,,i], na.rm = T)
  
  
#   res.mB_bi$param.sd[i] <- sds[which(LL.mB_bi[,,i] == min(LL.mB_bi[,,i], na.rm = TRUE), arr.ind = TRUE)[,1]]
#   res.mB_bi$param.alpha[i]<- alphas[which(LL.mB_bi[,,i] == min(LL.mB_bi[,,i], na.rm = TRUE), arr.ind = TRUE)[,2]]
#   res.mB_bi$min2sumLL[i] <- min(LL.mB_bi[,,i], na.rm = T)
  
  
#   res.mB_gain$param.sd[i] <- sds[which(LL.mB_gain[,,i] == min(LL.mB_gain[,,i], na.rm = TRUE), arr.ind = TRUE)[,1]]
#   res.mB_gain$param.alpha[i]<- alphas[which(LL.mB_gain[,,i] == min(LL.mB_gain[,,i], na.rm = TRUE), arr.ind = TRUE)[,2]]
#   res.mB_gain$min2sumLL[i] <- min(LL.mB_gain[,,i], na.rm = T)
  
  
#   res.mB_loss$param.sd[i] <- sds[which(LL.mB_loss[,,i] == min(LL.mB_loss[,,i], na.rm = TRUE), arr.ind = TRUE)[,1]]
#   res.mB_loss$param.alpha[i]<- alphas[which(LL.mB_loss[,,i] == min(LL.mB_loss[,,i], na.rm = TRUE), arr.ind = TRUE)[,2]]
#   res.mB_loss$min2sumLL[i] <- min(LL.mB_loss[,,i], na.rm = T)
  
  
  
#   res.mF$param.sd[i] <- sds[which(LL.mF[,,i] == min(LL.mF[,,i], na.rm = TRUE),
#                                   arr.ind = TRUE)[,1]]
#   res.mF$param.alpha[i]<- alphas[which(LL.mF[,,i] == min(LL.mF[,,i], na.rm = TRUE), arr.ind = TRUE)[,2]]
#   res.mF$min2sumLL[i] <- min(LL.mF[,,i], na.rm = T)
#   print(i)  
# }