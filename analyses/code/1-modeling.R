# ==========================================================================
# Cognitive model fitting
# Author: Jana B. Jarecki
# ==========================================================================
library(data.table)
library(cogscimodels)


# Load data
d <- fread('../../data/processed/study1.csv', colClasses = list(character = "id"))

# Data preprocessing
# --------------------------------------------------------------------------
d <- d[condition == "experience"]
d[, relfreq_x := gamblep]
d[, count_x := round(gamblep * samplesize)]
d[, count_0 := samplesize - count_x]
## Rescale values to have a range of 0 - 1
d[, value_scaled := value / gamblex]


# Combine the models into a list
source("setup_models.R")
model_list <- list(bvu = BVU, baseline = Baseline, rf = RF)


# Fit the model parameters by = id (by-participant)
# --------------------------------------------------------------------------
# Estimate mdoel parameters separately by=id
modelfit <- d[, .(model=names(model_list), fit=lapply(model_list, do.call, args = list(d=.SD))), by=id]
# Save resulting object
saveRDS(modelfit, "study1_cognitive_smodels_fit.Rds")

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