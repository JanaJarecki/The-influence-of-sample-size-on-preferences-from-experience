# ==========================================================================
# Cognitive model fitting
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(data.table) # install.packages("pacman")
pacman::p_load_gh("janajarecki/cognitivemodels")
parallel <- TRUE # whether to fit in parallel on a Unix server

# Load data -----------------------------------------------------------------
study <- 2
path <- sub("X", study, "../../data/processed/studyX.csv")
d <- fread(path, colClasses = list(character = "id"))

# Data preprocessing -------------------------------------------------------
d <- d[condition == "experience"]
d[, relfreq_x := gamblep]
d[, count_x := round(gamblep * samplesize)]
d[, count_0 := samplesize - count_x]
# Rescale respondents' values to have a range of 0 - 1
d[, value_scaled := value / gamblex]


# Modeling -------------------------------------------------------------------
source("setup_models.R")
model_list <- list(
  bvu = BVU
  #bvud1 = BVU_d1, base = BASE,
  #rf = RF
  )
# Estimate model parameter by = id (by-participant)
if (parallel == FALSE) {
  modelfit <- d[, .(model=names(model_list), fit=lapply(model_list, do.call, args = list(d=.SD))), by=id]
} else {
  # Estimate models in parallel
  pacman::p_load(doMC)
  registerDoMC(cores = detect.Cores())
  setkey(d, "id")
  modelfit <- foreach(x=unique(d[["id"]]),.combine="rbind",.inorder=FALSE) %dopar% d[.(x), .(model=names(model_list), fit=lapply(model_list, do.call, args = list(d=.SD)))]
}



# Save resulting object that holds the models
fit <- readRDS(sub("X",study,"../fittedmodels/studyX_cognitive_models.rds"))
fit <- fit[!model %in% names(model_list)]
modelfit <- rbind(modelfit, fit)
setkey(modelfit, id)
saveRDS(modelfit,sub("X",study,"../fittedmodels/studyX_cognitive_models.rds"))