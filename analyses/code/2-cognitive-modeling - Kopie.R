# ==========================================================================
# Cognitive model fitting
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(data.table, modelr, purrr) # install.packages("pacman")
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
  bvu = BVU,
  bvu_d1 = BVU_d1,
  base = BASE,
  rf = RF
  )

# Cross-validation ------------------------------------------------------
# CV data for each id separately
set.seed(42)
cv_data <- d[, .(cvfold = list(crossv_kfold(.SD, 10))), by = id]

# Perform modeling with the cross-validation
if (parallel == TRUE) {
  pacman::p_load(doMC)
  registerDoMC(cores = detectCores())
  setkey(d, "id")

  modelfit <- foreach(
    x = unique(d[["id"]]), .combine="rbind", .inorder=FALSE) %dopar% {
      cv_data[.(x),
        (names(model_list)) := lapply(model_list, do_cv, cvfold),
        by = id]
  }
}
if (parallel == FALSE) {
  modelfit <- cv_data[,
    (names(model_list)) := lapply(model_list, do_cv, cvfold),
    by = id]
}



# Save resulting object that holds the models
saveRDS(modelfit,sub("X",study,"../fittedmodels/studyX_cognitive_models.rds"))