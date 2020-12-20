# ==========================================================================
# Cognitive model fitting
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(data.table, modelr, purrr) # install.packages("pacman")
pacman::p_load_gh("janajarecki/cognitivemodels@development")
parallel <- TRUE # fit on a parallel machine (Unix) or single core
if (parallel == TRUE) { pacman::p_load(doFuture) }

# Load data -----------------------------------------------------------------
study <- 2
path <- sub("X", study, "../../data/processed/studyX.csv")
d <- fread(path, colClasses = list(character = "id"))

# Data preprocessing --------------------------------------------------------
d <- d[condition == "experience"]
d[, relfreq_x := gamblep]
d[, count_x := round(gamblep * samplesize)]
d[, count_0 := samplesize - count_x]
# Rescale respondents' values to have a range of 0 - 1
d[, value_scaled := value / gamblex]



# Modeling ------------------------------------------------------------------
source("setup_models.R")
model_list <- list(
  bvu = BVU #,          # Bayesian value updating model
  # bvu_d = BVU_d,    # Bayesian value updating model with delta = 1
  # # bvu_a = BVU_a,
  # # bvu_p = BVU_p,
  # # bvu_ad = BVU_ad,
  # # bvu_ap = BVU_ap,
  # # bvu_pd = BVU_pd,
  # # bvu_apd = BVU_apd,
  # rf = RF,# Relative frequency model
  # # rf_w = RF_w,
  # # rf_wv = RF_wv,
  # base = BASE        # Baseline model
  )

# Cross-validation ----------------------------------------------------------
# Createy CV data  by participant id
# set.seed(42)
# k <- 20 # how many folds
# cv_data <- d[, .(cvfold = list(crossv_kfold(.SD, k))), by = id]

if (parallel == TRUE) {
  # registerDoMC(cores = detectCores())
  registerDoFuture()
  plan(multisession)  ## on MS Windows
  plan(multisession) 
  setkey(d, "id")  
  modelfit <- foreach(x = unique(d$id),
                      .combine = "rbind",
                      .inorder = FALSE, 
                      .packages = c("data.table", "cognitivemodels", "modelr"),
                      .export = c("model_list", "model_options", "reverse_power", "BVU")) %dopar% {
                        source("setup_models.R", local = TRUE)
                        d[.(x), .(
                          model = names(model_list),
                          map(model_list, exec, dt=.SD)), by = id]
                      }   
  
#  modelfit <- modelfit[cv_data, on = "id"]
  
} else {
  modelfit <- d[, .(
    model = names(model_list),
    fit = map(model_list, exec, dt=.SD)),
      by = id]
}



# Save resulting object that holds the models -------------------------------
saveRDS(modelfit,sub("X",study,"../fittedmodels/studyX_cognitive_models.rds"))
