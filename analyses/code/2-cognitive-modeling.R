# ==========================================================================
# Cognitive model fitting
# Author: Jana B. Jarecki
# ==========================================================================
library(cognitivemodels)
library(data.table)

# Load data -----------------------------------------------------------------
study <- 2
path <- sub("x", study, "../../data/processed/studyx.csv")
d <- fread(path, colClasses = list(character = "id"))

# Data preprocessing -------------------------------------------------------
d <- d[condition == "experience"]
d[, relfreq_x := gamblep]
d[, count_x := round(gamblep * samplesize)]
d[, count_0 := samplesize - count_x]
## Rescale values to have a range of 0 - 1
d[, value_scaled := value / gamblex]
dt <- d[id=="s41"]

# Modeling -------------------------------------------------------------------
# Combine the models into a list
source("setup_models.R")
model_list <- list(bvu = BVU, bvud1 = BVU_d1, base = Baseline, rf = RF)
# Estimate models by = id (by-participant)
modelfit <- d[id == "s41", .(model=names(model_list), fit=lapply(model_list, do.call, args = list(d=.SD))), by=id]
# Save resulting object
saveRDS(modelfit, sub("x", study, "../modelfits/studyx_cognitive_models_fit.rds"))