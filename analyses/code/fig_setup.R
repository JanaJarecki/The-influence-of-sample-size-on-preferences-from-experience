library(themejj)
library(ggplot2)
library(scales)
library(patchwork)
theme_set(themejj(base_family = "Arial"))

model_levels <- c("bvu", "bvu3", "rf", "baseline")
model_labels <- c(bvu="BVU", bvu3="BVU3", rf="RF", baseline="BASE")
model_labels_bquote <- c(bvu="BVU", bvu3=bquote("BVU"~delta==.(1)), rf="RF", baseline="BASE")
model_colors <- c(BASE = "grey", BVU = "black", BVU3 = "grey35", RF = "grey97")

order_evidence <- function(id, evidence) {
  evidence <- as.matrix(evidence)
  winners <- apply(evidence, 1, which.max)
  winners <- factor(winners, levels = 1:ncol(evidence))
  o <- sort(levels(winners))[order(table(winners), decreasing = TRUE)]
  o <- as.numeric(o)
  y <- NULL
  for (i in o) {
    y <- c(y, id[winners==i][order(evidence[winners==i,i], decreasing = TRUE)])
  }
  return(y)
}