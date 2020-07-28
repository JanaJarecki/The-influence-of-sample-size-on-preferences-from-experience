library(themejj)
library(ggplot2)
library(scales)
library(patchwork)
theme_set(themejj(base_family = "Arial"))

model_levels <- c("bvu", "rf", "base")
model_labels <- c(bvu="BVU", rf="RF", base="BASE")
model_colors <- c(BASE = "#f3f3f3", BVU = "#252629", RF = "#afb2b7")
model_colors_compl <- c(BASE = "#252629", BVU = "white", RF = "#252629")

order_evidence <- function(id, evidence) {
  evidence <- as.matrix(evidence)
  winners <- apply(evidence, 1, which.max)
  o <- sort(unique(winners))[order(table(winners), decreasing = TRUE)]
  y <- NULL
  for (i in o) {
    y <- c(y, id[winners==i][order(evidence[winners==i,i], decreasing = TRUE)])
  }
  return(y)
}

  theme_update(
    legend.position = "right",
    legend.direction = "vertical",
    legend.box.background = element_rect(size = 0.4, color = "black", fill = NA),
    legend.margin = margin(rep(0.4,4), unit="lines"),
    legend.key.width = unit(1, "lines"),
    legend.box.margin = margin(rep(0.3,4), unit = "lines"),
    legend.spacing.x = unit(0.4, "lines"),
    legend.text = element_text(margin = margin(r = 0.4, unit = "lines")),
    legend.key = element_rect(color = "white", size = 1)
    )