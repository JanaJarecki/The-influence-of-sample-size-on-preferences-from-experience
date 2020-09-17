# ==========================================================================
# Figure displaying the individual model fits
# ==========================================================================
source("setup_fig.R")
# ---- fig_modelfit ----
N <- length(unique(weights$id))
Nm <- ncol(weights) - 2
m <- setdiff(names(weights), c("id", "winner"))

fig <- ggplot(melt(weights, id = "id", measure = m), aes(x = factor(id, levels = weights[, .(id[order(-get(models[1]))]), keyby=winner]$V1),  y = value, fill = factor(variable, c("bvu", "base", "rf"), c("BVU", "BASE", "RF")))) +
    geom_bar(stat = "identity", color = "white", size = 0.01) +
    geom_hline(yintercept = c(.50), linetype = 2, color = "white", alpha = 0.8, size = 0.4) +
    scale_fill_manual("Model", values = model_colors) +
    scale_y_continuous("Evidence Strength", expand  = c(0,0), labels = percent, breaks = c(0,0.5,1)) +
    xlab(paste("Participant, N =", N)) +
    guides(fill = guide_legend(override.aes = list(linetype = 0, size = 4.5))) +
    theme(axis.ticks = element_blank(), 
      axis.text.x=element_blank(),
      legend.position = "right",
      axis.line = element_blank(),
      #axis.text.y = element_text(vjust = c(0,.5,.5,.5,1)),
      axis.title.x = element_text(margin = margin(t = 0.5, unit = "lines")),
      legend.key.height = unit(1.2, "lines"),
      legend.key = element_rect(fill = "white"))