# ==========================================================================
# Fig 1
# ==========================================================================
# Change the working directory to the location of THIS file
source("fig_setup.R")
library(cogscimodels) #devtools::install_github("janajarcki/cogscimodels")
library(tidybayes)
library(data.table)


#1/5 rewards, n = 5, n = 30
#4/5 rewards

plot_belief_updating <- function(px, N = 10000) {
  x <- c(px * 5, px * 30)
  labels <- c("",
    paste0("Small sample (", x[1], "/5 ", "s)"),
    paste0("Large sample (", x[2], "/30 ", "s)"))
  priors <- list(Uniform = c(1,1), Gain = c(0.4,1.6), Loss = c(1.6,0.4))
  prior_labels <- gsub("c\\(", "\nBeta ~ \\(", paste(c("Prior uniform", "Prior on low outcomes", "Prior on high outcomes"), priors))
  
  d <- rbindlist(lapply(priors, function (p) data.table(
    id = 1:N,
    p = rbeta(N, p[1], p[2]),
    s = rbeta(N, x[1] + p[1], 5  - x[1] + p[2]),
    l = rbeta(N, x[2] + p[1], 30 - x[2] + p[2]))), id = "Prior")

  d <- melt(d, 1:2, variable = "type")
  d[, type_p := factor(as.numeric(type == "p"), levels =1:0, labels = c("Prior", "Posterior"))]

  d[, type := factor(type, levels = c("p", "s", "l"), labels = labels)]
  d[, Prior := factor(Prior, levels = c("Uniform", "Gain", "Loss"), prior_labels)]

  p <- ggplot(d, aes(x = value, y = 0, fill = type, color = type, shape = type_p)) +
    geom_vline(aes(xintercept = px, linetype = "True Pr(gain)"),color = "grey70") +
    stat_slabh(alpha = 0.1) +
    stat_pointintervalh(aes(y = -(as.numeric(factor(type))-0.3)/10), fill = "white", stroke = 1, size = 0.9) +
    scale_fill_viridis_d("Sample size", option = "A", direction = -1, end = 0.85, labels = c("no", "small", "large")) +
    scale_color_viridis_d("Sample size", option = "A", direction = -1, end = 0.85, labels = c("no", "small", "large")) +
    scale_shape_manual("", values = c(16, 21)) +
    scale_linetype_manual("", values = c(1)) +
    facet_wrap(~Prior, scales = "free") +
    scale_x_continuous("Belief about Pr(gain)", limits = c(0,1), expand = c(0,0))+
    ylab("Density") +
    guides(linetype = guide_legend(keywidth = unit(2, "mm"), override.aes= list(color = "grey20"), order = -2), shape =  guide_legend(title = NULL, order = 1)) +
    theme(
      aspect.ratio = 0.7,
      legend.position = "top",
      legend.just = "top",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.key = element_rect(color = NA),
      legend.box.background = element_rect(color = NA, size = 0),
      axis.ticks.y = element_blank(),
      #axis.title.y = element_blank(),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      panel.spacing = unit(12, "mm"))
  return(p)
}

N <- 50000
p_02 <- plot_belief_updating(0.2, N = N) +ggtitle("Bayesian model learning about Pr(gain) = 20%")
p_80 <- plot_belief_updating(0.8, N = N) +ggtitle("Bayesian model learning about Pr(gain) = 80%") +theme(legend.position = "none")

p_02 + p_80 +plot_layout(nrow = 2) +plot_annotation(tag_levels = "a")

ggsave("../figures/fig1.eps", width = 7, height = 5.5, device=cairo_ps)