# ==========================================================================
# Figure: Influence of sample size on stand. valuations by cogn. model
# ==========================================================================
## ---- fig6 ----
# source("4-evaluations-by-prior.R", chdir = TRUE)
# source("setup_fig.R")

# Melt
dpredagg <- melt(d[condition == "experience"], c("id", "type", "ss", "prior", "model"), c("value_scaled", "conf_scaled"))
dpredagg[, variable := factor(variable, levels = c("value_scaled", "conf_scaled"), labels = c("Evaluation", "Confidence"))]
dpredagg <- dpredagg[model != "base"]

cols <- c("#B8DE29", "#1F968B", "#450D54")

plot_it <- function(x) {
    if (x == "Evaluation") {
        dpredagg <- dpredagg[, .(M = mean(value), SE = sd(value)/sqrt(.N)),
            by = .(type, ss, prior, model, variable)]
    } else {
        dpredagg <- dpredagg[, .(M = mean(value), SE = sd(value)/sqrt(.N)),
            by = .(type, ss, model, variable)]
    }
    dpredagg[, ss := factor(ss, labels = c("xs","s","m","l"))]
    dpredagg[, model := factor(model, model_levels, model_labels)]
    dpredagg[, type := factor(type, levels = c("p-bet", "$-bet"), labels = c("p-bet\n(high pr., low gain)", "$-bet\n(low pr., high gain)"))]
    
    p <- ggplot(dpredagg[variable == x], aes(ss, M)) +
        facet_wrap(~ model, scales = "free_y") +
        scale_shape_manual("Gamble Type", values = c(19,21)) +
        scale_linetype_manual("Winning Model", values = c(1,2,3)) +
        scale_fill_manual("Winning Model", values = model_colors) +
        scale_color_manual("Prior", values = cols) +
        scale_x_discrete("Sample Size", expand = c(0.3,0)) +
        ylab("Evaluation (M +/-SE)") +
        scale_y_continuous(paste(x, "(M +/-SE)"), expand = c(0,0)) +
        guides(fill = "none", linetype = "none") +
        theme(
            legend.position = "right",
            legend.direction = "vertical") +
        labs(title = "Evaluations change with sample size as function of winning model and prior")

    if (x == "Evaluation") {
        p <- p + geom_errorbar(aes(ymin = M-SE, ymax = M+SE, group = interaction(type, prior)), width = 0.1, position = pd, color = "grey") +
        geom_line(aes(color = prior, group = interaction(type, prior)), pos = pd, alpha = 0.5) +
        geom_point(aes(shape = type, color = prior), size = 2, position = pd, fill = "white")
    }
    if (x == "Confidence") {
        p <- p + geom_errorbar(aes(ymin = M-SE, ymax = M+SE, group = type), width = 0.1, position = pd, color = "grey") +
        geom_line(aes(group = type), pos = pd, alpha = 0.5) +
        geom_point(aes(shape = type), size = 2, position = pd, fill = "white")
    }
    return(p)
}

pd <- position_dodge(width = 0.09)

p1 <- plot_it("Evaluation") +ggtitle("Evaluations of gambles by model") +labs(caption = "Evaluations scaled to 0-1 and z-standardized within participants") 
p2 <- plot_it("Confidence") +ggtitle("Confidence ratings by model") +theme(legend.position = "none") +labs(caption = "Confidence z-standardized within participants") 


p1 + p2 +plot_layout(nrow = 2) +plot_annotation(title = "Qualitative predictions given cognitive strategies", tag_levels = "a")