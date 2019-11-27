# ==========================================================================
# Figure: Influence of sample size on stand. valuations by cogn. model
# ==========================================================================
## ---- fig6 ----
# source("4-evaluations-by-prior.R", chdir = TRUE)
d[winner=="bvu", priorx_cat := cut(val, c(0, 1, 2))]
d[winner!="bvu", priorx_cat := NA]
d[, priorx_cat := factor(priorx_cat, exclude = NULL, labels = c("Zero-outcome prior", "Gain prior", "None"))]
# Melt
dpredagg <- melt(d[condition == "experience"], c("id", "gambletype", "samplesizecat", "priorx_cat", "winner"), c("value_scaled", "conf_scaled"))
dpredagg[, variable := factor(variable, levels = c("value_scaled", "conf_scaled"), labels = c("Evaluation", "Confidence"))]
dpredagg <- dpredagg[winner != "base"]

plot_it <- function(x) {
    if (x == "Evaluation") {
        dpredagg <- dpredagg[, .(M = mean(value), SE = sd(value)/sqrt(.N)),
            by = .(gambletype, samplesizecat, priorx_cat, winner, variable)]
        dpredagg[, gambletype := factor(gambletype, levels = c("p-bet", "$-bet"), labels = c("p-bet\n(high probability, low gain)", "$-bet\n(low probability, high gain)"))]
    } else {
        dpredagg <- dpredagg[, .(M = mean(value), SE = sd(value)/sqrt(.N)),
            by = .(samplesizecat, winner, variable)]
    }
    dpredagg[, samplesizecat := factor(samplesizecat, levels = c("xs","s","m","l"))]
    dpredagg[, winner := factor(winner, model_levels, model_labels)]
    
    p <- ggplot(dpredagg[variable == x], aes(samplesizecat, M)) +
        facet_wrap(~ winner, scales = "free_y") +
        scale_shape_manual("Gamble Type", values = c(19,21)) +
        scale_linetype_manual("Winning Model", values = c(1,2,3)) +
        scale_fill_manual("Winning Model", values = model_colors) +
        scale_color_manual("Prior", values = c("darkblue", "lightblue", "brown")) +
        scale_x_discrete("Sample Size Category", expand = c(0.3,0)) +
        ylab("Evaluation (M +/-SE)") +
        scale_y_continuous(paste(x, "(M +/-SE)"), expand = c(0,0)) +
        guides(fill = "none", linetype = "none") +
        theme(
            legend.position = "right",
            legend.direction = "vertical") +
        labs(title = "Evaluations change with sample size as function of winning model and prior")

    if (x == "Evaluation") {
        p <- p + geom_errorbar(aes(ymin = M-SE, ymax = M+SE, group = interaction(gambletype, priorx_cat)), width = 0.1, position = pd, color = "grey") +
        geom_line(aes(color = priorx_cat, group = interaction(gambletype, priorx_cat)), pos = pd, alpha = 0.5) +
        geom_point(aes(shape=gambletype, color=priorx_cat), size = 1.5, position = pd, fill = "white")
    }
    if (x == "Confidence") {
        p <- p + geom_errorbar(aes(ymin = M-SE, ymax = M+SE), width = 0.1, position = pd, color = "grey") +
        geom_line(aes(group=0), pos = pd, alpha = 0.5) +
        geom_point(size = 1.5, position = pd, fill = "white")
    }
    return(p)
}

pd <- position_dodge(width = 0.09)

p1 <- plot_it("Evaluation") +ggtitle("Evaluations of gambles by model") +labs(caption = "Evaluations scaled to 0-1 and z-standardized within participants") 
p2 <- plot_it("Confidence") +ggtitle("Confidence ratings by model") +theme(legend.position = "none") +labs(caption = "Confidence z-standardized within participants") 


p1 + p2 +plot_layout(nrow = 2) +plot_annotation(title = "Qualitative predictions given cognitive strategies", tag_levels = "a")