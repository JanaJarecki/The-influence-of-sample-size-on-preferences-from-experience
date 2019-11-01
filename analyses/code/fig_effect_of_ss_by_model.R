# ==========================================================================
# Figure: Influence of sample size on stand. valuations by cogn. model
# ==========================================================================

## @knitr effect_of_ss_by_model

dpred[winner=="BVU", priorx_cat := cut(val, c(0, 1, 2))]
  dpred[, priorx_cat := factor(priorx_cat, exclude = NULL, labels = c(" - loss prior (0,1]", " - gain prior (1,2]", ""))]
  dpred[, value_scaled := value / gamblex]
  dpredagg <- dpred[winner!="BASE", .(M = mean(value_scaled), SE = sd(value_scaled)/sqrt(.N)), by = .(gambletype, samplesizecat, priorx_cat, winner)]
  dpredagg[, samplesizecat := factor(samplesizecat, levels = c("xs","s","m","l"))]
  d_n <- dpred[winner != "BASE", .(N = length(unique(id)), M = mean(value_scaled)), by = .(winner, priorx_cat, gambletype, samplesizecat)]
  d_n <- d_n[(gambletype == "$-bet" & samplesizecat == "l" )| (gambletype == "p-bet" & samplesizecat == "l")]
  d_n[gambletype == "p-bet" & grepl("gain", priorx_cat), M := 0.55]

  pd <- position_dodge(width = 0.1)
  ggplot(dpredagg, aes(samplesizecat, M, fill=winner)) +
    geom_errorbar(aes(ymin = M-SE, ymax = M+SE, group = interaction(winner, priorx_cat)), width = 0.1, pos = pd, color = "grey") +
    geom_line(aes(linetype = interaction(winner, priorx_cat, sep=""), group = interaction(winner, priorx_cat)), pos = pd) +
    geom_point(aes(shape = interaction(winner, priorx_cat, sep="")), size = 2, pos = pd, fill = "white", color = "white") +
    geom_point(aes(shape = interaction(winner, priorx_cat, sep="")), size = 1, pos = pd) +
    geom_text(data = d_n, aes(label = paste("n =", N)), size = 2.7, nudge_y = 0.02, nudge_x = 0.5, fontface = 3) +
    facet_wrap(~gambletype) +
    scale_shape_manual("Winning Model", values = c(25,24,21)) +
    scale_linetype_manual("Winning Model", values = c(1,1,3)) +
    scale_fill_manual("Winning Model", values = model_colors) +
    scale_x_discrete("Sample Size Category", expand = c(0.3,0)) +
    ylab("Evaluation, Scaled to 0 - 1 (M +/-SE)") +
    scale_y_continuous("Evaluation, Scaled to 0 - 1 (M +/-SE)", limits = c(0,1), expand = c(0,0)) +
    guides(shape = guide_legend(override.aes = list(fill = model_colors[c("BVU","BVU","RF")])), fill = "none") +
    theme(aspect.ratio = 1.6)