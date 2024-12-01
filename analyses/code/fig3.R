# ==========================================================================
# Figure: Qualitative fit of cogn. models
#         BIC weights as bar plots
# ==========================================================================
# ---- fig_qualitative ----
pred[, group := factor(paste0(factor(winner, levels = model_levels, labels = model_labels), "~", id))]
dummy_range <- pred[, .(obs = range(c(obs,pred)), pred = range(c(obs,pred))), by = group]
fig <- ggplot(pred, aes(x = obs, y = pred)) +
  geom_abline(linetype = 2, size = 0.4) +
  geom_point(aes(fill = factor(winner, levels = model_levels, labels = model_labels)), shape=21, color = "black", alpha = 0.6, size = 1.5) +
  geom_blank(data = dummy_range) +
  facet_wrap(~group, scales = "free", nrow = 5, drop=F, shrink=FALSE, labeller=label_parsed) +
  themejj(facet=TRUE) +
  scale_x_continuous("Predicted Evaluations", breaks = pretty_breaks()) +
  scale_y_continuous("Observed Evaluations", breaks = pretty_breaks()) +
  scale_fill_manual("Winning Model", values = model_colors, labels=parse_format()) +
  theme(
    axis.ticks = element_blank(),
    aspect.ratio = 1,
    strip.text = element_text(lineheight = unit(0.5, "lines")))
