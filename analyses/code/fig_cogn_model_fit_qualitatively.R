# ==========================================================================
# Figure: Quantitative fit of cogn. models
#         BIC weights as bar plots
# ==========================================================================

## @knitr cogn_model_fit_qualitatively


dpred[, group := factor(paste0(winner, ", ", sprintf("%02.0f", as.numeric(id))))]
dummy_range <- dpred[, .(value = range(c(value,pred)), pred = range(c(value,pred))), by = group]
ggplot(dpred, aes(x = value, y = pred)) +
  geom_abline(linetype = 2, size = 0.4) +
  geom_point(aes(fill = winner), shape=21, color = "black", alpha = 0.4, size = 1) +
  geom_blank(data = dummy_range) +
  facet_wrap(~group, scales = "free", nrow = 5, drop=TRUE, shrink=TRUE) +
  themejj(facet=TRUE, base_family = "Arial") +
  scale_x_continuous("Predicted Evaluations", breaks = pretty_breaks()) +
  scale_y_continuous("Observed Evaluations", breaks = pretty_breaks()) +
  scale_fill_manual("Winning Model", values = model_colors) +
  theme(
    axis.ticks = element_blank(),
    aspect.ratio = 1,
    strip.text = element_text(lineheight = unit(0.5, "lines")))