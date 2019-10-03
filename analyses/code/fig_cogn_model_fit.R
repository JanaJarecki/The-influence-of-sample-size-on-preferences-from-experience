## @knitr cogn_model_fit

# Order of participant ids (x-axis) for the plot
  id_order <- order_evidence(unique(weights$id), weights[, 2:4])
  wn <- names(winners)
  wn <- unique(c(wn, "base"))
  # Make the plots
  p1 <- ggplot(weights, aes(x=1, fill=winner)) +
    geom_bar(position = "stack") +
    scale_fill_manual("Model",
      values = model_colors,
      labels = unname(model_labels[wn]),
      drop = FALSE,
      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(expand = c(0,0.15)) +
    xlim(0.5,1.5) +
    xlab("Count")+
    geom_text(stat = "count", aes(label = (..count..), color = winner), position = position_stack(vjust = 0.5), family = "Arial", size = 3.4) +
    scale_color_manual(values = c("black", "black", "white")) +
    coord_flip() +
    guides(color = "none") +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_blank(),
      panel.spacing.x = unit(2, "lines"))
  p2 <- ggplot(melt(weights, id = 1, measure = 2:4), aes(x = factor(id, levels = id_order),  y = value, fill = factor(variable, model_levels, model_labels))) +
    geom_bar(stat = "identity", color = "white", size = 0.01) +
    geom_hline(yintercept = 0.5, linetype = 2, color = "grey80", alpha = 0.8, size = 0.4) +
    scale_fill_manual("Model", values = model_colors) +
    scale_y_continuous("Evidence Strength", expand  = c(0,0), labels = percent, breaks = c(0,0.5,1)) +
    xlab(paste("Participant, N =", N)) +
    guides(fill = guide_legend(override.aes = list(linetype = 0, size = 4.5))) +
    theme(axis.ticks = element_blank(), 
      axis.text.x=element_blank(),
      legend.position = "none",
      axis.line = element_blank(),
      axis.text.y = element_text(vjust = c(0,.5,.5,.5,1)),
      axis.title.x = element_text(margin = margin(t = 0.5, unit = "lines")),
      legend.key.height = unit(1.2, "lines"),
      legend.key = element_rect(fill = "white")) 
  # Combine the plots
  p1 + p2 +plot_layout(nrow=2, heights=c(.2,.75))