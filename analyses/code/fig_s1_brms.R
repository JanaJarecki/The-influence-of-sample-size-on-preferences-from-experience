source("fig_setup.R")
library(tidybayes)
library(magrittr)
library(ggplot2)
library(modelr)
# linmod_brms is the object with the fit



post <- posterior_samples(linmod_brms)

# n_distr <- 20
# ggplot(data.frame(value_scaled = c(-4,4)), aes(x = value_scaled)) +
#   mapply(function(mean, sd) {
#     stat_function(fun   = dnorm, 
#                   args  = list(mean = mean, sd = sd), 
#                   alpha = 1/2, 
#                   color = "grey")
#     }, 
#     # Enter means and standard deviations here
#     mean = post[1:n_distr, "b_gambletype1"],
#     sd   = post[1:n_distr, "sigma"]
#     ) +
#   geom_jitter(data = dpred, aes(y = -0.02),
#               height = .003, shape = 1, alpha = 1/3) +
#   scale_y_continuous(NULL, breaks = NULL) +
#   coord_flip()



  plot_fixef <- function(fit, par_levels, title) {
    par_levels <- paste0("b_", rownames(fixef(fit)))[-1]
    par_labels <- sub("samplesizecat_num","sample size",
    sub("gambletype1", "p-bet",
      sub("priorx_cat1", "loss prior",
        sub("priorx_cat2", "gain prior",
          sub("b_", "", 
            gsub(":", " x\n", par_levels))))))
    draws <- gather_draws(fit, !!!syms(par_levels), regex = TRUE)
    pars_me <- par_levels[1:4]
    pars_int1 <- par_levels[5:9]
    pars_int2 <- par_levels[10:11]
    draws$group <- ifelse(draws$.variable %in% pars_me, "Main Effect",
      ifelse(draws$.variable %in% pars_int1, "First-order Interaction",
        ifelse(draws$.variable %in% pars_int2, "Second-order Interaction", NA)))
    draws$group <- factor(draws$group, levels = c("Main Effect", "First-order Interaction", "Second-order Interaction"))
    draws$.variable <- factor(draws$.variable, par_levels, par_labels)

    
      ggplot(draws, aes(
          x = .variable,
          y = .value,
          group = group)) +
        geom_hline(yintercept=0, color = "grey80", linetype = 2, size = 0.4) +
        #stat_slab(fill = "black", alpha = 0.09, size = 0.1, color = "grey60") +
        stat_gradientinterval(shape = 21, point_color = "white", point_fill = "black") +
        facet_wrap(~group, ncol = 1, drop = TRUE, scales = "free_y") +
        theme(
          axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          panel.spacing.y = unit(1, "cm"))
      )
  }


# Do people in the p-bet condition with gain priors increase their evaluations with ss?
# Do people in the p-bet condition with loss priors decrease 
library(emmeans)
#get the adjusted means
trends <- emtrends(fit, ~ priorx_cat | gambletype, var = "samplesizecat_num")

#get all possible contrasts
cont <- contrast(warm_em, "tukey")
cont




  #Intercept Plot
  par <- c("intercept")

  # Main Effects plot
  pars_me <- c("samplesizecat_num", "priorx_cat1", "priorx_cat2", "gambletype1")
  par_levels <- paste0("b_", names(sort(fixef(linmod_brms, pars = pars)[, "Estimate"], decreasing = TRUE)))
  maineff <- plot_fixef(linmod_brms, par_levels, title = "Main Effects")


  # First-Order-Interactions
  pars_int1 <- rownames(fixef(linmod_brms))[6:10]
  par_levels <- paste0("b_", names(sort(fixef(linmod_brms, pars = pars)[, "Estimate"], decreasing = TRUE)))
  inteff1 <- plot_fixef(linmod_brms, par_levels, "First-order Interactions")


  # Second-Order-Interactions
  pars_int2 <- rownames(fixef(linmod_brms))[11:12]
  par_levels <- paste0("b_", names(sort(fixef(linmod_brms, pars = pars)[, "Estimate"], decreasing = TRUE)))
  inteff2 <- plot_fixef(linmod_brms, par_levels, "Second-order Interactions")





  maineff  /
    inteff1  /
      inteff2  +plot_annotation(tag_levels = "a")



  +
  scale_color_viridis_c()


  geom_halfeyeh(aes(slab_fill = .value), slab_alpha = 0.2, slab_size = 0.3, shape = 21, point_color = "white", interval_alpha=0.4, stroke = 1) +
  scale_fill_viridis_c()


dpred %>%
  data_grid(samplesizecat_num, priorx_cat, gambletype, id) %>%
  add_fitted_draws(linmod_brms) %>%
  head(10)


ddraws <- dpred %>%
  data_grid(samplesizecat_num, priorx_cat, gambletype, id) %>%
  add_fitted_draws(linmod_brms, dpar = c("mu", "sigma"))

ggplot(ddraws, aes(x = samplesizecat_num)) +
  stat_dist_slabinterval(aes(dist = "norm", arg1 = mu, arg2 = sigma), 
     slab_color = "gray65", alpha = 1/10, fill = NA, data = . %>% sample_draws(30), scale = .5
   ) +
  stat_dist_halfeye(aes(y = .value), side = "bottom", scale = .5) +
  geom_point(aes(y = value_scaled), data = dpred, shape = 21, fill = "#9ECAE1", size = 2, position = position_nudge(x = -.1))






mcmc_intervals(post, regex_par = pars) +
  mcmc_areas(
  post,
  regex_pars = pars,
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)



 +
  mcmc_areas()

  scale_fill_grey()



library(bayesplot)
pars <- c("samplesizecat", "priorx", "gambletype")
mcmc_intervals(post, regex_par="^b", prob = 0.8)
library(ggridges)
ap <- mcmc_areas_ridges(post, regex_pars = "gambletype")

ingredients <- ggplot_build(ap) %>% purrr::pluck("data", 1)

 +
  coord_flip() +
  geom_density_ridges_gradient()

+stat_density_ridges(quantile_lines = TRUE, quantiles = 2)

stanplot(linmod_brms, regex_pars = pars, type = "hist") +
  theme(axis.text.y = element_text(hjust = 0))

linmod_brms