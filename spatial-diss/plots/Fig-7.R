# Posterior predictive checks --------------------------------------------------------

# Actual observations
y <- full.data$n

# Distribution of the outcome from the specified model
yrep.4 <- posterior_predict(mod.4)

# Subset a sample of 100 draws from the model
samp100 <- sample(nrow(yrep.4), 100)

# Overly posterior predictions with observations to see how the distributions align
ppdens.4 <- ppc_dens_overlay(y, yrep.4[samp100, ]) +
  xlim(0, 600) +
  labs(x = "Asthma count", y = "Density")

# Add annotation to plot with model specification
ppdens.4 + plot_annotation(title = expression("Poverty + Race + Fr + PM"[2.5]*" Q + Fr x PM"[2.5]*" Q"))

# Save plot
ggsave(here("plots", "pp-checks.pdf"), 
       width = 7.5, height = 4)


