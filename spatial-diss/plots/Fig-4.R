# Posterior distributions of estimated coefficients -------------------------------------------------

# Extract the estimates for the coefficients from every draw
posterior.4 <- exp(as.matrix(mod.4))[, 2:15]

# Set plot title
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 89% intervals")

# Get column names for coefficients
x <- colnames(posterior.4)

# Set color scheme for plot
colors <- c("#EE9B67", "#EE9B67", "#001959", "#001959", "#1C5460", "#1C5460")
color_scheme_set(colors)

mcmc_areas(posterior.4,
           prob = 0.89,
           area_method = "scaled height") + 
  plot_title +
  scale_x_continuous(limits = c(0, 10)) +
  # Reverse the order of the coefficients and change the labels
  scale_y_discrete(limits = rev(x),
                   labels = rev(c("Prop. poverty ratio under 2.0",
                                  "Prop. non-white Q2",
                                  "Prop. non-white Q3",
                                  "Prop. non-white Q4",
                                  "Prop. non-white Q5",
                                  "Fr",
                                  bquote(PM[2.5] ~ "Q2"),
                                  bquote(PM[2.5] ~ "Q3"),
                                  bquote(PM[2.5] ~ "Q4"),
                                  bquote(PM[2.5] ~ "Q5"),
                                  bquote("Fr x "~ PM[2.5] ~ "Q2"),
                                  bquote("Fr x "~ PM[2.5] ~ "Q3"),
                                  bquote("Fr x "~ PM[2.5] ~ "Q4"),
                                  bquote("Fr x "~ PM[2.5] ~ "Q5"))),
                   name = "Coefficients") +
  # Add a vertical line at x = 1 (no effect) 
  geom_vline(xintercept = 1) +
  # hide the default vertical line at x = 0
  geom_vline(xintercept = 0, color = "white")

# Save plot
ggsave(here("plots", "post-dist.pdf"),
       width = 7.5, height = 5)  


