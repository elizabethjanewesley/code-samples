# Conditional effects -----------------------------------------------------

# These plots show the effects of the interaction between Fr and PM2.5 
# evaluated at different conditions of income and race

# Income conditions
conditions.pov <- data.frame(pct.under.2.00 = c("Prop. pov = 0.25" = 0.25, 
                                                "Prop. pov = 0.50" = 0.5, 
                                                "Prop. pov = 0.75" = 0.75))

# Race conditions
conditions.race <- data.frame(race.q = c("Prop. non-white Q1" = 1, 
                                         "Prop. non-white Q2" = 2,
                                         "Prop. non-white Q3" = 3,
                                         "Prop. non-white Q4" = 4,
                                         "Prop. non-white Q5" = 5))
# Labels for 
race.labels <- as_labeller(c('1' = "Prop. non-white Q1", 
                             '2' = "Prop. non-white Q2",
                             '3' = "Prop. non-white Q3",
                             '4' = "Prop. non-white Q4",
                             '5' = "Prop. non-white Q5"))

# Plot the effects of the interaction between Fr and PM2.5
# conditional on census tract economic status
fr.pm.pov <- plot(conditional_effects(mod.4, 
                                      conditions = conditions.pov, 
                                      effects = "Fr:pm.q",
                                      prob = 0.89),
                  plot = F)[[1]] +
  scale_color_scico_d(palette = "batlow", 
                      name = expression(atop("Mean", "PM"[2.5])),
                      labels = paste0("Q", 1:5)) +
  scale_fill_scico_d(palette = "batlow", 
                     name = expression(atop("Mean", "PM"[2.5])),
                     labels = paste0("Q", 1:5)) +
  labs(y = "Asthma count") +
  theme_bw()

# Plot the effects of PM2.5
# conditional on census tract economic status
pm.pov <- plot(conditional_effects(mod.4, 
                                   conditions = conditions.pov, 
                                   effects = "pm.q",
                                   prob = 0.89),
               plot = F)[[1]] +
  geom_point(aes(color = pm.q), size = 3) +
  geom_errorbar(aes(color = pm.q)) +
  scale_color_scico_d(palette = "batlow") +
  scale_fill_scico_d(palette = "batlow") +
  labs(y = "Asthma count",
       x = expression("PM"[2.5]*" quintiles")) +
  theme_bw() +
  theme(legend.position = "none") 

# Plot the effects of the interaction between Fr and PM2.5
# conditional on census tract racial composition
fr.pm.race <- plot(conditional_effects(mod.4, 
                                       conditions = conditions.race, 
                                       effects = "Fr:pm.q",
                                       prob = 0.89),
                   plot = F)[[1]] +
  facet_wrap(~ race.q, nrow = 1, labeller = race.labels) +
  scale_color_scico_d(palette = "batlow", 
                      name = expression(atop("Mean", "PM"[2.5])),
                      labels = paste0("Q", 1:5)) +
  scale_fill_scico_d(palette = "batlow", 
                     name = expression(atop("Mean", "PM"[2.5])),
                     labels = paste0("Q", 1:5)) +
  labs(y = "Asthma count") +
  theme_bw()

# Plot the effects of PM2.5
# conditional on census tract racial composition
pm.race <- plot(conditional_effects(mod.4, 
                                    conditions = conditions.race, 
                                    effects = "pm.q",
                                    prob = 0.89),
                plot = F)[[1]] +
  geom_point(aes(color = pm.q), size = 3) +
  geom_errorbar(aes(color = pm.q)) +
  facet_wrap(~ race.q, nrow = 1, labeller = race.labels) +
  scale_color_scico_d(palette = "batlow") +
  scale_fill_scico_d(palette = "batlow") +
  labs(y = "Asthma count",
       x = expression("PM"[2.5]*" quintiles")) +
  theme_bw() +
  theme(legend.position = "none")

# Put income plots together in a patchwork with subplot labels
pm.pov / fr.pm.pov + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a")

# Save income plot
ggsave(here("plots", "cond-effects-pov.pdf"),
       width = 7.5, height = 7.5)

# Put race plots together in a patchwork with subplot labels
pm.race / fr.pm.race + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a")

# Save race plot
ggsave(here("plots", "cond-effects-race.pdf"),
       width = 8, height = 7.5)
