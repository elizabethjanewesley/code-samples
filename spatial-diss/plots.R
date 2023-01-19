# This script produces data visualizations, including descriptive plots,
# mapping of spatial distributions, and plots for evaluating and interpreting
# model output.

library(bayesplot)
library(scico)
library(tidybayes)
library(patchwork)

# Load full dataset
full.data <- st_read(here("data", "full-data.shp"))

# Load models
mod.1 <- read_rds(here("models", "mod-1.rds"))
mod.2 <- read_rds(here("models", "mod-2.rds"))
mod.3 <- read_rds(here("models", "mod-3.rds"))
mod.4 <- read_rds(here("models", "mod-4.rds"))
mod.5 <- read_rds(here("models", "mod-5.rds"))


# Spatial distribution of variables ----------------------------------

# Relative risk per census tract compared to overall KC rate during the study period
overall.rate <- sum(full.data$n) / sum(full.data$child.pop)

full.data <- full.data %>% 
  # Relative risk is census tract rate compared to overall rate
  mutate(RR = (n / child.pop) / overall.rate)

# Map of RR
map.rate <- ggplot(full.data) +
  geom_sf(aes(fill = RR), lwd = 0) +
  scale_fill_scico(name = "RR", 
                   palette = "batlow",
                   breaks = c(1, 3, 5)) +
  coord_sf(crs = st_crs(full.data), datum = NA) +
  theme_bw() 

# Map of LST
map.lst <- ggplot(full.data) +
  geom_sf(aes(fill = lst), lwd = 0) +
  scale_fill_scico(name = "Mean LST", palette = "batlow") +
  coord_sf(crs = st_crs(full.data), datum = NA) +
  theme_bw() 

# Map of race
map.race <- ggplot(full.data) +
  geom_sf(aes(fill = race.q), lwd = 0) +
  scale_fill_scico_d(name = "Prop.\nnon-white\nresidents\nquintiles", 
                     palette = "batlow",
                     breaks = c(5, 4, 3, 2, 1)) +
  coord_sf(crs = st_crs(full.data), datum = NA) +
  theme_bw() 

# Map of income
map.pov <- ggplot(full.data) +
  geom_sf(aes(fill = pct.under.2.00), lwd = 0) +
  scale_fill_scico(name = "Prop.\npoverty ratio\nunder 2.0", palette = "batlow") +
  coord_sf(crs = st_crs(full.data), datum = NA) +
  theme_bw() 

# Map of Fr
map.fr <- ggplot(full.data) +
  geom_sf(aes(fill = Fr), lwd = 0) +
  scale_fill_scico(name = "Mean Fr", palette = "batlow", direction = -1) +
  coord_sf(crs = st_crs(full.data), datum = NA) +
  theme_bw() 

# Map of PM2.5
map.pm.q <- ggplot(full.data) +
  geom_sf(aes(fill = pm.q), lwd = 0) +
  scale_fill_scico_d(name = expression(atop("Mean PM"[2.5], "quintiles")), 
                     palette = "batlow",
                     breaks = c(5, 4, 3, 2, 1)) +
  coord_sf(crs = st_crs(full.data), datum = NA) +
  theme_bw() 

# Put maps together in a patchwork with subplot labels
patch <- (map.rate / map.race / map.lst) | (map.pov / map.fr / map.pm.q) 

patch + plot_annotation(tag_levels = 'a')

# Save maps
ggsave(here("plots", "descript-spatial.pdf"), 
       width = 7.5, height = 7.5)



# Descriptive plots of relationships --------------------------------------

# These plots show how asthma counts vary with bivariate relationships 
# Each plot shows the mean asthma rate per bin, with each bin defined
# by quintiles of two variables. For instance, how many asthma cases were there
# in census tracts that were in both the lowest quintile for income and the lowest
# quintile for Fr. 

# Individual plots

# Income & Fr
pf <- full.data %>% 
  mutate(rate = n / child.pop) %>% 
  # Group by quintiles and calculate the mean rate per bin 
  group_by(pov.q, Fr.q) %>% 
  summarize(rate = mean(rate)) %>% 
  ggplot() +
  geom_tile(aes(x = pov.q, y = Fr.q, fill = rate)) 

# Income & race
pr <- full.data %>% 
  mutate(rate = n / child.pop) %>% 
  group_by(pov.q, race) %>% 
  summarize(rate = mean(rate)) %>% 
  ggplot() +
  geom_tile(aes(x = pov.q, y = race.q, fill = rate)) 

# Income & PM2.5
pp <- full.data %>% 
  mutate(rate = n / child.pop) %>% 
  group_by(pov.q, pm.q) %>% 
  summarize(rate = mean(rate)) %>% 
  ggplot() +
  geom_tile(aes(x = pov.q, y = pm.q, fill = rate)) 

# Fr & PM2.5
fp <- full.data %>% 
  mutate(rate = n / child.pop) %>% 
  group_by(Fr.q, pm.q) %>% 
  summarize(rate = mean(rate)) %>% 
  ggplot() +
  geom_tile(aes(x = Fr.q, y = pm.q, fill = rate)) 

# Fr & race
fr <- full.data %>% 
  mutate(rate = n / child.pop) %>% 
  group_by(Fr.q, race.q) %>% 
  summarize(rate = mean(rate)) %>% 
  ggplot() +
  geom_tile(aes(x = Fr.q, y = race.q, fill = rate)) 

# PM2.5 & race
pmr <- full.data %>% 
  mutate(rate = n / child.pop) %>% 
  group_by(pm.q, race.q) %>% 
  summarize(rate = mean(rate)) %>% 
  ggplot() +
  geom_tile(aes(x = race.q, y = pm.q, fill = rate)) 

# Put plots together in a patchwork with subplot labels
p <- pf + plot_spacer() + plot_spacer() +
  pr + fr + plot_spacer() + 
  pp + fp + pmr + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a')

# remove axis stuff from some subplots and change axis labels
p[[1]] <- p[[1]] + 
  ylab("Fr") +
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

p[[4]] <- p[[4]] + 
  ylab("Prop. non-white") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

p[[5]] <- p[[5]] + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

p[[7]] <- p[[7]] +
  ylab(expression("PM"[2.5])) +
  xlab("Poverty ratio") +
  theme_bw() +
  theme(panel.grid = element_blank())

p[[8]] <- p[[8]] + 
  xlab("Fr") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

p[[9]] <- p[[9]] + 
  xlab("Prop. non-white") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# Set to equal grid coordinates and set the colorbar 
p & coord_equal() & 
  scale_fill_scico(palette = "batlow",
                   limits = c(0, 0.45),
                   name = "Asthma\nrate") 

# Save plot
ggsave(here("plots", "descript.pdf"), width = 7.5, height = 7.5)


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
fr.pm.pov <- plot(conditional_effects(mod.7c, 
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
pm.pov <- plot(conditional_effects(mod.7c, 
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
fr.pm.race <- plot(conditional_effects(mod.7c, 
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
pm.race <- plot(conditional_effects(mod.7c, 
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



# Posterior distributions of estimated coefficients -------------------------------------------------

# Extract the estimates for the coefficients from every draw
posterior.5 <- exp(as.matrix(mod.5))[, 2:15]

# Set plot title
plot_title <- ggtitle("Posterior distributions",
                     "with medians and 89% intervals")

# Get column names for coefficients
x <- colnames(posterior.5)

# Set color scheme for plot
colors <- c("#EE9B67", "#EE9B67", "#001959", "#001959", "#1C5460", "#1C5460")
color_scheme_set(colors)

mcmc_areas(posterior.5,
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




# Posterior predictive checks --------------------------------------------------------

# Actual observations
y <- full.data$n

# Distribution of the outcome from the specified model
yrep.5 <- posterior_predict(mod.5)

# Subset a sample of 100 draws from the model
samp100 <- sample(nrow(yrep.5), 100)

# Overly posterior predictions with observations to see how the distributions align
ppdens.5 <- ppc_dens_overlay(y, yrep.5[samp100, ]) +
  xlim(0, 600) +
  labs(x = "Asthma count", y = "Density")

# Add annotation to plot with model specification
ppdens.5 + plot_annotation(title = expression("Poverty + Race + Fr + PM"[2.5]*" Q + Fr x PM"[2.5]*" Q"))

# Save plot
ggsave(here("plots", "pp-checks.pdf"), 
       width = 7.5, height = 4)




# Spatial distribution of model predictions ------------------------------

# Map the difference between the predicted counts and the observed counts

# Summarize the predicted counts per tract
mod.5.pred <- full.data %>% 
  # add predictions to dataset
  add_predicted_draws(mod.5) %>% 
  # Group by census tract and calculate summary stats for predicted cases
  group_by(geoid) %>% 
  summarise(n = first(n),
            mean.pred = mean(.prediction),
            med.pred = median(.prediction),
            min.pred = min(.prediction),
            max.pred = max(.prediction))

# Join summaries to dataset
tracts.pred <- tracts %>% 
  right_join(mod.7c.pred)

# Percent difference between observed and predicted
tracts.pred <- tracts.pred %>% 
  mutate(pct.diff = (mean.pred - n) / mean.pred)

# Map of percent difference
map.diff <- ggplot(tracts.pred) +
  geom_sf(aes(fill = pct.diff), lwd = 0) +
  scale_fill_scico(name = "Percent\ndifference", palette = "vik", limit = r) +
  coord_sf(crs = st_crs(tracts.pred), datum = NA) +
  labs(x = "Percent difference between mean predicted \nand observed cases") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) +
  theme_bw()

# Scatterplot of obs vs pred 

scat.plot <- ggplot(mod.7c.pred, aes(x = n, y = mean.pred)) +
  geom_point(color = "#001959") +
  geom_abline(intercept = 0, slope = 1, color = "#808133") +
  labs(x = "Observed cases",
       y = "Mean predicated cases") +
  coord_fixed(ratio = 1, ylim = c(0, 1000), xlim = c(0, 1000)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) +
  theme_bw()

# Put map and scatterplot together in a patchwork with subplot labels
map.diff + scat.plot  + plot_annotation(tag_levels = 'a')

# Save plot
ggsave(here("plots", "pp-maps.pdf"),
       width = (7.5 * 1.5), height = (3 * 1.5))



