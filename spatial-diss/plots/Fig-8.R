# Spatial distribution of model predictions ------------------------------

# Map the difference between the predicted counts and the observed counts

# Summarize the predicted counts per tract
mod.4.pred <- full.data %>% 
  # add predictions to dataset
  add_predicted_draws(mod.4) %>% 
  # Group by census tract and calculate summary stats for predicted cases
  group_by(geoid) %>% 
  summarise(n = first(n),
            mean.pred = mean(.prediction),
            med.pred = median(.prediction),
            min.pred = min(.prediction),
            max.pred = max(.prediction))

# Join summaries to dataset
tracts.pred <- tracts %>% 
  right_join(mod.4.pred)

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

scat.plot <- ggplot(mod.4.pred, aes(x = n, y = mean.pred)) +
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



