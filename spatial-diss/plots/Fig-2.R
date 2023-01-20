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


