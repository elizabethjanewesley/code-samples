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

