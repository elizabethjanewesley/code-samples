# FIPS codes for the counties of interest
county.list <- c("20209", "20091", 
                 "29095", "29047")

# Load counties and filter to the counties of interest
counties <- st_read(here("data", "tl_2021_us_county.shp")) %>% 
  filter(GEOID %in% county.list) 

# Load census tracts & transform the projection to match the counties
x <- st_read(here("data", "tracts.shp")) %>% 
  st_transform(st_crs(counties))

# Map and label the counties overlain with the census tracts
ggplot() +
  geom_sf(data = counties, aes(fill = NAME), linewidth = 1, color = "black") +
  scale_fill_scico_d(palette = "batlow") +
  geom_sf(data = x, aes(fill = NA), lwd = 1.2, color = "white") +
  geom_sf_label(data = counties, aes(label = NAME)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("")

# Save map
ggsave(here("plots", "county_map.pdf"), 
       width = 7.5, height = 7.5)




