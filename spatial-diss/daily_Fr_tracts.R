# This script uses Google Earth Engine (GEE) via the rgee package to calculate
# the fractional vegetation (Fr) for each census tract. Fr is calculated per tract as
# (NDVI - bare soil NDVI value) / (full vegetation NDVI value - bare soil NDVI value)^2. 
# NDVI is calculated per pixel using the near infrared (NIR) and red bands as 
# (NIR - Red) / (NIR + Red). Census tracts are simple features (spatial vector data).

library(tidyverse)
library(sf)
library(rgee)
library(here)

# Load census tracts
tracts <- read_sf(here("data", "tracts_geo.shp"))

# Define the data product and bands to be used in GEE
# Terra Surface Reflectance Daily Global 250m
data <- "MODIS/006/MOD09GQ"

red.band <- "sur_refl_b01" # 620-670nm, 250m res
nir.band <- "sur_refl_b02" # 841-876nm, 250m res

# Define the start date and end date for each year
start.date <- "x-01-01"
end.date <- "x-12-31"

# Create an empty tibble to append data to in the for loop
NDVI <- tibble()

# For each year from 2000 to 2021 calculate the daily median NDVI for each census
# tract and append to the tibble. 
for (year in 2000:2021) {
  
  # Date is a string in GEE. For each year create a start date and end date.
  year <- as.character(year)
  start <- str_replace(start.date, "x", year)
  end <- str_replace(end.date, "x", year)
  
  # Get all images for the year
  ndvi.day <- ee$ImageCollection(data) %>% 
    ee$ImageCollection$filterDate(start, end) %>% 
    # Calculate NDVI
    ee$ImageCollection$map(function(x) 
      x$normalizedDifference(list(nir.band, red.band))$rename("NDVI")) %>% 
    # Make each date a band
    ee$ImageCollection$toBands()  
  
  # extract median NDVI to each census tract
  NDVI.year <- ee_extract(x = ndvi.day, 
                         y = tracts["GEOID"], 
                         fun = ee$Reducer$median(),
                         sf = FALSE) %>% 
    pivot_longer(-GEOID, names_to = "Date", values_to = "NDVI")
  
  # append the daily values for the year to the tibble
  NDVI <- bind_rows(NDVI, NDVI.year)
  
  print(paste0(year, " is done!"))
  
}

# Formate date column
Fr <- Fr %>% 
  mutate(Date = str_remove(Date, "X"),
         Date = str_remove(Date, "_NDVI"),
         Date = ymd(Date))

# NDVI values for bare soil and fully veg chosen from a manual inspection of images
ndvi.veg <- 0.87
ndvi.soil <- 0.3

# Calculate Fr and round to 2 decimals
Fr <- Fr %>% 
  mutate(Fr = ((NDVI - ndvi.soil) / (ndvi.veg - ndvi.soil))^2,
         Fr = round(Fr, 2))

# Write to csv
write_csv(Fr, here("data", "MODIS_Fr_med_daily_tracts.csv"))
