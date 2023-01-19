# This script uses Google Earth Engine (GEE) via the rgee package to calculate
# the median land surface temperature for each census tract. Census tracts
# are simple features (spatial vector data).

library(tidyverse)
library(sf)
library(rgee)
library(here)

# Load census tracts
tracts <- read_sf(here("data", "tracts_geo.shp"))

# Define the data product and band to be used in GEE
# MOD11A1.006 Terra Land Surface Temperature and Emissivity Daily Global 1km
data <- "MODIS/006/MOD11A1" 
band <- "LST_Day_1km"

# Define the start date and end date for each year
start.date <- "x-01-01"
end.date <- "x-12-31"

# Create an empty tibble to append data to in the for loop
LST <- tibble()

# For each year from 2000 to 2021 calculate the daily median LST for each census
# tract and append to the tibble. 
for (year in 2000:2021) {
  
  # Date is a string in GEE. For each year create a start date and end date.
  year <- as.character(year)
  start <- str_replace(start.date, "x", year)
  end <- str_replace(end.date, "x", year)
  
  # get all LST images for the year
  lst.day <- ee$ImageCollection(data) %>% 
    ee$ImageCollection$filterDate(start, end) %>% 
    # Select only the desired band
    ee$ImageCollection$map(function(x) x$select(band)) %>% 
    # Make each date a band
    ee$ImageCollection$toBands()  
  
  # extract median LST to each census tract
  LST.year <- ee_extract(x = lst.day, 
                    y = tracts["GEOID"], 
                    fun = ee$Reducer$median(),
                    sf = FALSE) %>% 
    pivot_longer(-GEOID, names_to = "Date", values_to = "LST_Day")
  
  # append the daily values for the year to the tibble
  LST <- bind_rows(LST, LST.year)
  
  print(paste0(year, " is done!"))
  
}

# Formate date column
LST <- LST %>% 
  mutate(Date = str_sub(Date, start = 2, end = 11),
         Date = str_replace_all(Date, pattern = "_", replacement = "-"),
         Date = date(Date))

# Convert LST to Celsius
LST <- LST %>% 
  mutate(LST_Day = LST_Day * 0.02 - 273.15)

# Write to csv
write_csv(LST, here("data", "MODIS_LST_med_daily_tracts.csv"))

