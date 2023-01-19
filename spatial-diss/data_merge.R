library(tidyverse)
library(here)
library(lubridate)
library(sf)
library(brms)

# Load data ---------------------------------------------------------------

# Load data---daily obs per census tract
# Filter out 2001 because there isn't air pollution data
tracts <- st_read(here("data", "tracts.shp")) %>% 
  filter(year(date) != 2001)

# Load data---poverty ratio from ACS-5 2010
pov <- read_rds(here("DissertationResearch", "Ch1", "poverty-ratio.rds")) %>% 
  dplyr::select(geoid, pct.under.2.00)

# Load MODIS Fr
Fr <- here("data", "MODIS_Fr_med_daily_tracts.csv") %>% 
  rename(geoid = GEOID) %>% 
  mutate(Date = ymd(str_sub(Date, 2, 11)),
         geoid = as.character(geoid)) %>% 
  # Filter to 2002-2012
  filter(year(Date) >= 2002 & year(Date) <= 2012) %>% 
  # Group by census tract and obtain the value corresponding to the 
  # 99th percentile so that there is one Fr value per tract for the
  # entire study period
  group_by(geoid) %>% 
  summarise(Fr = quantile(Fr.old, 
                          0.99, 
                          na.rm = TRUE,
                          names = FALSE))

# Load the PM data from CACES https://www.caces.us/data
pm2.5 <- read_csv(here("data", "caces_pollution.csv")) %>% 
  # Group by census tract and calculate the mean and median PM2.5 values
  group_by(geoid = as.character(fips),
           pollutant) %>% 
  summarise(mean = mean(pred_wght, na.rm = TRUE),
            med = median(pred_wght, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(pollutant == "pm25")

# Summarize variables for each census tract over the study period 
# (two tracts are dropped for missing poverty data)
tracts <- tracts %>% 
  group_by(geoid) %>% 
  summarize(n = sum(n), # total cases
            lst = mean(lst, na.rm = TRUE), # mean LST
            pct_nonwht = first(pct_nonwht), # this value is constant
            child.pop = first(child.pop)) %>% # this value is constant
  left_join(pov) %>% 
  left_join(Fr) %>% 
  left_join(pm2.5) %>% 
  drop_na() %>% 
  # Categorize each independent variable into quintiles
  mutate(pov.q = ntile(pct.under.2.00, 5),
         pov.q = as_factor(pov.q),
         pm.q = as_factor(ntile(pm25.mean, 5)),
         race.q = ntile(pct_nonwht, 5),
         race.q = as_factor(race.q),
         Fr.q = ntile(Fr, 5),
         Fr.q = as_factor(Fr.q),
         lst.q = ntile(lst, 5),
         lst.q = as_factor(lst.q))

# Save full dataset
write_sf(tracts, here("data", "full-data.shp"))



