# This script uses tidycensus to download the tract geometries and race data
# from the Census API. Asthma data is joined to the spatial points corresponding
# to each incident and then daily counts of acute incidents are calculated
# per tract.

# Libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(sf)
library(lubridate)
library(rgdal)
library(tidycensus)


census_api_key("d4e48f77bdb0f4af35200b84cbc46bd1c4f6d5a9")

# Load and process asthma data ---------------------------------------------------

# Load data
asthma <- read_csv(here::here("data",
                           "data_raw",
                           "01_ASTHMA_ENCOUNTERS_2000_2012_CENTERLINE_GEOCODED_ALL_ATTRIBUTES.csv"))

asthma <- asthma %>%
  select(MRN = MRN,
         account = ACCOUNT_NUMBER,
         date = ADMIT_DATE,
         pay = `Medical Coverage`,
         gender = SEX_01,
         ethnicity = `Race and Ethnicity`,
         severity = SEVERITY_01,
         age = AGE_01,
         geoid = GEOID10)

# Remove duplicate rows
asthma <- asthma %>%
  distinct()

# Change data types
asthma <- asthma %>%
  mutate(MRN = as.integer(MRN),
         account = as.integer(account),
         pay = as_factor(pay),
         gender = as_factor(gender),
         ethnicity = as_factor(ethnicity),
         severity = as_factor(severity),
         geoid = as.character(geoid))

# Spatial points ----------------------------------------------------------


# Load spatial points & join pts to asthma date by MRN (unique patient ID),
# account number (incident ID), and admission date

pts <- st_read(here::here("data",
                          "data_raw",
                          "V01_asthma_centerline.shp"),
               stringsAsFactors = FALSE) %>% 
  mutate(MRN = as.integer(MRN),
         account = as.integer(Account_Nu)) %>% 
  rename(date = Admit_Date)

# Join spatial points to asthma data
asthma <- pts %>%
  full_join(asthma,
            by = c("MRN", "account", "date")) %>%
  select(-c(1:6, 10:17)) %>%
  drop_na()

rm(pts)

# Drop the years 1999 & 2000 because the data from those years are incomplete
asthma <- asthma %>%
  filter(year(date) > 2000)


# Use tidycensus to obtain race data --------------------------------------

# get geometries for Kansas and Missouri and combine
# P003001 is the total pop, P003002 is the total white alone
# Calculate the percent White and percent non-White per tract

tracts <- rbind(get_decennial(state = "MO",
                          geography = "tract",
                          variables = c("P003001", "P003002"),
                          year = 2010,
                          geometry = TRUE,
                          sumfile = "sf1",
                          output = "wide",
                          keep_geo_vars = T),
                get_decennial(state = "KS",
                              geography = "tract",
                              variables = c("P003001", "P003002"),
                              year = 2010,
                              geometry = TRUE,
                              sumfile = "sf1",
                              output = "wide",
                              keep_geo_vars = T)) %>%
  mutate(pct.wht = P003002 / P003001,
         pct.nonwht = 1 - pct.wht)

# subset to tracts in local counties
counties <- list("Platte" = "165",
                 "Clay" = "047",
                 "Jackson" = "095",
                 "Cass" = "037",
                 "Wyandotte" = "209",
                 "Johnson" = "091",
                 "Miami" = "121")

tracts <- tracts %>%
  rowwise() %>%
  filter(COUNTY %in% counties)

# Reproject tracts to match points

tracts <- tracts %>%
  st_transform(st_crs(asthma))

# Save tract geometries for use with GEE
tract.geo <- tracts %>% 
  select(GEOID)

write_sf(tract.geo, here("data", "tracts_geo.shp"))

# Asthma counts per tracts ------------------------------------------------
## filter out severity = 1 (this category does not represent acute incidents)

# Daily counts by geoid
counts <- asthma %>% 
  st_drop_geometry() %>% 
  filter(severity != 1) %>% 
  group_by(geoid, date) %>% 
  summarise(n = n()) %>% 
  ungroup()

# Make sure that there are values for every date in the series for every tract
# If there is an NA indicating no acute cases, replace it with 0
min <- min(counts$date)
max <- max(counts$date)

counts <- counts %>% 
  group_by(geoid) %>% 
  complete(date = seq.Date(min, max, by = "day")) %>% 
  ungroup() %>% 
  mutate(n = replace_na(n, 0))

# Join counts to the tract geometries
tracts <- tracts %>% 
  left_join(counts, by = "geoid") 

# Save tracts with social and asthma data
write_sf(tracts, here("data", "tracts.shp"))