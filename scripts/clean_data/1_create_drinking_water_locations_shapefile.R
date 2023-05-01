# This pulls the drinking water cases from redcap and converts them to a geojson
#so that we can use in mapbox

# Author: Chris LeBoa
# Version: 2020-03-12

# Libraries
library(tidyverse)
library(sf)

# Parameters
redcap_data_location <- "/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data-raw/all_redcap_data_20200423.csv"


vars_interest <-
  cols_only(
    index_id = col_character(),
    country = col_character(),
    waterid = col_character(),
    watercolpt = col_double(),
    watercol_origsource = col_double(),
    watercolpit_sourcepipe = col_double(),
    waterid_manual = col_character(),
    hflat = col_double(),
    hflon = col_double()
  )


#===============================================================================

#Code


drinking_water <-
  read_csv(redcap_data_location, col_types = vars_interest) %>%
  filter ((!is.na(waterid) | !is.na(waterid_manual)) & country == "2") %>%
  mutate(object_id = index_id) %>%
  mutate(
    water_type = case_when(
      watercolpt == 1 ~ "pipe from unknown source",
      watercolpt == 2 ~ "Municipal supply",
      watercolpt == 3 ~ "Surface water",
      watercolpt == 3 ~ "Covered container",
      watercolpt == 7 ~ "Surface water",
      watercolpt == 8 ~ "Well",
      watercolpt == 9 ~ "Municipal supply",
      watercolpt == 10 ~ "Well",
      watercolpt == 10 ~ "Well",
      watercolpt == 12  ~ "Private company jug",
      watercol_origsource == 3 ~ "Private company",
      watercol_origsource == 2 ~ "Tanker truck",
      watercol_origsource == 1 ~ "Municipal supply",
      watercol_origsource == 4 ~ "Well",
      watercol_origsource == 5 ~ "Surface water",
      TRUE ~ ""),
      sample_id = if_else(!is.na(waterid), waterid, waterid_manual),
    latitude = hflat,
    longitude = hflon
  ) %>%
  drop_na(hflat, hflon)
  #st_as_sf(coords = c("hflon", "hflat"), crs = 4326)

glimpse(drinking_water)
table(drinking_water$watercolpt)

drinking_water_merged <-
  drinking_water %>%
  left_join(nepal_dw_lab_data, by = "sample_id")

view(nepal_dw_lab_data)

plot_drinking_water <-
  drinking_water_merged %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

nepal_map <- "/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/nepal_map/31Adist_polbnda_adm3_MoFALD_HRRP_wgs84.shp"

nepal <- st_read(nepal_map)

joined_drinking_water_pts <-  #add the regions from the nepal map to the drinking water locations
  plot_drinking_water %>%
  st_join(nepal)

drinking_water_merged %>%
  write_csv(here::here("data/water_samples/drinking_water_samples.csv"))




#drinking_water %>%
 # write_sf("/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/water_samples/drinking_water_samples.geojson")
#Giving error that cannot update this file becasue

st_write(drinking_water_merged, dsn = "drinking_water.shp", layer = "nc.shp", driver = "ESRI Shapefile")
## Writing layer `nc' to data source `nc1.shp' using driver `ESRI Shapefile'
## Writing 100 features with 14 fields and geometry type Multi Polygon.

