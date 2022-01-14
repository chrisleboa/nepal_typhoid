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
  drop_na(hflat, hflon) %>%
  st_as_sf(coords = c("hflon", "hflat"), crs = 4326)

drinking_water %>% write_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/water_samples/drinking_water_samples.csv")


drinking_water %>%
  write_sf("/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/water_samples/drinking_water_samples.geojson")


st_write(drinking_water, dsn = "drinking_water.shp", layer = "nc.shp", driver = "ESRI Shapefile")
## Writing layer `nc' to data source `nc1.shp' using driver `ESRI Shapefile'
## Writing 100 features with 14 fields and geometry type Multi Polygon.

