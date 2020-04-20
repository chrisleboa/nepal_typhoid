# This pulls the drinking water cases from redcap and converts them to a geojson
#so that we can use in mapbox 

# Author: Chris LeBoa
# Version: 2020-03-12

# Libraries
library(tidyverse)
library(sf)

# Parameters
redcap_data_location <- "/Users/ChrisLeBoa/GitHub/nepal_typhoid/data-raw/cases/all_redcap_data/sees_data_all_20200223.csv"

vars_interest <- 
  cols_only(
    index_id = col_double(),
    water_ra = col_character(), 
    hflat = col_double(),
    hflon = col_double()
  )


#===============================================================================

#Code

drinking_water <- 
  read_csv(redcap_data_location, col_types = vars_interest) %>% 
  drop_na() %>% 
  filter(hflon > 80, hflat < 33) %>% 
  st_as_sf(coords = c("hflon", "hflat")) 

drinking_water %>% 
  write_sf("/Users/ChrisLeBoa/GitHub/nepal_typhoid/data/water_samples/drinking_water_samples.geojson")
  
