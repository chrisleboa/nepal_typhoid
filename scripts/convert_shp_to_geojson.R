# This R scripst transforms shape files to KML files or geojsons to be
#uploaded to Mapbox

# Author: Chris LeBoa
# Version: 2020-02-18

# Libraries
library(tidyverse)
library(geojsonio)

#----Parameters

file_location_cases <- here::here("data/cases/all_cases.shp")
file_location_rivers <- here::here("data/rivers/rivers.shp")
file_location_water_samples <- here::here("data/water_samples/water_samples.shp")

file_location_cases_geojson <- here::here("data/cases/all_cases.geojson")
file_location_rivers_geojson <- here::here("data/rivers/rivers.geojson")
file_location_water_samples_geojson <-
  here::here("data/water_samples/water_samples.geojson")



#===============================================================================

#----Code

#Read in Data
cases <- read_sf(file_location_cases)
rivers <- read_sf(file_location_rivers)
water_positivity <- read_sf(file_location_water_samples)


#Write out Geojsons

geojson_write(cases, geometry = "point", file = file_location_cases_geojson)
geojson_write(water_positivity, geometry = "point",
              file = file_location_water_samples_geojson)
geojson_write(rivers, geometry = "polygon",
              file = file_location_rivers_geojson)



