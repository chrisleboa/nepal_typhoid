# This file joins the water sample positivity data and the water sample
#locations data

# Author: Chris LeBoa
# Version: 2020-02-05

# Libraries
library(tidyverse)
library(sf)

# Parameters
file_input_water_locations <-
  here::here("data/water_samples/water_sample_locations.shp")
file_input_water_positivity <-
  here::here("data/water_samples/water_sample_positivity.rds")

file_output_water_samples <-
  here::here("data/water_samples/water_samples.shp")
#===============================================================================

#Code

water_locations <- read_sf(file_input_water_locations)
positivity <- read_rds(file_input_water_positivity)

water_samples <-
  water_locations %>%
  left_join(positivity, by = c("Sample_ID" = "location")) %>%
  st_transform(4326) %>%
  write_sf(file_output_water_samples)
