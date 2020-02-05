# This file plots the points where water is collected in Kathmandu for
# environmental surveillance of typhi and paratyphi.

#Water is gathered from these points on a monthly basis from Oct 2019 - 2020

# Author: Chris LeBoa
# Version: 2020-02-05

# Libraries
library(tidyverse)

# Parameters
file_input_location <- here::here("data-raw/water_sample_locations/Water_Samples.shp")
file_output_location <- here::here("data/water_samples/water_sample_locations.shp")
#===============================================================================

#Code

read_sf(file_input_location) %>%
  drop_na(geometry) %>%
  select(Sample_ID, geometry) %>%
  write_sf(file_output_location)
