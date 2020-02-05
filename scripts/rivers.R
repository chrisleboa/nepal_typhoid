# This file plots the rivers of Kathmandu

# Author: Chris LeBoa
# Version: 2020-02-05

# Libraries
library(tidyverse)

# Parameters
file_input_location <- here::here("data-raw/rivers_ktm/Rivers_noponds.shp")
file_output_location <- here::here("data/rivers/rivers.shp")
#===============================================================================

#Code

read_sf(file_input_location) %>%
  drop_na(geometry) %>%
  write_sf(file_output_location)
