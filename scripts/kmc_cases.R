# This script reads in case data for Kathmandu Cases

# Author: Chris LeBoa
# Version: 2020-02-04

# Libraries
library(tidyverse)
library(sf)

# Parameters
file_input_location <- here::here("data-raw/cases/cases_kmc_20200204/Cases.shp")
file_output_location <- here::here("data/cases/kmc_20200204.shp")
#===============================================================================

#Code

read_sf(file_input_location) %>%
  select(Case_Type, Case_Name, SEAP_ID, geometry) %>%
  filter(SEAP_ID != 0) %>%
  drop_na(geometry) %>%
  write_sf(file_output_location)
