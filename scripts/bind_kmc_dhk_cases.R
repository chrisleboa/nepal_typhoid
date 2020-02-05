# This script appends the case data for the Kathmandu and Dhulikhel Cases

# Author: Chris LeBoa
# Version: 2020-02-04

# Libraries
library(tidyverse)
library(sf)

# Parameters
file_input_location_dhk <- here::here("data/cases/dhk_20200204.shp")
file_input_location_kmc <- here::here("data/cases/kmc_20200204.shp")

file_output_location_all_cases <- here::here("data/cases/all_cases.shp")
#===============================================================================

#Code

cases_dhk <- read_sf(file_input_location_dhk)
cases_kmc <- read_sf(file_input_location_kmc)

cases <- rbind(cases_kmc, cases_dhk)

cases %>%
  write_sf(file_output_location_all_cases)
