# This reads in and cleans the data on water positivity from the 20
# River sampling points where water has been taken for the previous 4 months

# Author: Name
# Version: 2020-02-05

# Libraries
library(tidyverse)
library(sf)
library(readxl)

# Parameters
file_input_location <- here::here("data-raw/RiverSampling_Compilation.xlsx")
# file_output_location_typhi <-
#   here::here("data/water_samples/water_sample_positivity_typhi.rds")
file_output_location <-
  here::here("data/water_samples/water_sample_positivity.rds")

#===============================================================================

Code

df <- read_excel(file_input_location)

df_longer <-
  df %>%
  pivot_longer(
    cols = -location,
    names_to = "sampling_time",
    values_to = "ct_value"
  ) %>%
  separate(sampling_time, c("species", "time"), "_")

df_totals <-
  df_longer %>%
  mutate(ct_value = ifelse(ct_value < 35, 1, 0)) %>%
  group_by(location, species) %>%
  summarise(n_ctpos = sum(ct_value))

#Check work

# df_totals %>%
#   group_by(species) %>%
#   summarise(pos = sum(n_ctpos))

#write output files

# df_totals %>%
#   filter(species == "St") %>%
#   write_rds(file_output_location_typhi)
#
# df_totals %>%
#   filter(species == "Spt") %>%
#   write_rds(file_output_location_paratyphi)

df_totals %>%
  pivot_wider(names_from = species, values_from = n_ctpos) %>%
  write_rds(file_output_location)
