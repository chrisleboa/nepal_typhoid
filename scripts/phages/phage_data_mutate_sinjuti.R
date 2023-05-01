# This file turns data into the form that sanjuti needs

# Author: chris LeBoa
# Version: 2022-04-05

# Libraries
library(tidyverse)

# Parameters

#===============================================================================

Code

nepal_river_data_formatted %>%
  transmute(
    Country_zoom = "Nepal",
    sample_id = str_glue("{sample_id}_{redcap_event_name}"),
    SampleColDate = river_datetime,
    Province = if_else(str_detect(sample_id, "PN") == TRUE, "Kavre", "Kathmandu"),
    District = if_else(str_detect(sample_id, "PN") == TRUE, "Kavre", "Kathmandu"),
    Lattitude = river_latitude,
    Longitude = river_longitude,
    Source = "River",
    Enrichment = "N/A",
    Plaques_obs = if_else(river_phage_typhi == 1 | river_phage_paratyphi == 1, 1, 0),
    Host = case_when(
      river_phage_typhi == 1 & river_phage_paratyphi == 1 ~ "S.Typhi and S. Paratyphi",
      river_phage_typhi == 1 & river_phage_paratyphi == 0 ~ "S.Typhi",
      river_phage_typhi == 0 & river_phage_paratyphi == 1 ~ "S.Paratyphi",
      TRUE ~ "None")

  ) %>%
  write_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/phages/Phage_cleaned.csv")
