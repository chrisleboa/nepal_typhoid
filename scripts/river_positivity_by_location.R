# River Positivity by location
## Creates table of sample positivity to send to Chris Uzzelli

# Author: Chris LeBoa
# Version: 2021-11-09

# Libraries
library(tidyverse)
library(irr)
library(lubridate)



# Parameters

output_location <- here::here("data/water_samples/river_location_positivity/positivity_by_location.csv")

nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  mutate(
    typhi_pos_num = if_else(river_pcr_typhi_1_t16_ct <= 35 | river_pcr_typhi_2_t16_ct <= 35, 1, 0),
    paratyphi_pos_num = if_else(river_pcr_paratyphi_1_t16_ct <= 35 | river_pcr_paratyphi_2_t16_ct <= 35, 1, 0),
    typhi_delta_pos_num =
      if_else(
        (river_pcr_typhi_1_t16_ct <= 35 & river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct >= 3) |
          (river_pcr_typhi_2_t16_ct <= 35 & river_pcr_typhi_2_t0_ct - river_pcr_typhi_2_t16_ct >= 3), 1, 0),
    paratyphi_delta_pos_num =
      if_else(
        (river_pcr_paratyphi_1_t16_ct <= 35 & river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct >= 3) |
          (river_pcr_paratyphi_2_t16_ct <= 35 & river_pcr_paratyphi_2_t0_ct - river_pcr_paratyphi_2_t16_ct >= 3), 1, 0)
  ) %>%
  group_by(sample_id) %>%
  summarise(
    typhi_dna_pos = sum(typhi_pos_num, na.rm = TRUE),
    paratyphi_dna_pos = sum(paratyphi_pos_num, na.rm = TRUE),
    typhi_phage_pos = sum(river_phage_typhi, na.rm = TRUE),
    paratyphi_phage_pos = sum(river_phage_paratyphi, na.rm = TRUE)
                        ) %>%
  write_csv(output_location)

