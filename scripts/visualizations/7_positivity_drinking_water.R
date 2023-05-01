# Positivity Tables: Drinking water

# Author: Chris LeBoa
# Version: 2022-11-04

# Libraries
library(tidyverse)

# Parameters

#===============================================================================

#Code

water_pts_analysis <-
  joined_drinking_water_pts %>%
  st_drop_geometry() %>%
  mutate(
    region = if_else(HRRP_DNAME == "Lalitpur", "Kathmandu", HRRP_DNAME),
    source = case_when(
      watercol_origsource == 1 ~ "Municipal",
      watercol_origsource == 2 ~ "Tanker",
      watercol_origsource == 3 ~ "Private company",
      watercol_origsource == 4 ~ "Pipe",
      watercol_origsource == 5 ~ "Surface water",
      water_type == "Municipal supply" ~ "Municipal",
      water_type == "Well" ~ "Well",
      water_type == "Private company jug" ~ "Private company",
      water_type == "pipe from unknown source" ~ "Pipe",
      water_type == "Surface water" ~ "Surface water"
      ))
    #calling the lalitpur point Kathmandu because Lalitpur is near KTM

joined_drinking_water_pts %>%
  st_drop_geometry() %>%
  count(watercolpt)

joined_drinking_water_pts %>%
  st_drop_geometry() %>%
  count(watercol_origsource, water_type)

### From comments on paper draft was asked to do subgroup analysis based on the locations

#1  is municipal
# 2 is tanker truck
# 3 is private company jug
# 4 is pipe from unknown source
# 5 is surface water
#view(joined_drinking_water_pts)

water_pts_analysis %>%
  drop_na(region) %>%
  group_by(region) %>%
  count(source) %>%
  pivot_wider(names_from = "region", values_from = "n")




joined_drinking_water_pts <-
joined_drinking_water_pts %>%
  st_drop_geometry() %>%
  mutate(across(where(is.character), ~na_if(., "UD"))) %>%
  mutate(
    pcr_typhi1_dup1_ct = as.numeric(pcr_typhi1_dup1_ct),
    pcr_typhi1_dup2_ct = as.numeric(pcr_typhi1_dup2_ct),
    pcr_paratyphi1_dup1_ct = as.numeric(pcr_paratyphi1_dup1_ct),
    pcr_paratyphi1_dup2_ct = as.numeric(pcr_paratyphi1_dup2_ct),
    pos_typhi = if_else(pcr_typhi1_dup1_ct <40.0 | pcr_typhi2_dup2_ct < 40.0, 1, 0),
    pos_paratyphi = if_else(pcr_paratyphi1_dup1_ct <40.0 | pcr_paratyphi1_dup2_ct < 40.0, 1, 0)
         )

joined_drinking_water_pts %>%
  count(pos_typhi, pos_paratyphi)

joined_drinking_water_pts %>%
  filter(pos_typhi == 1 | pos_paratyphi == 1) %>%
count(HRRP_DNAME, water_type)



############## The following code was to figure out which of the prior dataset samples were positive and is now depreciated#####


#The dataset received for uploading to redcap had more positives than other dataset
nepal_dw_lab_data %>%
  mutate(
    pos_typhi = if_else(pcr_typhi1_mean_ct <40 | pcr_typhi2_mean_ct < 40, 1, 0),
    pos_paratyphi = if_else(pcr_paratyphi1_mean_ct <40 | pcr_paratyphi2_mean_ct < 40, 1, 0)
  ) %>%
  count(pos_typhi, pos_paratyphi)

#Figuring out which drinking water samples were positive
nepal_dw_lab_data %>% filter(pcr_paratyphi1_mean_ct <40 | pcr_paratyphi2_mean_ct < 40)

#Trying to find these sample ids
drinking_water_merged %>%
  filter(sample_id == "N0035")

#we see when we look at the sample ids from the joined drinking water points we see that many of the sample ids from the lab id form are not there
joined_drinking_water_pts$sample_id


