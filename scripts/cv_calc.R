## CV Calculations
## This script calculates the CV values across the sample duplicates, same day duplicates
## and same week duplicates. It is meant to show the variation within duplicates

## author: Chris LeBoa
## edited: 7/6/2021

# input: Nepal River data formatted
# output



cv_clean <-
  nepal_river_data_formatted %>%
  filter(!is.na(redcap_repeat_instance)) %>%
  select(sample_id, redcap_repeat_instance, redcap_event_name, sample_date, river_pcr_typhi_1_t16_ct, river_pcr_typhi_2_t16_ct) %>%
  rowwise() %>%    # Used to make calculations across columns in the same row
  mutate(
    pcr_1 = if_else(river_pcr_typhi_1_t16_ct == 99, 40, river_pcr_typhi_1_t16_ct),   ## Changing 99 to NA for undetected samples
    pcr_2 = if_else(river_pcr_typhi_2_t16_ct == 99, 40, river_pcr_typhi_2_t16_ct),
    cv = sd(c(pcr_1, pcr_2)) / mean(c(pcr_1, pcr_2), na.rm = TRUE)  ## CV calculation
  ) %>%
  select(-c(river_pcr_typhi_1_t16_ct, river_pcr_typhi_2_t16_ct, cv))

# Sample duplicate

cv_sample <-
  cv_clean %>%
  mutate(
    cv = sd(c(pcr_1, pcr_2)) / mean(c(pcr_1, pcr_2), na.rm = TRUE)  ## CV calculation
  )


#within_day duplicate
cv_day <-
cv_clean %>%
  pivot_wider(names_from = "redcap_repeat_instance", values_from = c("pcr_1", "pcr_2")) %>%
  rowwise() %>%
  mutate(
    cv_day = sd(c(pcr_1_1, pcr_1_2, pcr_2_1, pcr_2_2)) / mean(c(pcr_1_1, pcr_1_2, pcr_2_1, pcr_2_2), na.rm = TRUE)
  )

#within_week_duplicate

cv_week <-
nepal_river_data_formatted %>%
  filter(
    str_detect(redcap_event_name, "2019 11 1| 2019 11 1b"),
    redcap_repeat_instance == 1 | is.na(redcap_repeat_instance)
     ) %>%
  select(sample_id, redcap_repeat_instance, redcap_event_name, river_pcr_typhi_1_t16_ct, river_pcr_typhi_2_t16_ct) %>%
  rowwise() %>%    # Used to make calculations across columns in the same row
  mutate(
    pcr_1 = if_else(river_pcr_typhi_1_t16_ct == 99, 40, river_pcr_typhi_1_t16_ct),   ## Changing 99 to NA for undetected samples
    pcr_2 = if_else(river_pcr_typhi_2_t16_ct == 99, 40, river_pcr_typhi_2_t16_ct),
    cv = sd(c(pcr_1, pcr_2)) / mean(c(pcr_1, pcr_2), na.rm = TRUE)  ## CV calculation
  ) %>%
  select(-c(river_pcr_typhi_1_t16_ct, river_pcr_typhi_2_t16_ct, cv, redcap_event_name)) %>%
  pivot_wider(names_from = "redcap_repeat_instance", values_from = c("pcr_1", "pcr_2")) %>%
  rowwise() %>%
  mutate(
    cv_week = sd(c(pcr_1_NA, pcr_1_1, pcr_2_1, pcr_2_NA)) / mean(c(pcr_1_NA, pcr_1_1, pcr_2_1, pcr_2_NA), na.rm = TRUE)
  )

cv_sample %>%
  write_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/rivers/cv_sample.csv")
cv_day %>%
  write_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/rivers/cv_day.csv")
cv_week %>%
  write_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/rivers/cv_week.csv")



cv_sample %>%
  ungroup() %>%
  summarize(mean(cv, na.rm = TRUE))

cv_day %>%
  ungroup() %>%
  summarize(mean(cv_day, na.rm = TRUE))

cv_week %>%
  ungroup() %>%
  summarize(mean(cv_week, na.rm = TRUE))
