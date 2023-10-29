# Phage analysis distance from upstream

#This analysis was requested by Sneha for the phage paper af

# Author: Name
# Version: 2023-10-08

# Libraries
library(tidyverse)
library(lme4)
#install.packages("broom.mixed")
library(broom.mixed)
#install.packages("WeMix")
library(WeMix)

# Parameters
nepal_river_data_formatted <- read_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/rivers/rivers_formatted.csv")
#===============================================================================
nepal_river_data_formatted
#Code

# This was the code to clean the river data.
# I am reading in the clean data above but am leaving this here so you can see how the river distances were combined.
# nepal_river_data_formatted <-
#   nepal_river_data %>%
#   select(-c(river_pcr_typhi_run_date)) %>%
#   mutate_at(
#     vars(matches("river_pcr")),
#     ~as.double(recode(., "UD" = "99", .default = .)
#     )) %>%
#   mutate(
#     typhi_pos =
#       if_else(
#         river_pcr_typhi_1_t16_ct < 35 | river_pcr_typhi_2_t16_ct < 35, "Pos", "Neg"),
#     paratyphi_pos =
#       if_else(
#         river_pcr_paratyphi_1_t16_ct  < 35 | river_pcr_paratyphi_1_t16_ct < 35,
#         "Pos",
#         "Neg"
#       ),
#     typhi_border =
#       case_when(
#         river_pcr_paratyphi_1_t16_ct  < 35 | river_pcr_paratyphi_1_t16_ct < 35 ~ "Pos",
#         (river_pcr_paratyphi_1_t16_ct  > 35 & river_pcr_paratyphi_1_t16_ct  < 40) |
#           (river_pcr_paratyphi_1_t16_ct  > 35 & river_pcr_paratyphi_1_t16_ct  < 40) ~ "Border",
#         TRUE ~ "Neg"
#       ),
#     sample_date = floor_date(as_date(river_datetime), "week"),
#     sample_week = floor_date(sample_date, "week"),
#     redcap_event_name =
#       str_replace_all(str_extract(redcap_event_name, "[^a]+"), "_", " "),
#     redcap_event_name_parsed =
#       str_replace(redcap_event_name, "1b", "15"),
#     sample_month =
#       if_else(str_detect(redcap_event_name_parsed, "15"),
#               parse_date(redcap_event_name_parsed, format = "%Y %m %d "),
#               if_else(str_detect(redcap_event_name_parsed, " 1$"),
#                       parse_date(redcap_event_name_parsed, format = "%Y %m %d"),
#                       parse_date(redcap_event_name_parsed, format = "%Y %m ")
#               )),
#     stream_location = case_when(
#       str_detect(sample_id, "BAG-3|BIS-3|DBK-3|MAN-3") ~ -10,
#       str_detect(sample_id, "BAG-2|BIS-2|DBK-2|MAN-2")  ~ -5,
#       str_detect(sample_id, "BAG-1|BIS-1|DBK-1|MAN-1") ~ -1,
#       str_detect(sample_id, "COM-0")  ~ 0, #Need to ask Jason about this
#       str_detect(sample_id, "COM-1") ~ 1,
#       str_detect(sample_id, "COM-2") ~ 5,
#       str_detect(sample_id, "COM-3") ~ 10,
#       TRUE ~ NA_real_
#     ),
#     river_name = str_extract(sample_id, "^(.+?)-")
#   ) %>%
#   drop_na(typhi_pos)


nepal_river_data_formatted %>%
  mutate(
    stream = case_when(
      stream_location == - 10 ~ "upstream",
      stream_location == - 5 ~ "city_center",
      stream_location == - 1 ~ "city_center",
      stream_location >= 0 ~ "downstream"
      #,
     # is.na(stream_location) ~"kavre"
    ),
    typhi_pos_num = if_else(str_detect(typhi_pos, "Pos") == TRUE, 1, 0),
    paratyphi_pos_num = if_else(str_detect(paratyphi_pos, "Pos") == TRUE, 1, 0)
  ) %>%
  group_by(stream) %>%
  summarise(
    num_pos_typhi = sum(river_phage_typhi, na.rm = TRUE),
    num_pos_para = sum(river_phage_paratyphi, na.rm = TRUE),
    mean_pos_typhi = mean(river_phage_typhi_count, na.rm = TRUE),
    mean_pos_paratyphi = mean(river_phage_paratyphi_count, na.rm = TRUE),
    count = n(),
    prop_pos_typhi = num_pos_typhi / count * 100,
    prop_pos_paratyphi = num_pos_typhi / count * 100
  ) %>%
  write.csv("/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/phages/phage_positivity_by_distance.csv")




