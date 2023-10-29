# River water Positivity tables. For a conference on typhoid we wanted to create summary tables on
# total sample positivity. We

# Author: Chris LeBoa
# Version: 2021-07-13

# Libraries
library(tidyverse)
library(irr)
library(lubridate)
# Parameters
river_cleaned_data_location <- "/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/rivers/rivers_formatted.csv" #This file is generated in the scripts <- clean_data folder <- 2_clean_riversample_data
hydrology_data_location <- here::here("data/water_samples/EStyphi_KTM_generalresults.xlsx")

#Overall River Water Positive

nepal_river_data_formatted <- read_csv(river_cleaned_data_location)
nepal_hydrology_data <- readxl::read_excel(hydrology_data_location) %>%
  select(Sample_ID, escatch.upstrmpop) %>%
  mutate(sample_id = str_replace(Sample_ID, "_", "-"))


table_cleaned_data <-
nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  left_join(nepal_hydrology_data, by = "sample_id") %>%
  mutate(
    month = month(sample_month),
    monsoon = if_else(month %in% c("6", "7", "8", "9"), 1, 0),
    segment = case_when(
      str_detect(sample_id, "BAG-3|DBK-3|MAN-3|BIS-3") == TRUE ~ "0", #upstream
      str_detect(sample_id, "BAG-1|BIS-1|MAN-1|COM-0|BAG-2|BIS-2|MAN-2|DBK-1|DBK-2") == TRUE ~ "1", #Center area
      str_detect(sample_id, "COM-1|COM-2|COM-3") == TRUE ~ "2", #downstream
      .default = NA), #Punymata
    region = if_else(str_detect(sample_id, "PN-1|PN-2|PN-3") == TRUE, "Kavre", "Kathmandu"),
    typhi_pos_num = if_else(river_pcr_typhi_1_t16_ct <= 35 | river_pcr_typhi_2_t16_ct <= 35, 1, 0),
    paratyphi_pos_num = if_else(river_pcr_paratyphi_1_t16_ct <= 35 | river_pcr_paratyphi_2_t16_ct <= 35, 1, 0),
    typhi_t0_pos_num = if_else(river_pcr_typhi_1_t0_ct <= 35 | river_pcr_typhi_2_t0_ct <= 35, 1, 0),
    paratyphi_t0_pos_num = if_else(river_pcr_paratyphi_1_t0_ct <= 35 | river_pcr_paratyphi_2_t0_ct <= 35, 1, 0),
    typhi_delta_pos_num =
      if_else(
      (river_pcr_typhi_1_t16_ct <= 35 & river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct >= 3) |
        (river_pcr_typhi_2_t16_ct <= 35 & river_pcr_typhi_2_t0_ct - river_pcr_typhi_2_t16_ct >= 3), 1, 0),
    paratyphi_delta_pos_num =
      if_else(
        (river_pcr_paratyphi_1_t16_ct <= 35 & river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct >= 3) |
          (river_pcr_paratyphi_2_t16_ct <= 35 & river_pcr_paratyphi_2_t0_ct - river_pcr_paratyphi_2_t16_ct >= 3), 1, 0)
  )
  #group_by(sample_id) %>%

#### The following four blocks of code output the total number of positives by grouping variable
###  They then write out the resulting tables as a CSV which are combined to create table 2 in the text

#Overall
table_cleaned_data %>% #Could write this as function at some point if i find the time but group_by for the overall one wouldn't really work
  summarise(
    typhi_phage_pos = sum(river_phage_typhi, na.rm = TRUE),
    paratyphi_phage_pos = sum(river_phage_paratyphi, na.rm = TRUE),
    typhi_pos = sum(typhi_pos_num),
    typhi_t0_pos = sum(typhi_t0_pos_num),
    paratyphi_pos = sum(paratyphi_pos_num, na.rm = TRUE),
    paratyphi_t0_pos = sum(paratyphi_t0_pos_num, na.rm = TRUE),
    typhi_total = sum(!is.na(typhi_pos_num)),
    pt_total = sum(!is.na(paratyphi_pos_num)),
    typhi_35_pct = typhi_pos / typhi_total * 100,
    pt_35_pct = paratyphi_pos / pt_total *100,
    typhi_pos_delta = sum(typhi_delta_pos_num, na.rm = TRUE),
    paratyphi_pos_delta = sum(paratyphi_delta_pos_num, na.rm = TRUE),
    typhi_delta_total = sum(!is.na(typhi_delta_pos_num)),
    pt_delta_total = sum(!is.na(paratyphi_delta_pos_num )),
    typhi_delta_pct = typhi_pos_delta / typhi_delta_total * 100,
    pt_delta_pct = paratyphi_pos_delta / pt_delta_total *100
  )

#Seasonal
table_cleaned_data %>%
group_by(monsoon) %>%
  summarise(
  typhi_phage_pos = sum(river_phage_typhi, na.rm = TRUE),
  paratyphi_phage_pos = sum(river_phage_paratyphi, na.rm = TRUE),
  typhi_pos = sum(typhi_pos_num),
  typhi_t0_pos = sum(typhi_t0_pos_num),
  paratyphi_pos = sum(paratyphi_pos_num, na.rm = TRUE),
  paratyphi_t0_pos = sum(paratyphi_t0_pos_num, na.rm = TRUE),
  typhi_total = sum(!is.na(typhi_pos_num)),
  pt_total = sum(!is.na(paratyphi_pos_num)),
  typhi_35_pct = typhi_pos / typhi_total * 100,
  pt_35_pct = paratyphi_pos / pt_total *100,
  typhi_pos_delta = sum(typhi_delta_pos_num, na.rm = TRUE),
  paratyphi_pos_delta = sum(paratyphi_delta_pos_num, na.rm = TRUE),
  typhi_delta_total = sum(!is.na(typhi_delta_pos_num)),
  pt_delta_total = sum(!is.na(paratyphi_delta_pos_num )),
  typhi_delta_pct = typhi_pos_delta / typhi_delta_total * 100,
  pt_delta_pct = paratyphi_pos_delta / pt_delta_total *100
) %>%
  select(monsoon, typhi_total, typhi_pos, typhi_pos_delta, paratyphi_pos, paratyphi_pos_delta, typhi_35_pct, pt_35_pct ) %>%
  write_csv(., "season_total.csv")

#Distance
table_cleaned_data %>%
  group_by(segment) %>%
  summarise(
    typhi_phage_pos = sum(river_phage_typhi, na.rm = TRUE),
    paratyphi_phage_pos = sum(river_phage_paratyphi, na.rm = TRUE),
    typhi_pos = sum(typhi_pos_num),
    typhi_t0_pos = sum(typhi_t0_pos_num),
    paratyphi_pos = sum(paratyphi_pos_num, na.rm = TRUE),
    paratyphi_t0_pos = sum(paratyphi_t0_pos_num, na.rm = TRUE),
    typhi_total = sum(!is.na(typhi_pos_num)),
    pt_total = sum(!is.na(paratyphi_pos_num)),
    typhi_35_pct = typhi_pos / typhi_total * 100,
    pt_35_pct = paratyphi_pos / pt_total *100,
    typhi_pos_delta = sum(typhi_delta_pos_num, na.rm = TRUE),
    paratyphi_pos_delta = sum(paratyphi_delta_pos_num, na.rm = TRUE),
    typhi_delta_total = sum(!is.na(typhi_delta_pos_num)),
    pt_delta_total = sum(!is.na(paratyphi_delta_pos_num )),
    typhi_delta_pct = typhi_pos_delta / typhi_delta_total * 100,
    pt_delta_pct = paratyphi_pos_delta / pt_delta_total *100
  ) %>%
  select(segment, typhi_total, typhi_pos, typhi_pos_delta, paratyphi_pos, paratyphi_pos_delta, typhi_35_pct, pt_35_pct ) %>%
  write_csv(., "segment_total.csv")

#Regional difference
table_cleaned_data %>%
  group_by(region) %>%
  summarise(
    typhi_phage_pos = sum(river_phage_typhi, na.rm = TRUE),
    paratyphi_phage_pos = sum(river_phage_paratyphi, na.rm = TRUE),
    typhi_pos = sum(typhi_pos_num),
    typhi_t0_pos = sum(typhi_t0_pos_num),
    paratyphi_pos = sum(paratyphi_pos_num, na.rm = TRUE),
    paratyphi_t0_pos = sum(paratyphi_t0_pos_num, na.rm = TRUE),
    typhi_total = sum(!is.na(typhi_pos_num)),
    pt_total = sum(!is.na(paratyphi_pos_num)),
    typhi_35_pct = typhi_pos / typhi_total * 100,
    pt_35_pct = paratyphi_pos / pt_total *100,
    typhi_pos_delta = sum(typhi_delta_pos_num, na.rm = TRUE),
    paratyphi_pos_delta = sum(paratyphi_delta_pos_num, na.rm = TRUE),
    typhi_delta_total = sum(!is.na(typhi_delta_pos_num)),
    pt_delta_total = sum(!is.na(paratyphi_delta_pos_num )),
    typhi_delta_pct = typhi_pos_delta / typhi_delta_total * 100,
    pt_delta_pct = paratyphi_pos_delta / pt_delta_total *100
  ) %>%
  select(region, typhi_total, typhi_pos, typhi_pos_delta, paratyphi_pos, paratyphi_pos_delta, typhi_35_pct, pt_35_pct ) %>%
  write_csv(., "region_total.csv")

## Mixed effects model to understand the relationship between each grouping variable and outcome
## The data is not i.i.d and so using a mixed effect model
## The outcome is a binomial so use the binomial family of generalized linear models.
     ## The LME4 package is
library(lme4)
#install.packages("broom.mixed")
library(broom.mixed)
install.packages("WeMix")
library(WeMix)

#We analyze for statistical significance using the generalized linear mixed effect model

## IF the outcome was not binomial we would use a lmer model and anova to assess statiostical significance of the results.
## This is a good resource https://www.stat.cmu.edu/~hseltman/309/Book/chapter15.pdf
# null_model_segment =  glmer(typhi_pos_num ~ (1|sample_id), data = table_cleaned_data %>% filter(!is.na(segment)), REML = FALSE, family = "binomial")
# null_model =  lmer(typhi_pos_num ~ (1|sample_id), data = table_cleaned_data, family = "binomial")
#
# season_mixed = lmer(paratyphi_pos_num ~ monsoon + (1|sample_id), data = table_cleaned_data)
# summary(season_mixed)
# confint(season_mixed) #We also could present the beta and confidence intervals
# anova(season_mixed, null_model)


#Analyzing monsoon vs not monsoon

season_mixed = glmer(typhi_pos_num ~ monsoon + (1|sample_id), data = table_cleaned_data, family = "binomial")
summary(season_mixed)

season_mixed_pt = glmer(paratyphi_pos_num ~ monsoon + (1|sample_id), data = table_cleaned_data, family = "binomial")
summary(season_mixed_pt)
#confint(season_mixed) #We also could present the beta and confidence intervals
#anova(season_mixed, null_model)

# The region variable is Typhi vs Paratyphi
region_mixed = glmer(typhi_pos_num ~ region + (1|sample_id), data = table_cleaned_data, family = "binomial")
summary(region_mixed)
#confint(region_mixed) #We also could present the beta and confidence intervals
#anova(region_mixed, null_model)
region_mixed_pt = glmer(paratyphi_pos_num ~ region + (1|sample_id), data = table_cleaned_data, family = "binomial")
summary(region_mixed_pt)

#the distance is the distance along the stream
distance_mixed = glmer(paratyphi_pos_num ~ segment + (1|sample_id), data = table_cleaned_data %>% filter(!is.na(segment)), family = "binomial")
summary(distance_mixed)
exp(confint(distance_mixed, parm = c("segment1", "segment2")))



## Interactions with River water

# # The following code filters by the instances when people were using the rivers
#in different ways and cross checks them with if the water sample from that location had detectable levels of S. Typhi and S. Paratyphi
nepal_river_data_formatted %>%
  filter(river_wash_veg == 1, str_detect(typhi_pos,"Pos") == TRUE)

nepal_river_data_formatted %>%
  filter(river_wash_veg == 1, str_detect(paratyphi_pos,"Pos") == TRUE)

nepal_river_data_formatted %>%
  filter(river_walk_across == 1, str_detect(typhi_pos,"Pos") == TRUE)
nepal_river_data_formatted %>%
  filter(river_walk_across == 1, str_detect(paratyphi_pos,"Pos") == TRUE)

nepal_river_data_formatted %>%
  filter(river_wash_clothes == 1, str_detect(typhi_pos,"Pos") == TRUE)
nepal_river_data_formatted %>%
  filter(river_wash_clothes == 1, str_detect(paratyphi_pos,"Pos") == TRUE)

nepal_river_data_formatted %>%
  filter(river_bathing == 1, str_detect(typhi_pos,"Pos") == TRUE)
nepal_river_data_formatted %>%
  filter(river_bathing == 1, str_detect(paratyphi_pos,"Pos") == TRUE)

#This code counts the number of sites at which human interaction with the river water occured
table_cleaned_data %>%
  filter(river_wash_veg == 1) %>%
  count(sample_id)
table_cleaned_data %>%
  filter(river_walk_across == 1) %>%
  count(sample_id)
table_cleaned_data %>%
  filter(river_wash_clothes == 1) %>%
  count(sample_id)
table_cleaned_data %>%
  filter(river_bathing == 1) %>%
  count(sample_id)


#Write samples that are positive for paratyphi into a csv for export for manuscript
nepal_river_data_formatted %>% filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>% group_by(sample_id) %>%  count(paratyphi_pos) %>% filter(paratyphi_pos == "Pos") %>% write_csv(.,"paratyphi_pos_sample_id.csv")

