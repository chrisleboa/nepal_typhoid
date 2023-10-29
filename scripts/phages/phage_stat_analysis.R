# Phage positivity statistical significance

##We use Mixed effects model to understand the relationship between each grouping variable and outcome
##  The data is not i.i.d and so using a mixed effect model
## The outcome is a binomial so use the binomial family of generalized linear models.

#We analyze for statistical significance using the generalized linear mixed effect model
## IF the outcome was not binomial we would use a lmer model and anova to assess statiostical significance of the results.
## This is a good resource https://www.stat.cmu.edu/~hseltman/309/Book/chapter15.pdf


# Author: Christopher LeBoa
# Version: 2023-10-08

# Libraries
library(tidyverse)
library(irr)
library(lubridate)
library(lme4)
library(broom.mixed)
library(WeMix)
library(sjPlot)

# Parameters
river_cleaned_data_location <- "/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/rivers/rivers_formatted.csv" #This file is generated in the scripts <- clean_data folder <- 2_clean_riversample_data
hydrology_data_location <- here::here("data/water_samples/EStyphi_KTM_generalresults.xlsx")
#===============================================================================

#Code

#Read in datasets
nepal_river_data_formatted <- read_csv(river_cleaned_data_location)
nepal_hydrology_data <- readxl::read_excel(hydrology_data_location) %>%
  select(Sample_ID, escatch.upstrmpop) %>%
  mutate(sample_id = str_replace(Sample_ID, "_", "-"))

#Clean data to make a monsoon variable
table_cleaned_data_v2 <-
  nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  left_join(nepal_hydrology_data, by = "sample_id") %>%
  mutate(
    month = month(sample_month),
    monsoon = if_else(month %in% c("6", "7", "8", "9"), 1, 0),
    segment = case_when(
      str_detect(sample_id, "BAG-3|DBK-3|MAN-3|BIS-3") == TRUE ~ "upstream", #upstream
      str_detect(sample_id, "BAG-1|BIS-1|MAN-1|COM-0|BAG-2|BIS-2|MAN-2|DBK-1|DBK-2") == TRUE ~ "city center", #Center area
      str_detect(sample_id, "COM-1|COM-2|COM-3") == TRUE ~ "downstream", #downstream
      .default = NA), #Punymata
    segment = relevel(factor(segment), ref = "upstream"),
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


#Analyzing monsoon vs not monsoon

season_mixed = glmer(river_phage_typhi ~ monsoon + (1|sample_id), data = table_cleaned_data, family = "binomial")
summary(season_mixed)

season_mixed_pt = glmer(river_phage_paratyphi ~ monsoon + (1|sample_id), data = table_cleaned_data, family = "binomial")
summary(season_mixed_pt)
#confint(season_mixed) #We also could present the beta and confidence intervals
#anova(season_mixed, null_model)

# The region variable is Typhi vs Paratyphi
region_mixed = glmer(river_phage_typhi ~ region + (1|sample_id), data = table_cleaned_data, family = "binomial")
summary(region_mixed)
#confint(region_mixed) #We also could present the beta and confidence intervals
#anova(region_mixed, null_model)
region_mixed_pt = glmer(river_phage_paratyphi ~ region + (1|sample_id), data = table_cleaned_data, family = "binomial")
summary(region_mixed_pt)

#the distance is the distance along the stream

##Segment 1 is city center and segment 2 is downsream

distance_mixed_typhi = glmer(river_phage_typhi ~ segment + (1|sample_id), data = table_cleaned_data_v2 %>% filter(!is.na(segment)), family = "binomial")
summary(distance_mixed)
tidy(distance_mixed_typhi,conf.int=TRUE,exponentiate=TRUE)


distance_mixed_paratyphi = glmer(river_phage_paratyphi ~ segment + (1|sample_id), data = table_cleaned_data_v2 %>% filter(!is.na(segment)), family = "binomial")
summary(distance_mixed)

exp(confint(distance_mixed, parm = c("segment1", "segment2")))

tidy(distance_mixed,conf.int=TRUE,exponentiate=TRUE)

tab_model(distance_mixed_typhi, distance_mixed_paratyphi)

