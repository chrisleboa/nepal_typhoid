# Creates a table one of drinking water results to show that the households where
#drinking water was taken are representative of the seap population

# Author: Chris LeBoa
# Version: 2021-05024

# Libraries
library(tidyverse)
library(sf)
library(tableone)

# Parameters
redcap_data_location <- "/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data-raw/all_redcap_data_20200423.csv"

 # Recode
toilet_recode <- c(
  `0` = "No toilet/bush/field",
  `1` =  "Flush to sewer system",
  `2` = "Flush to septic tank",
  `3` = "Flush to somewhere else",
  `4` = "Pit latrine",
  `5` = "Bucket or hanging toilet",
  .default = NA
  )

dr_water_recode <-
  c(
  `1` = "Municipal water",
  `2` = "Private water company",
  `3` =  "Groundwater (tube well/borehole/well)",
  `4` = "Surface water",
  `5` =  "Rainwater",
  `6` = "Bottled water",
  .default = NA
  )

mother_school_recode <- c(
  `1` = "No Schooling",
 `2` = "No Schooling",
  `3` = "Primary",
  `4` = "Secondary",
  `5` = "College",
  `6` = "College",
  `7` =  "College",
  `8` = "Vocational School",
  .default = NA
)


#===============================================================================

#Code
water_samples <-
  read_csv(redcap_data_location) %>%
  filter (country == "2", consent_complete == "2") %>%
  mutate(
    water_sampled = if_else(!is.na(waterid) | !is.na(waterid_manual), "Water Sample Taken", "No Sample Taken"),
    toilet = recode(hftoilet, !!! toilet_recode),
    toilet_share = if_else(hftoiletshare == 1, "Yes", "No"),
    drinking_water = recode(hfdrwater, !!! dr_water_recode),
    treat_drink_water = if_else(hftreatdkwater == "1", "Yes", "No"),
    monthly_family_income = hf_famincome_nepal,
    mother_schooling = recode(hfmother_school, !!! mother_school_recode),
    typhoid_vax = if_else(receivevacc == 1, "Yes", "No")
  )

water_to_bind <-
water_samples %>%
  select(waterid, treat_drink_water)

#view(water_samples)

list_variables <-
  c(
    "age", "hfhhsize", "toilet", "toilet_share", "drinking_water", "treat_drink_water", "monthly_family_income",
    "mother_schooling", "typhoid_vax"
  )

list_catvar <-
  c(
    "toilet", "toilet_share", "drinking_water", "treat_drink_water",
    "mother_schooling", "typhoid_vax"
  )



tableOne <-
  CreateTableOne(
    data = water_samples,
    vars = list_variables,
    strata = "water_sampled"
  )

tab_1_result <- print(tableOne, formatOptions = list(big.mark = ","))

write.csv(tab_1_result, "data/table_one.csv")


glimpse(water_samples)




