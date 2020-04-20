# This file cleans the data on typhoid cases and controls from redcap to only
#use the nepal data

# Author: Chris LeBoa
# Version: 2020-02-23

# Libraries
library(tidyverse)

# Parameters
file_redcap_data_all_raw <- "/Users/ChrisLeBoa/GitHub/nepal_typhoid/data-raw/cases/all_redcap_data/sees_data_all_20200223.csv"
file_redcap_data_nepal <- "/Users/ChrisLeBoa/GitHub/nepal_typhoid/data/cases/redcap/redcap_data_nepal.rds"

#===============================================================================

#Code

redcap_data <- read_csv(file_redcap_data_all_raw)

#Want just cases from nepal and the first followup for cases and controls
redcap_data_nepal <-
  redcap_data %>%
  filter(redcap_data_access_group == "nepal_site") %>%
  select(
    index_id, redcap_event_name, dbslon, dbslat, hfcoordinate, rc_bldculres, sex,
    age_final, hftoilet, hftoiletshare, hftoiletshare_other, hfdrwater,
    hfdrwateroth, hfdrwater_municipal, hfdrwater_municipal_other, hf_water_access,
    hf_water_enter_home, hfwaterstore, hftreatdkwater, hf_famincome_nepal,
    waterid, waterid_manual, watercolpt, dbs_eduindex, dbs_edulevel, receivevacc
  ) %>%
  write_rds(file_redcap_data_nepal)

#summary(redcap_data_nepal)
