# This script converts the data from survey 123 to a form that can be understood
#by Redcap

# Author: Chris LeBoa
# Version: 2020-02-12

# Libraries
library(tidyverse)
library(lubridate)

# Parameters
file_survey123_data <-
  "/Users/ChrisLeBoa/GitHub/nepal_typhoid/data-raw/survey123_data/survey123_data_nepal.csv"
file_output_location
# Collector Names
name <- c("Sneha", "Jivan", "Chris", "Mudita", "Other")



# recode_collector <- function(name){
#   as.logical(
#     map_chr(
#       c(`First Collector Name`, `Second Collector Name`),
#       ~str_detect(., "^{{name}}")
#     ))
# }

rainfall_recode <-
  c(
    "today" = 1,
    "yesterday" = 2,
    "this_week" = 3,
    "sometime_before_this_week" = 4,
    "other" = 5
  )

yn_recode <-
  c(
    "yes" = 1,
    "no" = 2,
    "not sure" = 98,
    "not_sure" = 98
  )

evidence_recode <-
  c(
    "observed" = 1,
    "someone_told_us" = 2
  )

little_lot_recode <-
  c(
    "a_little" = 1,
    "a_medium_amount" = 2,
    "a_lot" = 3
  )

roaming_recode <-
  c(
    "In enclosure" = 1,
    "Roaming free" = 2,
    "Both" = 3
  )
#===============================================================================

#Code

survey_123 <-
  read_csv(file_survey123_data)

glimpse(survey_123)

data_from_survey123 <-
  survey_123 %>%
  mutate(
    river_datetime = mdy_hm(`Collection Time`)
  ) %>%
  transmute(
    #`Sample ID`,
    sample_id = str_extract(`Sample ID`, "^\\w+\\-\\d") %>% str_to_upper(.),
    redcap_event_name =
      str_c(
        as.character(year(river_datetime)),
        "_",
        as.character(month(river_datetime)),
        "_",
        case_when(
          month(river_datetime) == 11 & day(river_datetime) < 25  ~ "1a_",
          month(river_datetime) == 11 & day(river_datetime) >= 25 ~ "1b_",
                                                             TRUE ~   ""
          ),
        "arm_1",
        sep = ""
      ),
    river_datetime =
      str_c(
      as.character(month(river_datetime)),
      "/",
      as.character(day(river_datetime)),
      "/",
      as.character(year(river_datetime)),
      " ",
      as.character(hour(river_datetime)),
      ":",
      "30",
      sep = ""
    ),
    #  str_replace_all(as.character(date(river_datetime)), "-", "/"),
    # river_collector_names_1 =
    #   if_else(recode_collector(name[1]), 1, 0),
    river_samplenumber = `Sample Number`,
    river_temp = `Temperature`,
    river_ph = `pH of water sample`,
    river_orp = `Oxidation-Reduction Profile`,
    river_rainfall =
      recode(
        `When is the last time it rained at the area you are collecting water?`,
        !!! rainfall_recode),
    river_bathing = recode(`Is this area used for bathing?`, !!! yn_recode),
    river_wash_clothes =
      recode(`Is this area used for washing clothes?`, !!! yn_recode),
    river_wash_veg =
      recode(`Is this area used for washing vegetables`, !!! yn_recode),
    river_drink_water =
      recode(`Do people drink the river water from this site?`, !!! yn_recode),
    river_walk_across =
      recode(`Do people walk across the river here?`, !!! yn_recode),
    river_trash =
      recode(`Is there trash around this site?`, !!! yn_recode),
    river_trash_amount =
      recode(`How much trash is around this site?`, !!! little_lot_recode),
    river_feces =
      recode(
        `Do you see any open defecation or used daipers at this site?`,
        !!! yn_recode
      ),
    river_feces_describe = `Describe the open defecation`,
    river_sewer =
      recode(
        `Do you see any agricultural fields near this site?`,
        !!! yn_recode
      ),
    river_sewer_n = as.double(`How many sewers do you observe?`),
    river_sewer_draining =
          `How many sewage pipes are actively draining into the river at this time?`,
    river_animals =
      recode(
        `Do you see any animals around this site?`,
        !!! yn_recode
      ),
    river_animals_enclosed =
      recode(
        `Are they in an enclosure or roaming free?`,
        !!! roaming_recode
      ),
    river_animals_cow = `How many cows do you see?`,
    river_animals_dog = `How many dogs do you see?`,
    river_animals_cat = `How many cats do you see?`,
    river_animals_chicken = `How many chicken do you see?`,
    river_animals_goat = `How many goats  do you see?`,
    river_animals_other = `How many other animals do you see?`,
    river_agriculture =
      recode(
        `Do you see any agricultural fields near this site?`,
        !!! yn_recode
      ),
    river_latitude = y,
    river_longitude = x,
    river_water_field_sampling_metadata_complete = 2
  )

head(data_from_survey123)

data_from_survey123 %>%
  mutate_if(is.numeric, funs(replace_na(., " "))) %>%
  write_csv(
    "/Users/ChrisLeBoa/GitHub/nepal_typhoid/data/water_samples/water_samples_forRedcap.csv"
    )
