# This script converts the data from survey 123 to a form that can be understood
#by Redcap

# Author: Chris LeBoa
# Version: 2020-02-12

# Libraries
library(tidyverse)

# Parameters
file_survey123_data <-
  "/Users/ChrisLeBoa/GitHub/nepal_typhoid/data-raw/redcap_data/survey123_data_nepal.csv"



#===============================================================================

#Code

survey_123 <-
  read_csv(file_survey123_data)

glimpse(survey_123)
