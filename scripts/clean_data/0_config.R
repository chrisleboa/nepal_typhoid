# This file configures all packages you will need to run the Nepal ES data analysis

#install.packages("gridExtra")

# Libraries
library(tidyverse)
library(RCurl)
library(lubridate)
library("ggsci")
library("gridExtra")
library(googlesheets4)
library(mgcv)

# Redcap API

#For drinking water dates
sees_nepal_data <- postForm(
  uri='https://redcap.stanford.edu/api/',
  token='1F2BCB605944BE0455E5CEA4FB4DE65E',
  content='report',
  format='csv',
  report_id='57256',
  csvDelimiter='',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  returnFormat='csv'
)
#Drinking water lab results
sees_dw_lab <-"/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/water_samples/Nepal_PCR_DrinkingWater_Redcap_Upload.csv"
  #"/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/water_samples/drinking_water_lab_results.csv"
#For River Positivity Analysis
river_nepal_data <- here::here("data-raw/NepalEnvironmentalSu_DATA_2023-04-21_1344.csv")

sabin_data_location <- here::here("data/cases/sabin data/Combined_10June2021.csv")
#Hydrology data
hydrology_data_location <- here::here("data/water_samples/EStyphi_KTM_generalresults.xlsx")

#weather data
weather_data <- here::here("data-raw/Nepal Weather/rainfall.csv")

#Negative Control Data

neg_data_location <- "1ZnL6NckAKeuZgyYXbkz_PPYCXI2e5B2ANUWocHRXVSA"


#read in data
nepal_sees_data <- read_csv(sabin_data_location)
nepal_drinking_water_data <- read_csv(
  postForm(
  uri='https://redcap.stanford.edu/api/',
  token='1F2BCB605944BE0455E5CEA4FB4DE65E',
  content='report',
  format='csv',
  report_id='57256',
  csvDelimiter='',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  returnFormat='csv'
))

nepal_dw_lab_data <- read_csv(sees_dw_lab) #the lab data from drinking water samples

nepal_hydrology_data <- readxl::read_excel(hydrology_data_location) %>%
  select(Sample_ID, escatch.upstrmpop) %>%
  mutate(sample_id = str_replace(Sample_ID, "_", "-"))

nepal_river_data <- read_csv(river_nepal_data)
nepal_rainfall_data <- read_csv(weather_data)
neg_data <- read_sheet(neg_data_location, na = c("UD","","NULL" ))

write_csv(nepal_river_data, here::here("data/water_samples/EStyphi_KTM_river_results.xlsx"))


#Parameters
pcr_threshold <- 35
#===============================================================================



