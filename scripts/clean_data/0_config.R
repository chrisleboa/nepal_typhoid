# This file configures all packages you will need to run the Nepal ES data analysis


# Libraries
library(tidyverse)
library(RCurl)
library(lubridate)
library("ggsci")
library("gridExtra")
library(googlesheets4)


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
#For River Positivity Analysis
river_nepal_data <- postForm(
  uri='https://redcap.stanford.edu/api/',
  token='E7E4D521E860D283CC17037378399BA0',
  content='report',
  format='csv',
  report_id='74088',
  csvDelimiter='',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  returnFormat='csv'
)

sabin_data_location <- here::here("data/cases/sabin data/Combined_10June2021.csv")

#Hydrology data
hydrology_data_location <- here::here("data/water_samples/EStyphi_KTM_generalresults.xlsx")

#weather data
weather_data <- here::here("data-raw/Nepal Weather/rainfall.csv")

#Negative Control Data

neg_data_location <- "1ZnL6NckAKeuZgyYXbkz_PPYCXI2e5B2ANUWocHRXVSA"


#read in data
nepal_sees_data <- read_csv(sabin_data_location)
nepal_hydrology_data <- readxl::read_excel(hydrology_data_location) %>%
  select(Sample_ID, escatch.upstrmpop) %>%
  mutate(sample_id = str_replace(Sample_ID, "_", "-"))
nepal_river_data <- read_csv(river_nepal_data)
nepal_rainfall_data <- read_csv(weather_data)
neg_data <- read_sheet(neg_data_location, na = c("UD","","NULL" ))



#Parameters
pcr_threshold <- 35
#===============================================================================



