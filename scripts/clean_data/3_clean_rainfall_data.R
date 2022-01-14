# This script imports the nepal rainfall data and outputs a monthly rainfall
# measure for the month
# Author: Chris Leboa
# Version: 2021-07-27

# Libraries
library(tidyverse)

station_interest <- c("Dhulikhel", "Kathmandu Airport")

# Parameters

nepal_rainfall_month <-
  nepal_rainfall_data %>%
  filter(station %in% station_interest) %>%
  unite(date_format, year:days, sep = "-", remove = FALSE) %>%
  mutate(
    date = as_date(date_format),
    year = as_factor(year)) %>%
  group_by(year, month, station) %>%
  summarise(rain_month = sum(rainfall_sum, na.rm = TRUE))
nepal_rainfall_month


