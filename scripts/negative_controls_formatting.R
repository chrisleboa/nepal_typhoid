# Negative control reformatting

# Author: Christopher LeBoa
# Version: 2021-02-11

# Libraries
library(tidyverse)
library(lubridate)


# Parameters

file_location <- here::here("data/negative_controls/ES-data.xlsx")
file_output_location <- here::here("data/negative_controls/formatted-ES-data.csv")

data <-
  readxl::read_excel(file_location, na = "UD", col_types = c("text", "text", "date", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "text")) %>%
  filter(!is.na(collection_point)) %>%
  fill(Date, .direction = "down") %>%
  mutate(
    month = month(Date),
    year = year(Date),
    date = ymd(Date),
    ID = str_glue("{collection_point}-{year}-{month}")
  ) %>%
  select(-c(month, year, Date))

data %>%
  write_csv(file_output_location)

