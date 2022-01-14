# Phages Graph

# Author: Christopher LeBoa
# Version: 2021-07-13

# Libraries
library(tidyverse)

file_in <-"/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/phages/S.PTyphi Viability Master.xlsx"

data_pt <- readxl::read_excel(file_in, sheet = 1)

data_t <- readxl::read_excel(file_in, sheet = 2)

graph_fxn <- function(dataset, title){
  dataset %>%
    select(Sample, Broth, starts_with("Day"), -ends_with(c("av", "Avg"))) %>%
    mutate_if(is.logical, as.numeric) %>%
    pivot_longer(cols = starts_with("Day"), names_to = "day", names_pattern = "(\\d+)", values_to = "phages") %>%
    ggplot(aes(x = as.numeric(day), y = phages, color = as.factor(Sample))) +
    geom_point() +
    geom_smooth(method = "gam") +
    facet_wrap(vars(Broth), nrow = 2) +
    theme_bw() +
    theme(legend.position = "top") +
    labs(
      title = title,
      x = "Days",
      y = "Number of organisms",
      color = "Sample:"
      )
}

graph_fxn(data_pt, title = "S. Paratyphi Count over time")
graph_fxn(data_t, title = "S. Typhi count over time")

# data %>%
#   str_remove_all(" Contam") %>%
#   head()  #This code was messing me up so i just removed the two contaminated pieces manually


# Parameters

#===============================================================================

Code
