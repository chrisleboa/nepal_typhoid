# Isocline changes between positive and negaitve samples.
#Was originally intentioned as a figure for the supplement of the text to show viability but was removed upon co-author feedback.

# Author: Chris LeBoa
# Version: 2023-07-25

# Libraries
library(tidyverse)

# Parameters
river_cleaned_data_location <- "/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/rivers/rivers_formatted.csv" #This file is generated in the scripts <- clean_data folder <- 2_clean_riversample_data

nepal_river_data_formatted <- read_csv(river_cleaned_data_location)

#The neg data file can be inputted by running the 0_config.R file
#===============================================================================

#Code
## Looking at the change shift

## Creating negative control dataset to match samples
# To to T16 changes of positivity

# Plottting isoclines of positivity
nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  mutate(
    typhi_pos_num = if_else(river_pcr_typhi_1_t16_ct <= 35 | river_pcr_typhi_2_t16_ct <= 35, 1, 0),
    paratyphi_pos_num = if_else(river_pcr_paratyphi_1_t16_ct <= 35 | river_pcr_paratyphi_2_t16_ct <= 35, 1, 0),
    typhi_delta_pos_num =
      if_else(
        (river_pcr_typhi_1_t16_ct <= 35 & river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct >= 3) |
          (river_pcr_typhi_2_t16_ct <= 35 & river_pcr_typhi_2_t0_ct - river_pcr_typhi_2_t16_ct >= 3), 1, 0),
    paratyphi_delta_pos_num =
      if_else(
        (river_pcr_paratyphi_1_t16_ct <= 35 & river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct >= 3) |
          (river_pcr_paratyphi_2_t16_ct <= 35 & river_pcr_paratyphi_2_t0_ct - river_pcr_paratyphi_2_t16_ct >= 3), 1, 0)
  ) %>%
  filter(paratyphi_delta_pos_num == 1, river_pcr_paratyphi_1_t0_ct < 99, river_pcr_paratyphi_1_t16_ct < 35) %>%
  ggplot(aes(river_pcr_paratyphi_1_t16_ct, river_pcr_paratyphi_1_t0_ct, color = paratyphi_pos)) +
  geom_point() +
  geom_hline(yintercept = 35) +
  geom_vline(xintercept = 35) +
  theme_bw()

nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  mutate(
    typhi_pos_num = if_else(river_pcr_typhi_1_t16_ct <= 35 | river_pcr_typhi_2_t16_ct <= 35, 1, 0),
    paratyphi_pos_num = if_else(river_pcr_paratyphi_1_t16_ct <= 35 | river_pcr_paratyphi_2_t16_ct <= 35, 1, 0),
    typhi_delta_pos_num =
      if_else(
        (river_pcr_typhi_1_t16_ct <= 35 & river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct >= 3) |
          (river_pcr_typhi_2_t16_ct <= 35 & river_pcr_typhi_2_t0_ct - river_pcr_typhi_2_t16_ct >= 3), 1, 0),
    paratyphi_delta_pos_num =
      if_else(
        (river_pcr_paratyphi_1_t16_ct <= 35 & river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct >= 3) |
          (river_pcr_paratyphi_2_t16_ct <= 35 & river_pcr_paratyphi_2_t0_ct - river_pcr_paratyphi_2_t16_ct >= 3), 1, 0)
  ) %>%
  filter(typhi_delta_pos_num == 1, river_pcr_typhi_1_t0_ct < 99, river_pcr_typhi_1_t16_ct < 35) %>%
  ggplot(aes(river_pcr_typhi_1_t16_ct, river_pcr_typhi_1_t0_ct, color = typhi_pos)) +
  geom_point() +
  geom_hline(yintercept = 35) +
  geom_vline(xintercept = 35) +
  theme_bw()


neg_data2 <-
  neg_data %>%
  filter(!is.na(Sample_ID)) %>%
  fill(Date) %>%
  unnest(Date) %>%
  mutate(
    date = as_date(Date),
    char = as.character(date),
    samp_id = str_glue("{char}_{Sample_ID}")
  ) %>%
  pivot_wider(names_from = time, values_from = c(st_ct1, st_ct2, spt_ct1, spt_ct2)) %>%
  filter(!is.na(st_ct1_0) &  !is.na(st_ct1_16) |
           !is.na(st_ct2_0) &  !is.na(st_ct2_16) |
           !is.na(spt_ct1_0) &  !is.na(spt_ct1_16) |
           !is.na(spt_ct2_0) &  !is.na(spt_ct2_16)
  ) %>%
  mutate(
    neg_delta_pos_sum = if_else(
      !is.na(st_ct1_0) &  !is.na(st_ct1_16) == TRUE,
      st_ct1_0 - st_ct1_16,  st_ct2_0 - st_ct2_16))



change_typhi <-
  nepal_river_data_formatted %>%
  filter(
    is.na(redcap_repeat_instance) | redcap_repeat_instance == 1,
    river_pcr_typhi_1_t0_ct < 99,
    river_pcr_typhi_1_t16_ct < 99,
  ) %>%
  mutate(
    delta_typhi = river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct,
    delta = river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct,
    delta_paratyphi = river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_2_t16_ct,
    typhi_delta_pos_num =
      if_else(
        (river_pcr_typhi_1_t16_ct <= 35 & river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct >= 3) |
          (river_pcr_typhi_2_t16_ct <= 35 & river_pcr_typhi_2_t0_ct - river_pcr_typhi_2_t16_ct >= 3), 1, 0),
    paratyphi_delta_pos_num =
      if_else(
        (river_pcr_paratyphi_1_t16_ct <= 35 & river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct >= 3) |
          (river_pcr_paratyphi_2_t16_ct <= 35 & river_pcr_paratyphi_2_t0_ct - river_pcr_paratyphi_2_t16_ct >= 3), 1, 0)
  )




change_paratyphi <-
  nepal_river_data_formatted %>%
  filter(
    is.na(redcap_repeat_instance) | redcap_repeat_instance == 1,
    river_pcr_paratyphi_1_t0_ct < 99,
    river_pcr_paratyphi_1_t16_ct < 99,
  ) %>%
  mutate(
    delta_typhi = river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct,
    delta_paratyphi = river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct,
    delta = river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct,
    typhi_delta_pos_num =
      if_else(
        (river_pcr_typhi_1_t16_ct <= 35 & river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct >= 3) |
          (river_pcr_typhi_2_t16_ct <= 35 & river_pcr_typhi_2_t0_ct - river_pcr_typhi_2_t16_ct >= 3), 1, 0),
    paratyphi_delta_pos_num =
      if_else(
        (river_pcr_paratyphi_1_t16_ct <= 35 & river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct >= 3) |
          (river_pcr_paratyphi_2_t16_ct <= 35 & river_pcr_paratyphi_2_t0_ct - river_pcr_paratyphi_2_t16_ct >= 3), 1, 0)
  )

change_total <-
  change_typhi %>%select(delta) %>%
  full_join(change_paratyphi %>% select(delta))

combined_data <-
  data.frame(
    delta_ct = c(
      #change_typhi$delta_typhi,
      #change_paratyphi$delta_paratyphi,
      change_total$delta,
      neg_data2$neg_delta_pos_sum
    ),
    status = c(
      # rep("typhi (n = 68)", dim(change_typhi)[1]),
      # rep("paratyphi (n = 49)", dim(change_paratyphi)[1]),
      rep("Contaminated Sample (n = 119)", dim(change_total)[1]),
      rep("Negative Control (n = 13)", dim(neg_data2)[1])
    )
  )

combined_data %>%
  count(status)

combined_data %>%
  ggplot() +
  geom_density(aes(x = delta_ct, fill = status), alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values=c('blue', 'green', "red")) +
  theme(legend.position = "top") +
  labs(
    title = "Average change in Ct between time 0 and time 16 extractions",
    x = "CT change T0 to T16",
    y = "Proportion of samples",
    fill = ""
  )


## Looking at the change shift between negative controls and positive samples

nepal_river_data_formatted %>%
  filter(
    is.na(redcap_repeat_instance) | redcap_repeat_instance == 1,
    river_pcr_paratyphi_1_t0_ct < 99,
    river_pcr_paratyphi_1_t16_ct < 99,
  ) %>%
  mutate(
    delta_typhi = river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct,
    delta_paratyphi = river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct,
    typhi_delta_pos_num =
      if_else(
        (river_pcr_typhi_1_t16_ct <= 35 & river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct >= 3) |
          (river_pcr_typhi_2_t16_ct <= 35 & river_pcr_typhi_2_t0_ct - river_pcr_typhi_2_t16_ct >= 3), 1, 0),
    paratyphi_delta_pos_num =
      if_else(
        (river_pcr_paratyphi_1_t16_ct <= 35 & river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct >= 3) |
          (river_pcr_paratyphi_2_t16_ct <= 35 & river_pcr_paratyphi_2_t0_ct - river_pcr_paratyphi_2_t16_ct >= 3), 1, 0)
  ) %>%
  # filter(delta_paratyphi < 0) %>%
  # select(sample_id, delta_paratyphi, river_pcr_paratyphi_1_t16_ct, river_pcr_paratyphi_1_t0_ct )
  filter( ) %>%
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_histogram(aes(delta_paratyphi), fill = "blue", binwidth = 1.5) +
  #geom_histogram(aes(delta_paratyphi), fill = "blue", alpha = .5) +
  labs(
    title = "River water samples change in CT",
    x = "S.Paratyphi CT change",
    y = "Number of samples"
  )

# To to T16 changes of positivity

# Plottting isoclines of positivity
nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  mutate(
    typhi_pos_num = if_else(river_pcr_typhi_1_t16_ct <= 35 | river_pcr_typhi_2_t16_ct <= 35, 1, 0),
    paratyphi_pos_num = if_else(river_pcr_paratyphi_1_t16_ct <= 35 | river_pcr_paratyphi_2_t16_ct <= 35, 1, 0),
    typhi_delta_pos_num =
      if_else(
        (river_pcr_typhi_1_t16_ct <= 35 & river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct >= 3) |
          (river_pcr_typhi_2_t16_ct <= 35 & river_pcr_typhi_2_t0_ct - river_pcr_typhi_2_t16_ct >= 3), 1, 0),
    paratyphi_delta_pos_num =
      if_else(
        (river_pcr_paratyphi_1_t16_ct <= 35 & river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct >= 3) |
          (river_pcr_paratyphi_2_t16_ct <= 35 & river_pcr_paratyphi_2_t0_ct - river_pcr_paratyphi_2_t16_ct >= 3), 1, 0)
  ) %>%
  filter(paratyphi_delta_pos_num == 1, river_pcr_paratyphi_1_t0_ct < 99, river_pcr_paratyphi_1_t16_ct < 35) %>%
  ggplot(aes(river_pcr_paratyphi_1_t16_ct, river_pcr_paratyphi_1_t0_ct, color = paratyphi_pos)) +
  geom_point() +
  geom_hline(yintercept = 35) +
  geom_vline(xintercept = 35) +
  theme_bw()

nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  mutate(
    typhi_pos_num = if_else(river_pcr_typhi_1_t16_ct <= 35 | river_pcr_typhi_2_t16_ct <= 35, 1, 0),
    paratyphi_pos_num = if_else(river_pcr_paratyphi_1_t16_ct <= 35 | river_pcr_paratyphi_2_t16_ct <= 35, 1, 0),
    typhi_delta_pos_num =
      if_else(
        (river_pcr_typhi_1_t16_ct <= 35 & river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct >= 3) |
          (river_pcr_typhi_2_t16_ct <= 35 & river_pcr_typhi_2_t0_ct - river_pcr_typhi_2_t16_ct >= 3), 1, 0),
    paratyphi_delta_pos_num =
      if_else(
        (river_pcr_paratyphi_1_t16_ct <= 35 & river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct >= 3) |
          (river_pcr_paratyphi_2_t16_ct <= 35 & river_pcr_paratyphi_2_t0_ct - river_pcr_paratyphi_2_t16_ct >= 3), 1, 0)
  ) %>%
  filter(typhi_delta_pos_num == 1, river_pcr_typhi_1_t0_ct < 99, river_pcr_typhi_1_t16_ct < 35) %>%
  ggplot(aes(river_pcr_typhi_1_t16_ct, river_pcr_typhi_1_t0_ct, color = typhi_pos)) +
  geom_point() +
  geom_hline(yintercept = 35) +
  geom_vline(xintercept = 35) +
  theme_bw()

