# River water Positivity tables. For a conference on typhoid we wanted to create summary tables on
# total sample positivity

# Author: Chris LeBoa
# Version: 2021-07-13

# Libraries
library(tidyverse)
library(irr)
library(lubridate)
# Parameters


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
  #group_by(sample_id) %>%
  summarise(
    typhi_phage_pos = sum(river_phage_typhi, na.rm = TRUE),
    paratyphi_phage_pos = sum(river_phage_paratyphi, na.rm = TRUE),
    typhi_pos = sum(typhi_pos_num),
    paratyphi_pos = sum(paratyphi_pos_num, na.rm = TRUE),
    typhi_total = sum(!is.na(typhi_pos_num)),
    pt_total = sum(!is.na(paratyphi_pos_num)),
    typhi_35_pct = typhi_pos / typhi_total * 100,
    pt_35_pct = paratyphi_pos / pt_total *100,
    typhi_pos_delta = sum(typhi_delta_pos_num, na.rm = TRUE),
    paratyphi_pos_delta = sum(paratyphi_delta_pos_num, na.rm = TRUE),
    typhi_delta_total = sum(!is.na(typhi_delta_pos_num)),
    pt_delta_total = sum(!is.na(paratyphi_delta_pos_num )),
    typhi_delta_pct = typhi_pos_delta / typhi_delta_total * 100,
    pt_delta_pct = paratyphi_pos_delta / pt_delta_total *100
  ) %>%
  view()

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
  group_by(sample_id) %>%
  summarise(
    typhi_phage_pos = sum(river_phage_typhi, na.rm = TRUE),
    paratyphi_phage_pos = sum(river_phage_paratyphi, na.rm = TRUE),
    typhi_pos = sum(typhi_pos_num),
    paratyphi_pos = sum(paratyphi_pos_num, na.rm = TRUE),
    typhi_total = sum(!is.na(typhi_pos_num)),
    pt_total = sum(!is.na(paratyphi_pos_num)),
    typhi_35_pct = typhi_pos / typhi_total * 100,
    pt_35_pct = paratyphi_pos / pt_total *100,
    typhi_pos_delta = sum(typhi_delta_pos_num, na.rm = TRUE),
    paratyphi_pos_delta = sum(paratyphi_delta_pos_num, na.rm = TRUE),
    typhi_delta_total = sum(!is.na(typhi_delta_pos_num)),
    pt_delta_total = sum(!is.na(paratyphi_delta_pos_num )),
    typhi_delta_pct = typhi_pos_delta / typhi_delta_total * 100,
    pt_delta_pct = paratyphi_pos_delta / pt_delta_total *100
  )

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
  select(river_phage_paratyphi, paratyphi_pos_num) %>%
  kappa2()



## Upstream Positive locations
nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  left_join(nepal_hydrology_data, by = "sample_id") %>%
  #drop_na(paratyphi_pos) %>%
  filter(str_detect(sample_id, "BAG-3|DBK-3|MAN-3|BIS-3")) %>%
  mutate(
    typhi_pos_num = if_else(river_pcr_typhi_1_t16_ct <= 35 | river_pcr_typhi_2_t16_ct <= 35, 1, 0),
    paratyphi_pos_num = if_else(river_pcr_paratyphi_1_t16_ct <= 35 | river_pcr_paratyphi_2_t16_ct <= 35, 1, 0)
  ) %>%
  summarise(
    typhi_pos = sum(typhi_pos_num),
    paratyphi_pos = sum(paratyphi_pos_num, na.rm = TRUE),
    typhi_total = sum(!is.na(typhi_pos_num)),
    pt_total = sum(!is.na(paratyphi_pos_num)),
    typhi_35_pct = typhi_pos / typhi_total * 100,
    pt_35_pct = paratyphi_pos / pt_total *100
  )

## All the way downstream location
nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  left_join(nepal_hydrology_data, by = "sample_id") %>%
 # drop_na(paratyphi_pos) %>%
  filter(str_detect(sample_id, "COM-3")) %>%
  mutate(
    typhi_pos_num = if_else(river_pcr_typhi_1_t16_ct <= 35 | river_pcr_typhi_2_t16_ct <= 35, 1, 0),
    paratyphi_pos_num = if_else(river_pcr_paratyphi_1_t16_ct <= 35 | river_pcr_paratyphi_2_t16_ct <= 35, 1, 0)
  ) %>%
  summarise(
    typhi_pos = sum(typhi_pos_num),
    paratyphi_pos = sum(paratyphi_pos_num, na.rm = TRUE),
    typhi_total = sum(!is.na(typhi_pos_num)),
    pt_total = sum(!is.na(paratyphi_pos_num)),
    typhi_35_pct = typhi_pos / typhi_total * 100,
    pt_35_pct = paratyphi_pos / pt_total *100
  )

## Middle of city positives
nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  left_join(nepal_hydrology_data, by = "sample_id") %>%
  #drop_na(paratyphi_pos) %>%
  filter(str_detect(sample_id, "BAG-1|BIS-1|MAN-1|COM-0|COM-1")) %>%
  mutate(
    typhi_pos_num = if_else(river_pcr_typhi_1_t16_ct <= 35 | river_pcr_typhi_2_t16_ct <= 35, 1, 0),
    paratyphi_pos_num = if_else(river_pcr_paratyphi_1_t16_ct <= 35 | river_pcr_paratyphi_2_t16_ct <= 35, 1, 0)
  ) %>%
  summarise(
    typhi_pos = sum(typhi_pos_num),
    paratyphi_pos = sum(paratyphi_pos_num, na.rm = TRUE),
    typhi_total = sum(!is.na(typhi_pos_num)),
    pt_total = sum(!is.na(paratyphi_pos_num)),
    typhi_35_pct = typhi_pos / typhi_total * 100,
    pt_35_pct = paratyphi_pos / pt_total *100
  )


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

## Looking at the change shift

## Creating negative control dataset to match samples

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
    typhi_delta_pos_num =
      if_else(
        (river_pcr_typhi_1_t16_ct <= 35 & river_pcr_typhi_1_t0_ct - river_pcr_typhi_1_t16_ct >= 3) |
          (river_pcr_typhi_2_t16_ct <= 35 & river_pcr_typhi_2_t0_ct - river_pcr_typhi_2_t16_ct >= 3), 1, 0),
    paratyphi_delta_pos_num =
      if_else(
        (river_pcr_paratyphi_1_t16_ct <= 35 & river_pcr_paratyphi_1_t0_ct - river_pcr_paratyphi_1_t16_ct >= 3) |
          (river_pcr_paratyphi_2_t16_ct <= 35 & river_pcr_paratyphi_2_t0_ct - river_pcr_paratyphi_2_t16_ct >= 3), 1, 0)
  )

combined_data <-
  data.frame(
    delta_ct = c(
      change_typhi$delta_typhi,
      change_paratyphi$delta_paratyphi,
      neg_data2$neg_delta_pos_sum
      ),
    status = c(
      rep("typhi (n = 68)", dim(change_typhi)[1]),
      rep("paratyphi (n = 49)", dim(change_paratyphi)[1]),
      rep("neg_control (n = 13)", dim(neg_data2)[1])
      )
    )

combined_data %>%
  count(status)

combined_data %>%
  ggplot() +
  geom_density(aes(x = delta_ct, fill = status), alpha = 0.5) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values=c('blue', 'green', "red")) +
  labs(
    title = "River water samples change in CT",
    x = "CT change T0 to T16",
    y = "Proportion of samples"
  )


## Looking at the change shift

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


view(nepal_river_data_formatted)


