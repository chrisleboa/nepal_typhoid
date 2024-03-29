# Figure 1: Positivity in relation to stream location
  # Samples were chosen at distances 1,5 and 10 km from the river confluence points
  # This plot shows the % of time locations were positive at these different distances up and down the streams

  #Punymata points have been excluded from this plot since they are on a different river system

# Author: Chris LeBoa
# Version: 2021-05-31





## I tried to use a gam on this and it said there were not enough nodes to do so
nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  left_join(nepal_hydrology_data, by = "sample_id") %>%
  drop_na(paratyphi_pos) %>%
  pivot_longer(cols = c(typhi_pos, paratyphi_pos), names_to = "organism", values_to = "dna") %>%
  group_by(stream_location, organism) %>%
  summarise(
    dna_pos = sum(dna == "Pos"),
    num_samples = n(),
    percent_pos = dna_pos / num_samples * 100
  ) %>%
  mutate(
    # sample_date = parse_date(redcap_event_name, "%Y %m %d"),
    # sample_date = if_else(is.na(sample_date), parse_date(redcap_event_name, "%Y %m"), sample_date),
    organism = str_remove(organism, "_pos")
  ) %>%
  ggplot(aes(stream_location, percent_pos, fill = organism, color = organism)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "loess") +
  scale_x_continuous() +
  scale_fill_nejm() +
  scale_color_nejm() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(
    title = "DNA contamination by Location",
    y = "Percent of Samples Contaminated (%)",
    x = "People Living upstream of Sampling Point"
  )

##Setting up gam model to do the same thing as above

gam_data <- nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  left_join(nepal_hydrology_data, by = "sample_id") %>%
  mutate(typhi_pos_num = if_else(typhi_pos == "Pos", 1, 0))

gam_data %>%
  select(sample_id, redcap_event_name, stream_location, typhi_pos_num) %>%
  write_csv(., "supp_figure_data.csv")

gam_model = gam(typhi_pos_num ~ s(stream_location, k = 3), data = gam_data, family = "binomial")

#run this as a logistic model where data is all in rows or where data is
# wonder if there is a fixed or random effect for location as a fixed, random effect on that
# The k fixes the numver of knobs / how deep the polynomial is to avoid overfitting

plot_gam(gam_model, main_var = stream_location) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Percentage S.Typhi DNA positivity \nby Distance from Confluence",
    y = "Samples contaminated with S.Typhi DNA (%)",
    x = "Downstream Distance from river confluence (km)")

plot_gam_check(gam_model)

summary(gam_model)

predict.gam(gam_model)




# Previously had worked on the same plots but inputted the number of people living upstream from each location
# using a hydrology measure. We have retained this code but now are presenting the data

nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  left_join(nepal_hydrology_data, by = "sample_id") %>%
  drop_na(paratyphi_pos) %>%
  pivot_longer(cols = c(typhi_pos, paratyphi_pos), names_to = "organism", values_to = "dna") %>%
  group_by(escatch.upstrmpop, organism) %>%
  summarise(
    dna_pos = sum(dna == "Pos"),
    num_samples = n(),
    percent_pos = dna_pos / num_samples * 100
    ) %>%
 mutate(
    # sample_date = parse_date(redcap_event_name, "%Y %m %d"),
    # sample_date = if_else(is.na(sample_date), parse_date(redcap_event_name, "%Y %m"), sample_date),
    organism = str_remove(organism, "_pos")
  ) %>%
  ggplot(aes(escatch.upstrmpop, percent_pos, fill = organism, color = organism)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "gam") +
  scale_x_continuous() +
  scale_fill_nejm() +
  scale_color_nejm() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(
    title = "DNA contamination by Location",
    y = "Percent of Samples Contaminated (%)",
    x = "People Living upstream of Sampling Point"
  )

# install.packages("devtools")
#devtools::install_github("m-clark/visibly")

library(visibly)
library(mgcv)

gam_data <- nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  left_join(nepal_hydrology_data, by = "sample_id") %>%
  mutate(typhi_pos_num = if_else(typhi_pos == "Pos", 1, 0))
  # drop_na(typhi_pos) %>%
  # pivot_longer(cols = c(typhi_pos, paratyphi_pos), names_to = "organism", values_to = "dna") %>%
  # group_by(escatch.upstrmpop, organism) %>%
  # summarise(
  #   dna_pos = sum(dna == "Pos"),
  #   num_samples = n(),
  #   percent_pos = dna_pos / num_samples * 100
  # ) %>%
  # filter(organism == "typhi_pos")

gam_model = gam(typhi_pos_num ~ s(escatch.upstrmpop, k = 3), data = gam_data, family = "binomial")

#run this as a logistic model where data is all in rows or where data is
   # wonder if there is a fixed or random effect for location as a fixed, random effect on that
   # The k fixes the numver of knobs / how deep the polynomial is to avoid overfitting

plot_gam(gam_model, main_var = escatch.upstrmpop) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  xlim(c(0,3500000)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Percentage S.Typhi DNA positivity \nby upstream population",
    y = "Samples contaminated with S.Typhi DNA%",
    x = "Number of people upstream")

plot_gam_check(gam_model)

summary(gam_model)

predict.gam(gam_model)

### Doing same gam with phages

nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  left_join(nepal_hydrology_data, by = "sample_id") %>%
  drop_na(paratyphi_pos) %>%
  pivot_longer(cols = c(river_phage_typhi, river_phage_paratyphi), names_to = "organism", values_to = "phage") %>%
  group_by(escatch.upstrmpop, organism) %>%
  summarise(
    dna_pos = sum(phage == 1),
    num_samples = n(),
    percent_pos = dna_pos / num_samples * 100
  ) %>%
  mutate(
    # sample_date = parse_date(redcap_event_name, "%Y %m %d"),
    # sample_date = if_else(is.na(sample_date), parse_date(redcap_event_name, "%Y %m"), sample_date),
    organism = str_remove(organism, "_pos")
  ) %>%
  ggplot() +
  geom_point(aes(escatch.upstrmpop, percent_pos, fill = organism, color = organism), alpha = .5) +
  geom_smooth(aes(escatch.upstrmpop, percent_pos, fill = organism, color = organism), method = "gam") +
  geom_hline(aes(yintercept = 100)) +
  scale_x_continuous() +
  scale_fill_nejm() +
  scale_color_nejm() +
  theme_bw() +
  ylim(c(0,140)) +
  theme(legend.position = "top") +
  labs(
    title = "Phage contamination by Location",
    y = "Percent of Samples Contaminated (%)",
    x = "People Living upstream of Sampling Point"
  )

gam_model = gam(typhi_pos_num ~ s(stream_location, k = 3), data = gam_data, family = "binomial")

#run this as a logistic model where data is all in rows or where data is
# wonder if there is a fixed or random effect for location as a fixed, random effect on that
# The k fixes the numver of knobs / how deep the polynomial is to avoid overfitting

nepal_river_data_formatted %>%
  mutate(
    stream = case_when(
      stream_location == - 10 ~ "upstream",
      stream_location == - 5 ~ "city_center",
      stream_location == - 1 ~ "city_center",
      stream_location >= 0 ~ "downstream"
    ),
    typhi_pos_num = if_else(str_detect(typhi_pos, "Pos") == TRUE, 1, 0),
    paratyphi_pos_num = if_else(str_detect(paratyphi_pos, "Pos") == TRUE, 1, 0)
  ) %>%
  group_by(stream) %>%
  summarise(
    num_pos_typhi = sum(typhi_pos_num, na.rm = TRUE),
    num_pos_para = sum(paratyphi_pos_num, na.rm = TRUE),
    prop_pos_typhi = num_pos_typhi / n() * 100,
    prop_pos_paratyphi = num_pos_para / n() * 100
  )


#Separation of Kathmandu and Kavre

nepal_drinking_water_data %>%
  filter(country == 2) %>%
  select(water_date)






## The final GAM function used for the paper
visibly::plot_gam(gam_model, main_var = stream_location) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  xlim(c(0,3500000)) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "",
    y = "PCR positive for S.Typhi DNA(%)",
    x = "Downstream distance from confluence point (km)")

#visibly::plot_gam_check(gam_model)
#summary(gam_model)
#predict.gam(gam_model)




