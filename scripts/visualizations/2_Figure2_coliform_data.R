# This makes the four paneled figure 2 for the nepal case data

# Figures are in order of
      # Plot 1: Coliform count over time
          #(Using a loess smooth line to show change over time)
      # Plot 2: Comparison of Coliform Count to location positivity
      # Plot 3: Typhi and paratyphi % of locations positive over time
      # Plot 4: Time of SEAP Cases throughout year
  # All plots are compiled onto the same grid for presentation as a single figure
# Author: Chris LeBoa
# Version: 2021-05-29


#===============================================================================
#install.packages("ggtext")
library(ggtext)
library(tidyverse)
library(lubridate)
nepal_river_data_formatted <- read_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/nepal_typhoid/data/rivers/rivers_formatted.csv")
# Code
nepal_river_data_formatted %>%
  filter(
    is.na(redcap_repeat_instance) | redcap_repeat_instance == 1,
    !is.na(river_e_coli_count)
  ) %>%
  mutate(month = month(sample_month, label = FALSE), month_number = month(sample_month, label = FALSE)) %>%
  #select(sample_month, redcap_event_name) %>%
  ggplot(aes(month, river_e_coli_count)) +
  #geom_path(color = "darkred") +
  geom_point()


coliform_month <-
  nepal_river_data_formatted %>%
  filter(
    is.na(redcap_repeat_instance) | redcap_repeat_instance == 1,
    !is.na(river_e_coli_count)
  ) %>%
  mutate(month = month(sample_month, label = TRUE), month_number = month(sample_month, label = FALSE)) %>%
  group_by(month_number) %>%
  summarise(col_mean = mean(river_e_coli_count),
            sd = sd(river_e_coli_count)
            # year = "2020-2021"
  ) %>%
  ungroup()

nepal_river_data_formatted %>% count(sample_month) %>% view()


combined_rain_coliform <-
  nepal_rainfall_month %>%
  left_join(coliform_month, by = c("month" = "month_number"))

plot_1  <-
  coliform_month %>%
  ggplot(aes(month_number, col_mean, group = 1)) +
  geom_col(fill = "darkred") +
  geom_errorbar(aes(ymin = col_mean - sd, ymax = col_mean + sd), width = 0.2) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_discrete(limits = month.abb) +
  scale_color_nejm() +
  theme_bw() +
  labs(
    title =  "A.",
    x = "month",
    y= "*E. Coli* coliform count (per mL)"
  ) +
  theme(axis.title.y = ggtext::element_markdown()) #USE GGText to get the italic naming in place

plot_1.7 <-
  nepal_rainfall_month %>%
  mutate(`weather station` = station) %>%
  #mutate(month = factor(month.abb[month],levels=month.abb)) %>%
  #write_csv(., "figure_2b_data.csv") PLOS wants data used for each figure
  ggplot(aes(month, rain_month, color = `weather station`)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  coord_cartesian(xlim = c(1,12), ylim = c(0,600)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_discrete(limits = month.abb)  +
  scale_color_nejm() +
  labs(
    title = "B.",
    x = "month",
    y = "monthly rain total (mm)"
  )


plot_2_data <-
  nepal_river_data_formatted %>%
  filter(
    is.na(redcap_repeat_instance) | redcap_repeat_instance == 1,
    !is.na(river_e_coli_count)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(typhi_pos, paratyphi_pos), names_to = "organism", values_to = "dna") %>%
  mutate(organism = str_remove(organism, "_pos")) %>%
  drop_na(dna)

plot_2_data %>%
  select(dna, river_e_coli_count, dna) %>%
   write_csv(., "figure_2a_data.csv") #Plos wants datasets used for each figure

plot_2 <-
  plot_2_data %>%
  ggplot(aes(dna, river_e_coli_count, color = dna)) +
  geom_boxplot() +
  #geom_jitter(alpha = .5) +
  coord_cartesian( ylim = c(0,100)) +
  facet_wrap(vars(organism)) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_nejm() +
  theme_bw() +
  theme(
    legend.position = ""
  ) +
  labs(
    title =  "B.",
    x = "Month",
    y= expression(paste(italic("E. Coli"),"coliform count (per mL)")),
    fill="Var1",
    color = ""
  )

num_samples <-
  nepal_river_data_formatted %>%
  filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
  mutate(month = month(sample_month, label = TRUE)) %>%
  count(month) %>%
  rename(num_samples = n)

plot_3_data <-
  nepal_river_data_formatted %>%
    filter(is.na(redcap_repeat_instance) | redcap_repeat_instance == 1) %>%
    pivot_longer(cols = c(typhi_pos, paratyphi_pos), names_to = "organism", values_to = "dna") %>%
    add_row(tibble_row(sample_month = as.Date("2020-08-01"), dna = "Pos", organism = "typhi_pos")) %>%
    add_row(tibble_row(sample_month = as.Date("2020-08-01"), dna = "Pos", organism = "paratyphi_pos")) %>%
 # add_row(tibble_row(month = "Aug", dna = "Pos", organism = "paratyphi_pos", percent_pos = NA)) %>%
    mutate(
      month = month(sample_month, label = TRUE),
      month_num = month(sample_month, label = FALSE)) %>%
   # mutate(orgamism = recode(organism, `paratyphi_pos` = "S. Paratyphi", `typhi_pos` = "S. Typhi")) %>%
    group_by(month, organism) %>%
   # group_by(redcap_event_name) %>%
    count(dna) %>%
    filter(!is.na(dna)) %>%
    left_join(num_samples, by = "month") %>%
    mutate(
      percent_pos = n / num_samples * 100
      ) %>%
    ungroup() %>%
   # drop_na(month) %>%
    #view()
    #add_row(tibble_row(redcap_event_name = "2020 06 ", organism = "paratyphi_pos", percent_pos = 0)) %>%
    #add_row(tibble_row(redcap_event_name = "2020 04 ", organism = "paratyphi_pos", percent_pos = 0)) %>%
    filter(dna == "Pos")

## Download data used for figure 2C


  plot_3 <-
    plot_3_data %>%
    ggplot() +
    geom_point(aes(month, percent_pos, color = organism, group = organism)) +
    geom_smooth(data = dplyr::filter(plot_3_data, month > "Aug"), aes(month, percent_pos, color = organism, group = organism), se = F) +
    geom_smooth(data = dplyr::filter(plot_3_data, month < "Aug"), aes(month, percent_pos, color = organism, group = organism), se = F) +
    #geom_vline(xintercept = "Aug", color = "grey40", linetype = "dashed", alpha = .5) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_color_discrete(labels = c("Paratyphi", "Typhi")) +
    scale_color_nejm() +
    labs(
      title = "C.",
      y = "% of samples PCR positive",
      x = "month"
    )

# plot_4 <-
#   case_data %>%
#   ggplot(aes(month, n, color = Species, group = ye)) +
#   geom_point() +
#   geom_line() +
#   facet_grid(Species) +
#   scale_color_nejm() +
#   theme_bw() +
#   theme(legend.position = "top") +
#   labs(
#     title = "SEAP Enteric Fever Cases by Month 2017 -2019",
#     y  = "Number of cases",
#     x = "Month"
#     )

plot_5 <-
  case_data_month %>%
  mutate(
    year = year(enroll_month),
    month = month(enroll_month)
  ) %>%
  group_by(year) %>%
  ggplot(aes(month, n, color = as_factor(year))) +
  geom_point() +
  geom_line() +
  facet_grid(cols = vars(Species)) +
  #facet_grid(Species) +
  scale_color_nejm() +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top"
    ) +
  labs(
    title = "Enteric Fever Cases by Month 2017 -2019",
    y  = "Number of cases",
    x = "Month",
    color = "",
    caption = "source: SEAP hospital surveillance"
  )

grid.arrange(plot_1, plot_1.7, plot_3, ncol = 1)

#Write data used for graphs
write_csv(coliform_month, "figure_2a_data.csv")
write_csv(nepal_rainfall_month, "figure_2b_data.csv")
plot_3_data %>%
  select(month, percent_pos, organism) %>%
  write_csv(., "figure_2c_data.csv")

