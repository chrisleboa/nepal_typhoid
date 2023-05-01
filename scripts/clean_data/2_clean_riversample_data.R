  # This file cleans the Nepal River water data tested for Typhi and Paratyphi using PCR and for phages
  ## It should be run after the 0_config file reads in all data

  # Author: Chris LeBoa
  # Version: 2021-05-30

  nepal_river_data_formatted <-
    nepal_river_data %>%
    select(-c(river_pcr_typhi_run_date)) %>%
    mutate_at(
      vars(matches("river_pcr")),
      ~as.double(recode(., "UD" = "99", .default = .)
      )) %>%
    mutate(
      typhi_pos =
        if_else(
          river_pcr_typhi_1_t16_ct < 35 | river_pcr_typhi_2_t16_ct < 35, "Pos", "Neg"),
      paratyphi_pos =
        if_else(
          river_pcr_paratyphi_1_t16_ct  < 35 | river_pcr_paratyphi_1_t16_ct < 35,
          "Pos",
          "Neg"
        ),
      typhi_border =
        case_when(
          river_pcr_paratyphi_1_t16_ct  < 35 | river_pcr_paratyphi_1_t16_ct < 35 ~ "Pos",
          (river_pcr_paratyphi_1_t16_ct  > 35 & river_pcr_paratyphi_1_t16_ct  < 40) |
            (river_pcr_paratyphi_1_t16_ct  > 35 & river_pcr_paratyphi_1_t16_ct  < 40) ~ "Border",
          TRUE ~ "Neg"
        ),
      sample_date = floor_date(as_date(river_datetime), "week"),
      sample_week = floor_date(sample_date, "week"),
      redcap_event_name =
            str_replace_all(str_extract(redcap_event_name, "[^a]+"), "_", " "),
      redcap_event_name_parsed =
        str_replace(redcap_event_name, "1b", "15"),
      sample_month =
        if_else(str_detect(redcap_event_name_parsed, "15"),
                             parse_date(redcap_event_name_parsed, format = "%Y %m %d "),
                if_else(str_detect(redcap_event_name_parsed, " 1$"),
                        parse_date(redcap_event_name_parsed, format = "%Y %m %d"),
                             parse_date(redcap_event_name_parsed, format = "%Y %m ")
                             )),
        stream_location = case_when(
          str_detect(sample_id, "BAG-3|BIS-3|DBK-3|MAN-3") ~ -10,
          str_detect(sample_id, "BAG-2|BIS-2|DBK-2|MAN-2")  ~ -5,
          str_detect(sample_id, "BAG-1|BIS-1|DBK-1|MAN-1") ~ -1,
          str_detect(sample_id, "COM-0")  ~ 0, #Need to ask Jason about this
          str_detect(sample_id, "COM-1") ~ 1,
          str_detect(sample_id, "COM-2") ~ 5,
          str_detect(sample_id, "COM-3") ~ 10,
          TRUE ~ NA_real_
        ),
      river_name = str_extract(sample_id, "^(.+?)-")
    ) %>%
    drop_na(typhi_pos)




  case_data <-
    nepal_sees_data %>%
    filter(Country == "Nepal") %>%
    mutate(
      enroll_date = parse_date(ScrVDate, format = "%d/%m/%Y"),
      month = month(enroll_date, label = TRUE)) %>%
    filter(
      year(enroll_date) > 2016,
      year(enroll_date) < 2020,
      str_detect(Species, "typhi") == TRUE
  )%>%
    #summarize(max = max(enroll_date),
   #           min = min(enroll_date))
    count(month, Species)

  case_data_month <-
    nepal_sees_data %>%
    filter(Country == "Nepal") %>%
    mutate(
      enroll_date = parse_date(ScrVDate, format = "%d/%m/%Y"),
      enroll_month = floor_date(enroll_date, "month"),
      month = month(enroll_date, label = TRUE)
      ) %>%
    filter(
      year(enroll_date) > 2016,
      year(enroll_date) < 2020,
      str_detect(Species, "typhi") == TRUE
    )%>%
    #summarize(max = max(enroll_date),
    #           min = min(enroll_date))
    count(enroll_month, Species)

  ##Original data went from 5/23/2016 to 9/1/2021 so limited to only years where all years of data are present


  # Weather

  nepal_river_data_formatted %>%
    group_by(sample_id) %>%
    count(river_wash_veg) %>%
    filter(river_wash_veg == 1)


