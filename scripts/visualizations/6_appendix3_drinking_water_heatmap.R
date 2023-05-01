# Heatmap of river positvity over time by

# Author: Name
# Version: 2021-06-01

# Libraries
library(tidyverse)
library(gridExtra)

# Parameters

discrete_palettes <- list(
  c("skyblue", "orange"),
  RColorBrewer::brewer.pal(3, "Set2"),
  RColorBrewer::brewer.pal(6, "Accent")
)

levels <- c(
  "PN-3", "PN-2", "PN-1",
  "BAG-3","BIS-3","DBK-3","MAN-3",
  "BAG-2", "BIS-2","DBK-2","MAN-2",
  "BAG-1","BIS-1","DBK-1","MAN-1",
  "COM-0", "COM-1", "COM-2", "COM-3"
)

levels2 <- c(
  "PN-3", "PN-2", "PN-1",
  "BAG-3","BAG-2","BAG-1",
  "BIS-3", "BIS-2","BIS-1",
  "DBK-3", "DBK-2","DBK-1",
  "MAN-3","MAN-2","MAN-1",
  "COM-0", "COM-1", "COM-2", "COM-3"
)


#===============================================================================

#Code

#using sampling trip on x axis
nepal_river_data_formatted %>%
  ungroup() %>%
  #add_row(tibble_row(redcap_event_name = "2020 06 ")) %>%
  add_row(tibble_row(redcap_event_name = "2020 04 ")) %>%
  add_row(tibble_row(redcap_event_name = "2020 05 ")) %>%
  drop_na(sample_id) %>%
  mutate(sample_id = factor(sample_id, levels = levels)) %>%
  ggplot(aes(x = redcap_event_name, y = sample_id, fill = as_factor(typhi_pos))) +
  geom_tile(colour="white",size=0.25) +
  labs(x="Community",y="",title="")+
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_manual(values = c("darkblue", "skyblue") )+
  theme_minimal()  +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=8),
        legend.title  = element_text(size=8),
        axis.title.y = element_text(size=6),
        legend.position = "bottom"
  ) +
  theme(plot.margin = unit(c(-.5,.5,.5,.5), "cm"))



## Using Date on x axis
heatplot_date <-
  nepal_river_data_formatted %>%
  ungroup() %>%
  mutate(
    sample_date = parse_date(redcap_event_name_parsed, "%Y %m"),
    sample_date = if_else(is.na(sample_date), parse_date(redcap_event_name_parsed, "%Y %m"), sample_date),
  ) %>%
  mutate(
    sample_id = factor(sample_id, levels = levels2),
    sample_id = fct_rev(sample_id)
  ) %>%
  ggplot(
    aes(
      x = sample_date,
      y = sample_id,
      alpha = as_factor(typhi_pos),
      fill = river_name,
    )) +
  geom_tile(colour="gray20",size=0.3) +
  scale_y_discrete(expand=c(0,0)) +
  scale_x_date(date_breaks = "month", date_labels = "%b %y") +
  scale_alpha_manual(values = c(1, .2))+
  scale_fill_nejm(guide = FALSE) +
  # scale_color_manual(guide = FALSE, values = c(`TRUE` = "black"))
  theme_bw()  +
  theme(
    #axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size=8),
    legend.title  = element_text(size=8),
    axis.title.y = element_text(size=8),
    #axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = .5)
  ) +
  labs(
    alpha = "Typhi DNA",
    x = "River Water Contamination",
    y = ""
  ) +
  theme(plot.margin = unit(c(-.5,.5,-.5,.5), "cm"))



# Substract legend for heatmap
tmp <- ggplot_gtable(ggplot_build(heatplot_date))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]


HeatNolegend <- heatplot_date + theme(legend.position="none")

# library(RColorBrewer)

# display.brewer.pal(n = 8, name = "YlGnBu")
# brewer.pal(n = 8, name = "YlGnBu")

table(nepal_sees_data$Country)

Histplot <-
  nepal_sees_data %>%
  mutate(
    consentdt = parse_date(ScrVDate, format = "%d/%m/%Y"),
    case_month = floor_date(consentdt, "month")
  ) %>%
  filter(
    Country == "Nepal",
    consentdt > "2019-11-01"
  ) %>%
  group_by(case_month) %>%
  count() %>%
  ggplot(aes(case_month, n)) +
  geom_bar(stat = "identity", fill="#225EA8", color="white", alpha=.9) +
  theme_minimal() +
  scale_y_continuous(expand= c(0,0), minor_breaks = NULL) +
  scale_x_continuous( breaks=NULL) +
  theme(axis.title.y = element_text(size=8)) +
  labs(y = "Number of S.Typhi Cases",
       x = ""
  ) +
  theme(plot.margin = unit(c(1,.5,0.1,.5), "cm"))




gA <- ggplotGrob(Histplot)
gB <- ggplotGrob(HeatNolegend)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)



grid.arrange(
  gA,
  gB,
  heights=c(0.45,0.75),
  widths = c(1, .2),
  nrow = 2,
  layout_matrix = rbind(c(1, NA), c(2, 3))
)

fig3 <- grid.arrange(gA, gB, legend, heights=c(0.45,0.75), widths = c(1, .2),layout_matrix = rbind(c(1, NA),
                                                                                                   c(2, 3)))
