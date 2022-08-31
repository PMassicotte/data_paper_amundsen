rm(list = ls())

stations <-
  read_csv("data/clean/greenedge_stations_clean.csv") %>%
  distinct(station, longitude, latitude, .keep_all = TRUE) %>%
  filter(str_starts(station, "G")) %>%
  mutate(station_type = case_when(
    str_detect(station_type, regex("nut", ignore_case = TRUE)) ~ "Nutrient",
    TRUE ~ station_type
  )) %>%
  filter(station_type %in% c("Full", "Basic", "Nutrient", "CTD")) %>%
  mutate(transect = parse_number(station) %/% 100 * 100) %>%
  filter(transect <= 700) %>%
  mutate(transect = as.factor(transect)) %>%
  drop_na(date)

dow <- read_csv("data/raw/dow_amundsen_2016.csv")

# Get the geographical coordinates
dow <- dow %>%
  inner_join(stations)

# Shapefile ---------------------------------------------------------------

wm <-
  rnaturalearth::ne_download(
    scale = "large",
    type = "countries",
    returnclass = "sf"
  )

# Plot --------------------------------------------------------------------

p1 <- dow %>%
  mutate(facet_label = glue("Using a SIC of {sic_target * 100}%")) %>%
  ggplot(aes(x = longitude, y = latitude, color = dow_length)) +
  geom_point() +
  facet_wrap(~facet_label, ncol = 1) +
  scale_color_viridis_c() +
  geom_sf(
    data = wm,
    size = 0.1,
    inherit.aes = FALSE,
    fill = "gray75"
  ) +
  coord_sf(xlim = c(-68, -52), ylim = c(67, 71)) +
  labs(
    color = "Open\nWater\nDays"
  ) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  "graphs/fig07.pdf",
  device = cairo_pdf,
  width = 10,
  height = 16,
  units = "cm"
)

knitr::plot_crop("graphs/fig07.pdf")
