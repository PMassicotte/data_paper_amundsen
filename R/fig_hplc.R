rm(list = ls())

source("R/utils.R")


# CTD ---------------------------------------------------------------------

hplc <- vroom::vroom("data/clean/greenedge_pigments.csv", delim = ",") %>%
  filter(mission == "amundsen_2016") %>%
  filter(str_starts(station, "G")) %>%
  mutate(transect = parse_number(station) %/% 100 * 100) %>%
  filter(transect %in% c(300, 500)) %>%
  filter(depth_m <= 200) %>%
  filter(method == "HPLC") %>%
  tibble::rowid_to_column() %>%
  select(rowid, date, latitude, longitude, transect, depth_m, pigment, conc_mg_m3)

hplc %>%
  distinct(transect)

# Interpolate data on regular grid ----------------------------------------

hplc <- hplc %>%
  pivot_wider(names_from = pigment, values_from = conc_mg_m3) %>%
  rowwise() %>%
  mutate(diadinoxanthin_diatoxanthin = sum(Diadinoxanthin, Diatoxanthin, na.rm = TRUE)) %>%
  pivot_longer(-c(rowid:depth_m), names_to = "pigment", values_to = "conc_mg_m3") %>%
  filter(
    pigment %in% c(
      "sum Phbd a",
      "Zeaxanthin",
      "Total Chlorophyll a",
      "diadinoxanthin_diatoxanthin"
    )
  ) %>%
  drop_na()

hplc_viz <- hplc %>%
  group_nest(transect, pigment) %>%
  mutate(variable_interpolated = map(
    data,
    ~ interpolate_to_tibble(., longitude, depth_m, conc_mg_m3, n = 100)
  )) %>%
  select(-data) %>%
  unnest(c(variable_interpolated)) %>%
  pivot_wider(names_from = pigment, values_from = conc_mg_m3) %>%
  janitor::clean_names()

# Plot --------------------------------------------------------------------

hplc_viz

p1 <- hplc_viz %>%
  drop_na(total_chlorophyll_a) %>%
  ggplot(aes(
    x = longitude,
    y = depth_m,
    fill = total_chlorophyll_a,
    z = total_chlorophyll_a
  )) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    expand = c(0, 0),
    labels = function(x) {
      paste0(-x, "°W")
    },
    breaks = scales::pretty_breaks(n = 3)
  ) +
  scale_fill_gradientn(
    colours = color,
    guide = guide_colorbar(
      title.position = "top",
      nrow = 1,
      barwidth = unit(10, "cm"),
      title.hjust = 0.5
    )
  ) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~transect, nrow = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(vjust = -18),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0),
    strip.text = element_blank()
  ) +
  ylab("Depth (m)") +
  xlab("Longitude") +
  labs(fill = bquote("Chla" ~ (mg ~ m^{-3})))

p2 <- hplc_viz %>%
  drop_na(sum_phbd_a) %>%
  ggplot(aes(
    x = longitude,
    y = depth_m,
    fill = sum_phbd_a,
    z = sum_phbd_a
  )) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    expand = c(0, 0),
    labels = function(x) {
      paste0(-x, "°W")
    },
    breaks = scales::pretty_breaks(n = 3)
  ) +
  scale_fill_gradientn(
    colours = color,
    guide = guide_colorbar(
      title.position = "top",
      nrow = 1,
      barwidth = unit(10, "cm"),
      title.hjust = 0.5
    )
  ) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~transect, nrow = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(vjust = -18),
    legend.margin = margin(0, 0, 0, 0),
    legend.position = "bottom",
    legend.box.margin = margin(0, 0, 30, 0),
    strip.text = element_blank()
  ) +
  ylab("Depth (m)") +
  xlab("Longitude") +
  labs(fill = bquote("Total phaeophorbid concentration" ~ (mg ~ m^{-3})))

# p3 <- hplc_viz %>%
#   drop_na(diadinoxanthin_diatoxanthin) %>%
#   ggplot(aes(x = longitude, y = depth_m, fill = diadinoxanthin_diatoxanthin, z = diadinoxanthin_diatoxanthin)) +
#   scale_y_reverse(expand = c(0, 0)) +
#   scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W"), breaks = scales::pretty_breaks(n = 3)) +
#   scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1)) +
#   geom_isobands(color = NA, bins = 100) +
#   facet_wrap(~transect, nrow = 1, strip.position = "top") +
#   theme(
#     panel.spacing = unit(1, "lines"),
#     plot.title = element_text(vjust = -18),
#     legend.margin = margin(0, 0, 0, 0),
#     legend.box.margin = margin(0, 0, 30, 0)
#   ) +
#   ylab("Depth (m)") +
#   xlab("Longitude")


# Zooplankton -------------------------------------------------------------

zoo <- readxl::read_excel(
  "data/raw/zooplankton_condensed_GE.xlsx",
  sheet = 1
) %>%
  janitor::clean_names() %>%
  mutate(station = parse_number(station_name), .after = station_name) %>%
  select(
    station,
    contains("m3")
  ) %>%
  mutate(transect = station %/% 100 * 100, .after = station) %>%
  filter(transect %in% c(300, 500)) %>%
  pivot_longer(contains("m3"))

zoo

zoo <- zoo %>%
  mutate(zoo_type = case_when(
    str_detect(name, "^copepode") ~ "Copepode",
    str_detect(name, "^non_copepode") ~ "Non-Copepode"
  ))

p3 <- zoo %>%
  ggplot(aes(x = factor(station), y = value, fill = zoo_type)) +
  geom_col(position = "dodge") +
  facet_wrap(~transect, scales = "free_x") +
  labs(
    y = quote("Abundance" ~ (ind~m^{-3})),
    x = "Stations"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    fill = element_blank()
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_blank()
  )

# SIC ---------------------------------------------------------------------

# I will use the coordinate of the CTD and MVP to extract the SIC that will be
# plotted.

ctd <- vroom::vroom("data/clean/greenedge_ctd.csv", delim = ",") %>%
  filter(mission == "amundsen_2016") %>%
  filter(str_starts(station, "G")) %>%
  mutate(transect = parse_number(station) %/% 100 * 100) %>%
  filter(transect %in% c(300, 500)) %>%
  filter(depth_m <= 200) %>%
  mutate(source = "ctd") %>%
  distinct(date, transect, longitude, latitude)

mvp <- vroom::vroom("data/clean/greenedge_mvp.csv", altrep = TRUE) %>%
  mutate(initial_longitude_deg = -initial_longitude_deg) %>%
  filter(pres <= 200)

# Recode the transect number
mvp <- mvp %>%
  mutate(transect = case_when(
    section_number %in% c("2016001_01", "2016001_02") ~ 100,
    section_number %in% c("2016001_05") ~ 400,
    section_number %in% c("2016001_04") ~ 300,
    section_number %in% c("2016001_08", "2016001_09") ~ 600,
    section_number %in% c("2016001_06", "2016001_07") ~ 500,
    section_number %in% c("2016001_03") ~ 200,
    TRUE ~ NA_real_
  )) %>%
  filter(transect %in% c(300, 500)) %>%
  add_column(source = "mvp", .before = 1) %>%
  distinct(date, transect, longitude = initial_longitude_deg, latitude = initial_latitude_deg)

stations <- bind_rows(ctd, mvp)

stations %>%
  group_by(transect) %>%
  summarise(median_date = median(date))

extract_sic <- function(date, transect, longitude, latitude) {

  df <- tibble(date, transect, longitude, latitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # Read SIC and convert it to a spatial object.
  sic_file <- glue("data/raw/sic/Arc_{format(df$date, '%Y%m%d')}_res3.125_pyres.nc")

  sic <- read_ncdf(sic_file)

  sic <- read_ncdf("../green_edge/data/ice_concentration/LongitudeLatitudeGrid_3.125km_Arctic.nc", var = c("longitude", "latitude")) %>%
    as_tibble() %>%
    select(-x, -y) %>%
    mutate(sic = as.vector(sic$sea_ice_concentration)) %>%
    filter(latitude >= 65 & between(longitude, -65, -50)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # Find the closest pixels
  df %>%
    st_join(sic, nngeo::st_nn, k = 1) %>%
    cbind(st_coordinates(.)) %>%
    janitor::clean_names() %>%
    as_tibble()
}

sic <- stations %>%
  future_pmap_dfr(~extract_sic(..1, ..4, ..3, ..2), .progress = TRUE)

# SIC plots ---------------------------------------------------------------

p_sic_t300 <- sic %>%
  filter(transect == 300) %>%
  ggplot(aes(x = x, y = sic)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W")) +
  xlab(NULL) +
  ylab("SIC (%)") +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_blank()
  )

p_sic_t500 <- sic %>%
  filter(transect == 500) %>%
  ggplot(aes(x = x, y = sic)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W")) +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_blank()
  )

# Combine plots -----------------------------------------------------------

p <-
  {
    (p_sic_t300 + labs(title = "Transect 300")) + (p_sic_t500 + labs(title = "Transect 500"))
  } + plot_layout(ncol = 2) -
  {
    p1 / p2 / p3
  } +
  plot_layout(nrow = 2, heights = c(0.1, 1))


# p <- p1 / p2 / p3 +
#   plot_layout(heights = c(1, 1, 1)) +
#   plot_annotation(tag_levels = "A")

ggsave(
  "graphs/fig_hplc.png",
  dpi = 600,
  width = 12,
  height = 14,
  units = "in"
)
