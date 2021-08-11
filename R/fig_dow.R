# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Plot of days of open water.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

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

dow <- read_csv("../green_edge/data/clean/dow_amundsen_2016.csv")

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
  facet_wrap(~ facet_label, ncol = 1) +
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
  "graphs/fig_dow.pdf",
  device = cairo_pdf,
  width = 4,
  height = 8
)

knitr::plot_crop("graphs/fig_dow.pdf")

pdftools::pdf_convert(
  pdf = "graphs/fig_dow.pdf",
  filenames = "graphs/fig_dow.png",
  dpi = 600
)

# bbox of Baffin ----------------------------------------------------------

baffin <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_crop(c(
    xmin = -65,
    ymin = 60,
    xmax = -45,
    ymax = 85
  )) %>%
  st_transform(crs = "+proj=stere +lat_0=90 +lat_ts=75 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Sea ice extent ----------------------------------------------------------

sic <- tibble(
  date =
    as.Date(c(
      "2016-06-13",
      "2016-06-21",
      "2016-06-29",
      "2016-07-07"
    ))
) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(yday = str_pad(yday, width = 3, side = "left", pad = "0")) %>%
  mutate(url = glue("ftp://sidads.colorado.edu/DATASETS/NOAA/G02186/shapefiles/1km/{year}/masie_ice_r00_v01_{year}{yday}_1km.zip"))

sic

walk(
  sic$url,
  ~ curl::curl_download(., destfile = glue(
    "data/raw/sea_ice_extent/{basename(.)}"
  ))
)

# Unzip sea ice concentration files
files <- fs::dir_ls("data/raw/sea_ice_extent/", glob = "*.zip")
walk(files, ~unzip(., exdir = dirname(.)))

files <- fs::dir_ls("data/raw/sea_ice_extent/", glob = "*.shp")

sic <- map(files, st_read) %>%
  do.call(rbind, .) %>%
  rownames_to_column(var = "date") %>%
  mutate(date = str_match(date, "\\d{7}")) %>%
  mutate(date = as.Date(date, "%Y%j"))

sic <- sic %>%
  st_transform(crs = st_crs(baffin)) %>%
  st_crop(st_bbox(baffin))

# Shiptrack ---------------------------------------------------------------

track <- read_csv("data/raw/greenedge_navigation_15min.csv") %>%
  mutate(date = as.Date(date)) %>%
  crossing(sic_date = unique(sic$date)) %>%
  group_nest(sic_date) %>%
  mutate(track = map2(data, sic_date, ~ filter(
    ., between(.$date, .y - lubridate::days(3), .y + lubridate::days(3))
  ))) %>%
  select(-data) %>%
  unnest(track) %>%
  select(-date) %>%
  rename(date = sic_date) %>%
  arrange(date) %>%
  mutate(date = format(date, "%B %d, %Y")) %>%
  mutate(date = fct_inorder(date))

track %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_path() +
  facet_wrap( ~ date)

# Plot sea ice extent -----------------------------------------------------

p2 <- sic %>%
  arrange(date) %>%
  mutate(date = format(date, "%B %d, %Y")) %>%
  mutate(date = fct_inorder(date)) %>%
  ggplot() +
  geom_sf(fill = "white", size = 0.1) +
  geom_sf(data = wm, size = 0.1, fill = "gray75") +
  facet_wrap(~ date) +
  coord_sf(
    crs = 4326,
    xlim = c(-70.5, -43),
    ylim = c(65, 72)
  ) +
  geom_path(
    data = track,
    aes(x = longitude, y = latitude),
    size = 0.25,
    inherit.aes = FALSE
  ) +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#2171B5"),
    panel.border = element_blank()
  )

ggsave(
  "graphs/fig_sea_ice_extent.pdf",
  device = cairo_pdf,
  width = 6,
  height = 6
)

knitr::plot_crop("graphs/fig_sea_ice_extent.pdf")

pdftools::pdf_convert(
  pdf = "graphs/fig_sea_ice_extent.pdf",
  filenames = "graphs/fig_sea_ice_extent.png",
  dpi = 600
)
