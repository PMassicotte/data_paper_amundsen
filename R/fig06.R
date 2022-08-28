# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Plot of days of open water.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Shapefile ---------------------------------------------------------------

wm <-
  rnaturalearth::ne_download(
    scale = "large",
    type = "countries",
    returnclass = "sf"
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
walk(files, ~ unzip(., exdir = dirname(.)))

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
  facet_wrap(~date)

# Plot sea ice extent -----------------------------------------------------

p2 <- sic %>%
  arrange(date) %>%
  mutate(date = format(date, "%B %d, %Y")) %>%
  mutate(date = fct_inorder(date)) %>%
  ggplot() +
  geom_sf(fill = "white", size = 0.1) +
  geom_sf(data = wm, size = 0.1, fill = "gray75") +
  facet_wrap(~date) +
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
  "graphs/fig06.pdf",
  device = cairo_pdf,
  width = 14,
  height = 14,
  units = "cm"
)

knitr::plot_crop("graphs/fig06.pdf")
