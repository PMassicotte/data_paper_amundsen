# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Figure showing the MIZ during the Amundsen cruise.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# https://www.researchgate.net/publication/305822747_Accuracy_of_Sea_Ice_Data_from_Remote_Sensing_Methods_its_Impact_on_Safe_Speed_Determination_and_Planning_of_Voyage_in_Ice-Covered_Areas
# https://www.natice.noaa.gov/products/daily_products.html

track <- st_read("../green_edge/data/doc.kml", layer = "Tracks")

sampling_dates <- read_csv("/media/data4tb/greenedge/greenedge_log_clean.csv")

wm <-
  rnaturalearth::ne_download(
    scale = "large",
    type = "countries",
    returnclass = "sf"
  )

range(sampling_dates$date)

# miz1 <- st_read("c:/Users/pmass/Downloads/nic_miz2016192nc_pl_a/nic_miz2016192nc_pl_a.shp")


# Downlaod MIZ data -------------------------------------------------------

# "https://www.natice.noaa.gov/pub/daily/arctic/2016//ice_edge/nic_autoc2016157n_pl_a.zip"

base_url <-
  "https://www.natice.noaa.gov/pub/daily/arctic/2016//ice_edge/nic_autoc2016{yday}n_pl_a.zip"
url <-
  glue(base_url, yday = lubridate::yday(unique(sampling_dates$date)))
destfiles <- fs::path("data/raw/shapefiles_miz/", basename(url))

# Download
walk2(url, destfiles, ~ curl::curl_download(..1, ..2))

# Unzip
walk(destfiles, unzip, exdir = "data/raw/shapefiles_miz/")


# Crop shapefiles ---------------------------------------------------------

shapefiles <-
  fs::dir_ls("data/raw/shapefiles_miz/", glob = "*.shp") %>%
  .[c(1, 18, 36)]

shapes <- future_map(shapefiles, function(shapefile) {
  st_read(shapefile, quiet = TRUE) %>%
    st_crop(c(
      xmin = -90,
      ymin = 55,
      xmax = -35,
      ymax = 90
    )) %>%
    mutate(
      date = str_extract(shapefile, "\\d{7}") %>% lubridate::parse_date_time(., orders = "Yj") %>% as.character()
    )
}, .progress = TRUE)

shapes2 <- shapes %>%
  do.call(rbind, .)


# Plot --------------------------------------------------------------------
stations <-
  read_csv("/media/data4tb/greenedge/greenedge_stations_clean.csv") %>%
  distinct(station, longitude, latitude) %>%
  filter(str_starts(station, "G"))

p <- shapes2 %>%
  ggplot() +
  geom_sf(data = wm, size = 0.1,fill = "gray95") +
  geom_sf(aes(fill = date), alpha = 0.5, color = NA) +
  # geom_sf(data = track, color = "red") +
  coord_sf(xlim = c(-70, -45), ylim = c(65, 72)) +
  paletteer::scale_fill_paletteer_d(rcartocolor, Teal) +
  annotate(
    geom = "text",
    x = -63.5,
    y = 67.5,
    label = "Qikiqtarjuaq",
    vjust = -0.25,
    hjust = 0,
    size = 4,
    family = "Poppins"
  ) +
  annotate(
    geom = "text",
    x = -49,
    y = 69,
    label = "Greenland",
    vjust = 0,
    hjust = 0,
    size = 6,
    family = "Poppins",
    fontface = 2
  ) +
  annotate(
    geom = "text",
    x = -62,
    y = 71.5,
    label = "Baffin Bay",
    vjust = 0,
    hjust = 0,
    size = 6,
    family = "Poppins",
    fontface = 2
  ) +
  annotate(
    geom = "text",
    x = -70.5,
    y = 67,
    label = "Baffin Island",
    vjust = 0,
    hjust = 0,
    size = 6,
    family = "Poppins",
    fontface = 2
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.1,
    height = unit(0.1, "cm"),
  ) +
  geom_point(
    data = stations,
    aes(x = longitude, y = latitude),
    size = 1
  ) +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    legend.title = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.98, 0.01),
    legend.background = element_rect(fill = "gray95"),
    legend.key = element_rect(fill = "gray95"),
    panel.grid = element_blank()
    # ,
    # panel.background = element_rect(fill = "#a8dbd9")
  )

ggsave("graphs/fig02.pdf", device = cairo_pdf, width = 8.96, height = 7.29)
