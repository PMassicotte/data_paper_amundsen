rm(list = ls())

track <- st_read("../green_edge/data/doc.kml", layer = "Tracks")
sampling_dates <- read_csv("/media/data4tb/greenedge/greenedge_log_clean.csv")

stations <-
  read_csv("/media/data4tb/greenedge/greenedge_stations_clean.csv") %>%
  distinct(station, longitude, latitude, .keep_all = TRUE) %>%
  filter(str_starts(station, "G")) %>%
  mutate(station_type = case_when(
    str_detect(station_type, regex("nut", ignore_case = TRUE)) ~ "Basic",
    TRUE ~ station_type
  )) %>%
  filter(station_type %in% c("Full", "Basic", "Nutrient", "CTD")) %>%
  mutate(transect = parse_number(station) %/% 100 * 100) %>%
  filter(transect <= 700) %>%
  mutate(transect = as.factor(transect)) %>%
  drop_na(date)

stations %>%
  count(station_type)

stations2 <- stations %>%
  group_by(transect) %>%
  mutate(median_date = as.Date(median(date)))

baffin <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_crop(c(
    xmin = -65,
    ymin = 60,
    xmax = -45,
    ymax = 85
  )) %>%
  st_transform(crs = "+proj=stere +lat_0=90 +lat_ts=75 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  as("Spatial")

plot(baffin)

bb <- raster("data/raw/IBCAO_V3_500m_RR.tif") %>%
  crop(baffin)

bb2 <- bb %>%
  sampleRegular(size = 1e4, asRaster = TRUE) %>%
  projectRaster(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  rasterToPoints() %>%
  as_tibble() %>%
  rename(z = 3)

wm <-
  rnaturalearth::ne_download(
    scale = "large",
    type = "countries",
    returnclass = "sf"
  )

# MIZ ---------------------------------------------------------------------

# "https://www.natice.noaa.gov/pub/daily/arctic/2016//ice_edge/nic_autoc2016157n_pl_a.zip"

base_url <- "https://www.natice.noaa.gov/pub/daily/arctic/2016//ice_edge/nic_autoc2016{yday}n_pl_a.zip"
url <- glue(base_url, yday = lubridate::yday(unique(stations2$median_date)))
destfiles <- fs::path("data/raw/shapefiles_miz/", basename(url))

# Download
walk2(url, destfiles, ~ curl::curl_download(..1, ..2))

# Unzip
walk(destfiles, unzip, exdir = "data/raw/shapefiles_miz/")


# Crop MIZ shape ----------------------------------------------------------

shapefiles <- fs::dir_ls("data/raw/shapefiles_miz/", glob = "*.shp")

baffin_interior <- st_read("data/clean/shapefile_baffin_bay/test2.shp")

shapes <- future_map(shapefiles, function(shapefile) {
  st_read(shapefile, quiet = TRUE) %>%
    st_cast("LINESTRING") %>%
    st_crop(c(xmin = -65, ymin = 66.5, xmax = -50, ymax = 72)) %>%
    st_intersection(baffin_interior) %>%
    mutate(
      date = str_extract(shapefile, "\\d{7}") %>% lubridate::parse_date_time(., orders = "Yj") %>% as.character()
    )
}, .progress = TRUE)

shapes2 <- shapes %>%
  do.call(rbind, .) %>%
  mutate(date = as.Date(date)) %>%
  right_join(stations2, by = c("date" = "median_date"))


# Plot --------------------------------------------------------------------

# shapes3 <- shapes2 %>%
#   st_cast("POINT") %>%
#   cbind(., st_coordinates(.)) %>%
#   arrange(transect, latitude)

p <- bb2 %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z)) %>%
  ggplot(aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z)) +
  ggisoband::geom_isobands(bins = 25, color = NA) +
  paletteer::scale_fill_paletteer_c(ggthemes, Blue, direction = -1, limits = c(-2500, 0), oob = scales::squish, guide = "legend", breaks = -c(0, 1000, 2000, 3000)) +
  paletteer::scale_color_paletteer_d(RColorBrewer, Set1) +
  geom_sf(
    data = wm,
    size = 0.1,
    inherit.aes = FALSE,
    fill = "white"
  ) +
  geom_sf(data = shapes2, aes(color = transect), show.legend = FALSE, size = 0.1, inherit.aes = FALSE) +
  coord_sf(xlim = c(-70.5, -43), ylim = c(65, 72)) +
  annotate(
    geom = "text",
    x = -63.5,
    y = 67.5,
    label = "Qikiqtarjuaq",
    vjust = -0.25,
    hjust = 0,
    size = 2,
    family = "Poppins"
  ) +
  annotate(
    geom = "text",
    x = -48,
    y = 70,
    label = "Greenland",
    vjust = 0,
    hjust = 0,
    size = 3,
    family = "Poppins",
    fontface = 2
  ) +
  annotate(
    geom = "text",
    x = -70,
    y = 71.5,
    label = "Baffin Bay",
    vjust = 0,
    hjust = 0,
    size = 4,
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
    size = 3,
    family = "Poppins",
    fontface = 2
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.08,
    height = unit(0.1, "cm"),
    line_width = 0.1,
    text_cex = 0.5
  ) +
  geom_point(
    data = stations,
    aes(x = longitude, y = latitude, shape = station_type, color = transect),
    size = 0.5,
    inherit.aes = FALSE
  ) +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.98, 0.01),
    legend.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "#B9DDF1"),
    legend.key = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 0, unit = "cm")
    # legend.direction = "vertical", legend.box = "horizontal"
  ) +
  labs(
    fill = "Depth (m)",
    shape = "Station",
    color = "Transect"
  ) +
  guides(
    fill = guide_colorbar(barwidth = unit(2, "cm"), barheight = unit(0.1, "cm"), direction = "horizontal", title.position = "top"),
    color = guide_legend(ncol = 2, override.aes = list(size = 1)),
    shape = guide_legend(ncol = 2, override.aes = list(size = 1))
  ) +
  annotate("point", x = -63.78953333, y = 67.47973333, size = 0.5)

ggsave(
  "graphs/fig01.pdf",
  device = cairo_pdf,
  width = 12,
  height = 12 / 1.229081,
  units = "cm"
)



