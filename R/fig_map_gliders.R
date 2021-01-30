# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Glider data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/utils.R")

wm <- rnaturalearth::ne_download(
  scale = "large",
  type = "countries",
  returnclass = "sf"
)

df <- vroom::vroom("data/clean/gliders.csv")

df %>%
  count(as.Date(date_time), sort = TRUE)

df %>%
  count(filename)

df <- df %>%
  mutate(glider_id = str_extract(filename, "qala\\d"))

# Transects ---------------------------------------------------------------

track <- st_read("../green_edge/data/doc.kml", layer = "Tracks")
sampling_dates <-
  read_csv("/media/data4tb/greenedge/greenedge_log_clean.csv")

stations <-
  read_csv("/media/data4tb/greenedge/greenedge_stations_clean.csv") %>%
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

# Bathymetry --------------------------------------------------------------

baffin <- ne_countries(scale = "large", returnclass = "sf") %>%
  st_crop(c(
    xmin = -65,
    ymin = 60,
    xmax = -45,
    ymax = 85
  )) %>%
  st_transform(crs = "+proj=stere +lat_0=90 +lat_ts=75 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  as("Spatial")

bb <- raster("data/raw/IBCAO_V3_500m_RR.tif") %>%
  crop(baffin)

bb2 <- bb %>%
  sampleRegular(size = 1e4, asRaster = TRUE) %>%
  projectRaster(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  rasterToPoints() %>%
  as_tibble() %>%
  rename(z = 3)

bb2 <- bb2 %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z))

# Geographic positions ----------------------------------------------------

set.seed(2019)

# Overview of the data, so I can choose a longitudinal transect
p <- df %>%
  group_by(filename, glider_id) %>%
  sample_frac(0.005) %>%
  arrange(time) %>%
  ungroup() %>%
  mutate(date = as.Date(date_time)) %>%
  select(filename, glider_id, time, latitude, longitude, date) %>%
  ggplot() +
  ggisoband::geom_isobands(
    data = bb2,
    aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z),
    bins = 25,
    color = NA,
    show.legend = FALSE
  ) +
  paletteer::scale_fill_paletteer_c(
    ggthemes,
    Blue,
    direction = -1,
    limits = c(-2500, 0),
    oob = scales::squish,
    guide = "legend",
    breaks = -c(0, 1000, 2000, 3000)
  ) +
  geom_sf(
    data = wm,
    size = 0.1,
    inherit.aes = FALSE,
    fill = "white"
  ) +
  geom_path(
    data = stations,
    aes(x = longitude, y = latitude, group = transect),
    lty = 2,
    size = 0.25,
    color = "#333333"
  ) +
  geom_point(aes(
    x = longitude,
    y = latitude,
    color = glider_id,
    group = filename
  ),
  size = 0.1
  ) +
  coord_sf(xlim = c(-65, -48), ylim = c(67, 71)) +
  xlab(NULL) +
  ylab(NULL) +
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
  annotate("point",
    x = -63.78953333,
    y = 67.47973333,
    size = 0.5
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.05,
    height = unit(0.05, "cm"),
    line_width = 0.1,
    text_cex = 0.5
  ) +
  labs(
    color = "Glider name"
  ) +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(0.98, 0.98),
    legend.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "#B9DDF1"),
    legend.key = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 0, unit = "cm")
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 1))
  )

ggsave(
  "graphs/fig_map_gliders.pdf",
  device = cairo_pdf,
  width = 12,
  height = 12 / 1.229081,
  units = "cm"
)



# files <- fs::dir_ls("/media/data4tb/greenedge/DATA/not-on-cyber-yet/gliders/NetCDF/", glob = "*.nc")
#
# df <- future_map(files, tidync::hyper_tibble, .progress = TRUE) %>%
#   set_names(basename(files)) %>%
#   bind_rows(.id = "filename") %>%
#   janitor::clean_names() %>%
#   mutate(date_time = lubridate::as_datetime(time))
#
# data.table::fwrite(df, "data/clean/gliders.csv")
