# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Floats data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

track <- st_read("../green_edge/data/doc.kml", layer = "Tracks")
sampling_dates <- read_csv("data/clean/greenedge_log_clean.csv")

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

# Crop MIZ shape ----------------------------------------------------------

shapefiles <- fs::dir_ls("data/raw/shapefiles_miz/", glob = "*.shp")
# shapefiles <- fs::dir_ls("~/Downloads/MIZ files/", glob = "*.shp", recurse = TRUE)
# shapefiles <- "/home/pmassicotte/Downloads/MIZ files/nic_miz2016163nc_pl_a/nic_miz2016163nc_pl_a.shp"
baffin_interior <-
  st_read("data/clean/shapefile_baffin_bay/test2.shp")

shapes <- future_map(shapefiles, function(shapefile) {
  st_read(shapefile, quiet = TRUE) %>%
    # filter(ICECODE == "CT18") %>%
    st_cast("LINESTRING") %>%
    st_crop(c(
      xmin = -65,
      ymin = 66.5,
      xmax = -50,
      ymax = 72
    )) %>%
    st_intersection(baffin_interior) %>%
    mutate(
      date = str_extract(shapefile, "\\d{7}") %>% lubridate::parse_date_time(., orders = "Yj") %>% as.character()
    )
}, .progress = TRUE)

shapes2 <- shapes %>%
  do.call(rbind, .) %>%
  mutate(date = as.Date(date)) %>%
  inner_join(stations2, by = c("date" = "median_date"))


# Interpolate bathy -------------------------------------------------------

bb2 <- bb2 %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z)) %>%
  rename(x = xyz.est.x, y = xyz.est.y, z = xyz.est.z)

# Plot --------------------------------------------------------------------

p1 <- bb2 %>%
  ggplot(aes(x, y, fill = z, z = z)) +
  ggisoband::geom_isobands(bins = 25, color = NA) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    limits = c(-2500, 0),
    oob = scales::squish,
    guide = "legend",
    breaks = -c(0, 1000, 2000, 3000)
  ) +
  paletteer::scale_color_paletteer_d("RColorBrewer::Set1") +
  geom_sf(
    data = wm,
    size = 0.1,
    inherit.aes = FALSE,
    fill = "white"
  ) +
  geom_sf(
    data = shapes2,
    aes(color = transect),
    show.legend = FALSE,
    size = 0.1,
    inherit.aes = FALSE
  ) +
  coord_sf(xlim = c(-70.5, -43), ylim = c(65, 72)) +
  annotate(
    geom = "text",
    x = -63.5,
    y = 67.5,
    label = "Qikiqtarjuaq",
    vjust = -0.25,
    hjust = 0,
    size = 3,
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
    text_cex = 0.75
  ) +
  geom_point(
    data = stations,
    aes(
      x = longitude,
      y = latitude,
      shape = station_type,
      color = transect
    ),
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
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
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
    fill = guide_colorbar(
      barwidth = unit(2, "cm"),
      barheight = unit(0.1, "cm"),
      direction = "horizontal",
      title.position = "top"
    ),
    color = guide_legend(ncol = 2, override.aes = list(size = 1)),
    shape = guide_legend(ncol = 2, override.aes = list(size = 1))
  ) +
  annotate("point",
    x = -63.78953333,
    y = 67.47973333,
    size = 0.5
  )

# Float -------------------------------------------------------------------

files <- list.files("../../float_ice_detection/data/raw/floats/full_profiles/", full.names = TRUE)

tmpfile <- future_map(files, function(x) {
  load(x)

  tmpfile <- tempfile(fileext = ".feather")

  write_csv(dataFloat, tmpfile)

  return(tmpfile)
})

float <- future_map_dfr(tmpfile, read_csv) %>%
  janitor::clean_names() %>%
  rename(latitude = lat) %>%
  rename(longitude = lon) %>%
  rename(depth_m = pdbar) %>%
  mutate(date = as.Date(time)) %>%
  mutate(float = str_extract(profile_id, "(takapm\\d{3})\\w")) %>%
  dplyr::select(
    float,
    profile_id,
    date,
    longitude,
    latitude,
    depth_m,
    temperature,
    salinity,
    sea_ice
  ) %>%
  distinct()

float %>%
  distinct(profile_id, date, longitude, latitude, .keep_all = TRUE) %>%
  separate(profile_id, into = c("float", "a", "b", "c", "d"), sep = "_") %>%
  dplyr::select(-c(a:d))

# Calculate the duration of the deployment of each float

float <- float %>%
  group_by(float) %>%
  mutate(deployment_duration = difftime(max(date, na.rm = TRUE), min(date, na.rm = TRUE))) %>%
  ungroup() %>%
  arrange(desc(deployment_duration))

float %>%
  distinct(float, deployment_duration)

float %>%
  count(date, sort = TRUE)

# Looks like there are missing dates in the data

float %>%
  count(float, lubridate::year(date))

float %>%
  filter(is.na(date))

# Only use 2016

float <- float %>%
  filter(lubridate::year(date) == 2016)

# Glider ------------------------------------------------------------------

glider <- vroom::vroom("data/clean/gliders.csv")

glider %>%
  count(as.Date(date_time), sort = TRUE)

glider %>%
  count(filename)

glider <- glider %>%
  mutate(glider_id = str_extract(filename, "qala\\d"))

glider <- glider %>%
  group_by(filename, glider_id) %>%
  sample_frac(0.005) %>%
  arrange(time) %>%
  ungroup() %>%
  mutate(date = as.Date(date_time)) %>%
  dplyr::select(filename, glider_id, time, latitude, longitude, date)

# Transects ---------------------------------------------------------------

track <- st_read("../green_edge/data/doc.kml", layer = "Tracks")
sampling_dates <- read_csv("data/clean/greenedge_log_clean.csv")

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

# Float -------------------------------------------------------------------

p2 <- float %>%
  drop_na(date) %>%
  ggplot() +
  ggisoband::geom_isobands(
    data = bb2,
    aes(x, y, fill = z, z = z),
    bins = 25,
    color = NA,
    show.legend = FALSE
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
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
  ggnewscale::new_scale_color() +
  geom_point(aes(
    x = longitude,
    y = latitude,
    color = glue("{float}")
  ),
  size = 0.5
  ) +
  scale_colour_hue(
    l = 70, c = 30,
    guide_legend(title = "Float name", order = 3, override.aes = list(size = 1))
  ) +
  coord_sf(xlim = c(-70.5, -43), ylim = c(67.2, 71.5)) +
  geom_path(
    data = stations,
    aes(x = longitude, y = latitude, group = transect),
    lty = 2,
    size = 0.25,
    color = "#333333"
  ) +
  ggnewscale::new_scale_color() +
  geom_point(
    data = glider,
    aes(
      x = longitude,
      y = latitude,
      color = glider_id,
      group = filename
    ), size = 0.1
  ) +
  paletteer::scale_color_paletteer_d(
    "ggsci::alternating_igv",
    guide = guide_legend(title = "Glider name", order = 2, override.aes = list(size = 1))
  ) +
  xlab(NULL) +
  ylab(NULL) +
  annotate(
    geom = "text",
    x = -63.5,
    y = 67.5,
    label = "Qikiqtarjuaq",
    vjust = -0.25,
    hjust = 0,
    size = 3,
    family = "Poppins"
  ) +
  annotate("point",
    x = -63.78953333,
    y = 67.47973333,
    size = 0.5
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.08,
    height = unit(0.1, "cm"),
    line_width = 0.1,
    text_cex = 0.75
  ) +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(0.98, 0.98),
    legend.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "#B9DDF1"),
    legend.key = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 0, unit = "cm")
  )

# Inset -------------------------------------------------------------------

bbox <- c(xmin = -180, xmax = 0, ymin = 30, ymax = 90)

canada <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large") %>%
  st_crop(bbox)

# crsuggest::suggest_crs(canada)

bbox_nsidc <- canada %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_transform(3979) %>%
  st_bbox()

map_bbox <- st_bbox(c(xmin = -70.5, xmax = -43, ymin = 65, ymax = 72), crs = 4326) %>%
  st_as_sfc() %>%
  st_transform(3979)

p3 <- canada %>%
  ggplot() +
  geom_sf(size = 0.1) +
  geom_sf(data = map_bbox, fill = NA, color = "red", size = 0.5) +
  coord_sf(crs = 3979, xlim = c(-2000000, 2500000), ylim = c(4000000, 10)) +
  theme(
    text = element_text(size = 6)
  )

# Combine -----------------------------------------------------------------

p4 <- p2 +
  annotation_custom(ggplotGrob(p3), xmin = -52, xmax = -40, ymin = 67, ymax = 69.45)

p <- p1 / p4 +
  plot_annotation(tag_levels = "A")

filename <- "graphs/fig01.pdf"

ggsave(
  filename,
  device = cairo_pdf,
  height = 8,
  width = 6
)

knitr::plot_crop(filename)

pdftools::pdf_convert(filename,
  format = "png",
  filenames = "graphs/fig01.png",
  dpi = 300
)
