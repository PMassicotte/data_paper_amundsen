# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Floats data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/utils.R")

wm <- rnaturalearth::ne_download(
  scale = "large",
  type = "countries",
  returnclass = "sf"
)

# Import float data -------------------------------------------------------

files <- list.files("../../float_ice_detection/data/raw/floats/full_profiles/", full.names = TRUE)

tmpfile <- future_map(files, function(x) {
  load(x)

  tmpfile <- tempfile(fileext = ".feather")

  write_csv(dataFloat, tmpfile)

  return(tmpfile)
})


df <- future_map_dfr(tmpfile, read_csv) %>%
  janitor::clean_names() %>%
  rename(latitude = lat) %>%
  rename(longitude = lon) %>%
  rename(depth_m = pdbar) %>%
  mutate(date = as.Date(time)) %>%
  select(profile_id, date, longitude, latitude, depth_m, temperature, salinity, sea_ice) %>%
  distinct()

df %>%
  distinct(profile_id, date, longitude, latitude, .keep_all = TRUE) %>%
  separate(profile_id, into = c("float", "a", "b", "c", "d"), sep = "_") %>%
  select(-c(a:d))


# Calculate the duration of the deployment of each float

df <- df %>%
  group_by(float) %>%
  mutate(deployment_duration = difftime(max(date, na.rm = TRUE), min(date, na.rm = TRUE))) %>%
  ungroup() %>%
  arrange(desc(deployment_duration))

df %>%
  distinct(float, deployment_duration)

# %>%
#   filter(dense_rank(desc(deployment_duration)) <= 3) %>%
#   distinct(float)

df %>%
  count(date, sort = TRUE)

# Looks like there are missing dates in the data

df %>%
  count(float, lubridate::year(date))

df %>%
  filter(is.na(date))

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

p <- df %>%
  drop_na(date) %>%
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
  geom_point(aes(
    x = longitude,
    y = latitude,
    shape = factor(lubridate::year(date)),
    color = glue("{float} ({as.numeric(deployment_duration, units = 'days')} days)")
    # color = as.numeric(deployment_duration, units = "days")
  ),
  size = 0.5
  ) +
  coord_sf(xlim = c(-75, -43), ylim = c(62, 75)) +
  # scale_color_viridis_d(option = "A") +
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
  labs(
    shape = "Year",
    color = "Float name"
  ) +
  guides(
    # fill = guide_colorbar(barwidth = unit(2, "cm"), barheight = unit(0.1, "cm"), direction = "horizontal", title.position = "top"),
    # color = guide_colorbar(override.aes = list(size = 1), barwidth = unit(2, "cm"), barheight = unit(0.1, "cm"), direction = "horizontal", title.position = "top"),
    shape = guide_legend(ncol = 2, override.aes = list(size = 1, alpha = 1)),
    color = guide_legend(label.theme = element_text(size = 4))
  )

ggsave(
  "graphs/fig_map_floats.pdf",
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
