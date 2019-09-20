bathy <-
  raster::brick("../../../../work/projects/transsiz/data/raw/IBCAO_V3_500m_RR2.tif")

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

bb <-
  raster("~/Downloads/IBCAO_V3_500m_RR_tif/IBCAO_V3_500m_RR.tif") %>%
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


# Plot --------------------------------------------------------------------

stations <-
  read_csv("/media/data4tb/greenedge/greenedge_stations_clean.csv") %>%
  distinct(station, longitude, latitude, .keep_all = TRUE) %>%
  filter(str_starts(station, "G")) %>%
  filter(station_type %in% c("Full", "Basic")) %>%
  mutate(transect = str_sub(station, 1, 2))

transect <- stations %>%
  group_by(transect) %>%
  filter(longitude == max(longitude))

p <- bb2 %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z)) %>%
  ggplot(aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z)) +
  ggisoband::geom_isobands(bins = 25, color = NA) +
  paletteer::scale_fill_paletteer_c(ggthemes, Blue, direction = -1, limits = c(-2500, 0), oob = scales::squish) +
  paletteer::scale_colour_paletteer_d(wesanderson, BottleRocket2) +
  geom_sf(
    data = wm,
    size = 0.1,
    inherit.aes = FALSE,
    fill = "gray95"
  ) +
  coord_sf(xlim = c(-70, -45), ylim = c(65, 72)) +
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
    aes(x = longitude, y = latitude, color = station_type),
    size = 2,
    inherit.aes = FALSE
  ) +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.98, 0.01),
    legend.background = element_rect(fill = "gray95"),
    panel.background = element_rect(fill = "#B9DDF1"),
    legend.key = element_rect(fill = "gray95"),
    panel.grid = element_blank()
  ) +
  labs(
    fill = "Depth (m)",
    color = "Station"
  ) +
  geom_mark_ellipse(
    data = stations,
    aes(
      x = longitude,
      y = latitude,
      group = transect
    ),
    inherit.aes = FALSE,
    expand = unit(1, "mm"),
    size = 0.25,
    con.size = 0.25,
    label.buffer = unit(7, "mm"),
    label.fontsize = 8,
    tol = 0.001
  ) +
  ggrepel::geom_text_repel(
    data = transect,
    aes(x = longitude, y = latitude, label = transect),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 2,
    size = 3
  ) +
  guides(
    fill = guide_colorbar(barwidth = unit(0.25, "cm"))
  )

ggsave(
  "graphs/fig01.pdf",
  device = cairo_pdf,
  width = 8.96,
  height = 7.29
)
