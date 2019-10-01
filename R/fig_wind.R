

# https://apps.ecmwf.int/datasets/data/interim-full-moda/levtype=sfc/

files <- fs::dir_ls("data/raw/wind/")

df <-
  map_df(files, hyper_tibble) %>%
  mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude)) %>%
  filter(between(latitude, 65, 72) & between(longitude, -70.5, -43)) %>%
  mutate(date = as.POSIXct(time * 3600, origin = "1900-01-01")) %>%
  mutate(date = as.Date(date)) %>%
  filter(date %in% as.Date(c("2016-06-15", "2016-07-01")))

wm <-
  rnaturalearth::ne_download(
    scale = "large",
    type = "countries",
    returnclass = "sf"
  ) %>%
  as("Spatial")

p <- df %>%
  mutate(date2 = as.Date(date)) %>%
  ggplot(aes(longitude, latitude)) +
  geom_polygon(
    data = wm,
    aes(x = long, y = lat, group = group),
    inherit.aes = FALSE,
    fill = "gray95",
    color = alpha("black", 0.2),
    size = 0.1
  ) +
  geom_quiver(aes(
    x = longitude,
    y = latitude,
    u = u10,
    v = v10,
  ),
  size = 0.1) +
  facet_wrap(~time) +
  coord_cartesian(xlim = c(-70.5, -43), ylim = c(65, 72)) +
  scale_fill_viridis_c() +
  facet_wrap(~date2, ncol = 1) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  "graphs/fig_wind.pdf",
  device = cairo_pdf,
  width = 8.3,
  height = 8.3 / 0.6,
  units = "cm"
)
