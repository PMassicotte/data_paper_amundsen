

# https://apps.ecmwf.int/datasets/data/interim-full-moda/levtype=sfc/

files <- fs::dir_ls("data/raw/wind/", regexp = "monthly")

df <-
  map_df(files, hyper_tibble) %>%
  mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude)) %>%
  filter(between(latitude, 65, 72) & between(longitude, -70.5, -43)) %>%
  mutate(date = as.POSIXct(time * 3600, origin = "1900-01-01")) %>%
  mutate(date = as.Date(date)) %>%
  filter(date %in% as.Date(c("2016-06-01", "2016-07-01"))) %>%
  mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE))

wm <-
  rnaturalearth::ne_download(
    scale = "large",
    type = "countries",
    returnclass = "sf"
  ) %>%
  as("Spatial")

p <- df %>%
  mutate(date2 = as.Date(date)) %>%
  mutate(ws = sqrt(u10^2 + v10^2)) %>%
  ggplot(aes(longitude, latitude, color = ws)) +
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
    v = v10
  ),
  size = 0.25,
  show.legend = TRUE
  ) +
  facet_wrap(~time) +
  coord_cartesian(xlim = c(-70.5, -43), ylim = c(65, 72)) +
  rcartocolor::scale_color_carto_c(
    palette = "OrYel",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"),
      keywidth = unit(8, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      label.position = "bottom",
      title = bquote("Wind speed" ~ (m ~ s^{
        -1
      })),
      override.aes = list(size = 2)
    )
  ) +
  scale_x_continuous(
    labels = function(x) {
      paste0(-x, "°W")
    },
    breaks = scales::pretty_breaks(n = 5)
  ) +
  scale_y_continuous(
    labels = function(x) {
      paste0(x, "°N")
    },
    breaks = scales::pretty_breaks(n = 5)
  ) +
  facet_wrap(~month, ncol = 1) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  "graphs/fig_wind.png",
  dpi = 600,
  width = 8.3,
  height = 8.3 / 0.6,
  units = "cm"
)

