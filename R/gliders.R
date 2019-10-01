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

# files <- fs::dir_ls("/media/data4tb/greenedge/DATA/not-on-cyber-yet/gliders/NetCDF/", glob = "*.nc")
#
# df <- future_map(files, tidync::hyper_tibble, .progress = TRUE) %>%
#   set_names(basename(files)) %>%
#   bind_rows(.id = "filename") %>%
#   janitor::clean_names() %>%
#   mutate(date_time = lubridate::as_datetime(time))
#
# data.table::fwrite(df, "data/clean/gliders.csv")

df <- vroom::vroom("data/clean/gliders.csv")

df %>%
  count(as.Date(date_time), sort = TRUE)

# Geographic positions ----------------------------------------------------

set.seed(2019)

# Overview of the data, so I can choose a longitudinal transect
df %>%
  group_by(filename) %>%
  sample_frac(0.0001) %>%
  ggplot() +
  geom_sf(data = wm, size = 0.1, inherit.aes = FALSE) +
  geom_point(aes(x = longitude, y = latitude, color = filename)) +
  coord_sf(xlim = c(-65, -55), ylim = c(68, 70)) +
  facet_wrap(~filename)

# Visualization -----------------------------------------------------------

p1 <- df %>%
  group_by(filename) %>%
  sample_frac(0.0001) %>%
  mutate(date = as.numeric(as.Date(date_time))) %>%
  ggplot() +
  geom_sf(data = wm, size = 0.1, inherit.aes = FALSE) +
  geom_point(aes(x = longitude, y = latitude, color = date)) +
  geom_mark_ellipse(
    expand = unit(1, "mm"),
    size = 0.25,
    aes(
      x = longitude,
      y = latitude,
      filter = str_detect(filename, "2.3|2.4|2.5|2.6")
    )
  ) +
  coord_sf(xlim = c(-70.5, -43), ylim = c(65, 72)) +
  scale_color_viridis_c(labels = function(x) lubridate::as_date(x)) +
  labs(
    color = "Date of\ndeployment"
  ) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.98, 0.01),
    axis.title = element_blank(),
    legend.background = element_rect(fill = "gray90")
  ) +
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
  annotate("point", x = -63.78953333, y = 67.47973333, size = 0.5)

df_viz <- df %>%
  filter(between(pres, 0, 100)) %>%
  filter(str_detect(filename, "2.3|2.4|2.5|2.6")) %>%
  filter(temp_qc %in% 0:2 | fluorescence_chla_qc %in% 0:2 | psal %in% 0:2) %>%
  select(time, latitude, longitude, pres, temp, molar_nitrate, psal) %>%
  pivot_longer(-c(time:pres), names_to = "variable", values_to = "value") %>%
  drop_na(latitude, longitude, value) %>%
  group_nest(variable) %>%
  mutate(variable_interpolated = map(data, ~interpolate_to_tibble(., longitude, pres, value, n = 200))) %>%
  select(-data) %>%
  unnest(c(variable_interpolated)) %>%
  pivot_wider(names_from = variable, values_from = value)

p2 <- df_viz %>%
  ggplot(aes(x = longitude, y = pres, fill = temp, z = temp)) +
  geom_isobands(bins = 50, color = NA) +
  scale_fill_gradientn(colours = color) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    panel.spacing = unit(1, "lines")
  ) +
  ylab("Depth (m)") +
  labs(
    fill = "Temperature (°C)"
  )

p3 <- df_viz %>%
  ggplot(aes(x = longitude, y = pres, fill = psal, z = psal)) +
  geom_isobands(breaks = seq(30, 34.75, by = 0.05), color = NA) +
  scale_fill_gradientn(
    colours = color,
    limits = c(32, 34),
    oob = scales::squish,
    breaks = seq(30, 34, by = 0.5)
  ) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    panel.spacing = unit(1, "lines")
  ) +
  ylab("Depth (m)") +
  labs(
    fill = bquote("Salinity"~(g~kg^{-1}))
  )

p4 <- df_viz %>%
  ggplot(aes(x = longitude, y = pres, fill = molar_nitrate, z = molar_nitrate)) +
  geom_isobands(bins = 50, color = NA) +
  scale_fill_gradientn(colours = color) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    panel.spacing = unit(1, "lines")
  ) +
  ylab("Depth (m)") +
  labs(
    fill = bquote(TODO)
  )

p <- p1 + (p2 / p3 / p4) +
  plot_layout(widths = c(2, 1), heights = c(4, 2))

ggsave("graphs/fig05.pdf", device = cairo_pdf, width = 16, height = 8)
