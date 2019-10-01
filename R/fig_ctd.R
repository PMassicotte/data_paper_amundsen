# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Figure for CTD transects.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/utils.R")

df <- vroom::vroom("/media/data4tb/greenedge/clean/ctd/greenedge_ctd.csv", delim = ",") %>%
  filter(mission == "amundsen_2016") %>%
  filter(str_starts(station, "G")) %>%
  mutate(transect = parse_number(station) %/% 100 * 100) %>%
  filter(transect <= 700) %>%
  filter(depth_m <= 200)

df %>%
  distinct(transect)

# Interpolate data on regular grid ----------------------------------------

df <- df %>%
  select(date, latitude, longitude, transect, depth_m, te90_degrees_c, asal_g_kg, flor_mg_m3, tran_percent) %>%
  pivot_longer(-c(date:depth_m), names_to = "variable", values_to = "value") %>%
  group_nest(transect, variable) %>%
  mutate(variable_interpolated = map(data, ~interpolate_to_tibble(., longitude, depth_m, value, n = 100))) %>%
  select(-data) %>%
  unnest(c(variable_interpolated)) %>%
  pivot_wider(names_from = variable, values_from = value)

# Order facets ------------------------------------------------------------

df <- df %>%
  mutate(transect = fct_relevel(as.character(transect), c("600", "500", "700", "300", "200", "100", "400")))

# Plot --------------------------------------------------------------------

p1 <- df %>%
  ggplot(aes(x = longitude, y = depth_m, fill = te90_degrees_c, z = te90_degrees_c)) +
  # geom_raster() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1)) +
  geom_isobands(bins = 50, color = NA) +
  facet_wrap(~transect, scales = "free_x", ncol = 1) +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "top"
  ) +
  ylab("Depth (m)") +
  labs(
    fill = "Temperature (°C)"
  ) +
  xlab("Longitude")


p2 <- df %>%
  ggplot(aes(x = longitude, y = depth_m, fill = asal_g_kg, z = asal_g_kg)) +
  # geom_raster() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = color,
    limits = c(33, 34.75),
    oob = scales::squish,
    breaks = seq(33, 34.75, by = 0.5),
    , guide = guide_colorbar(title.position = "top", nrow = 1)
  ) +
  geom_isobands(breaks = seq(30, 34.75, by = 0.05), color = NA) +
  facet_wrap(~transect, scales = "free_x", ncol = 1) +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "top"
  ) +
  ylab("Depth (m)") +
  labs(
    fill = bquote("Salinity"~(g~kg^{-1}))
  ) +
  xlab("Longitude")

p3 <- df %>%
  mutate(flor_mg_m3 = ifelse(flor_mg_m3 < 0, 0, flor_mg_m3)) %>%
  ggplot(aes(x = longitude, y = depth_m, fill = flor_mg_m3, z = flor_mg_m3)) +
  # geom_raster() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = color,
    limits = c(0, 8),
    oob = scales::squish,
    guide = guide_colorbar(title.position = "top", nrow = 1)
  ) +
  geom_isobands(bins = 100, color = NA) +
  facet_wrap(~transect, scales = "free_x", ncol = 1) +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "top"
  ) +
  ylab("Depth (m)") +
  labs(
    fill = "Fluorescence"
  ) +
  xlab("Longitude")

p4 <- df %>%
  ggplot(aes(x = longitude, y = depth_m, fill = tran_percent, z = tran_percent)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_isobands(bins = 100, color = NA) +
  scale_fill_gradientn(colours = color,
    limits = c(85, NA),
    oob = scales::squish,
    guide = guide_colorbar(title.position = "top", nrow = 1)
  ) +
  facet_wrap(~transect, scales = "free_x", ncol = 1) +
  ylab("Depth (m)") +
  xlab("Longitude") +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "top"
  ) +
  labs(
    fill = "Transmittance (%)"
  ) +
  xlab("Longitude")

p <- p1 + p2 + p3 + p4 +
  plot_layout(ncol = 4)

ggsave("graphs/fig_ctd.pdf", device = cairo_pdf, width = 12, height = 12, units = "in")
