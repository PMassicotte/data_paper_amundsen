# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# MVP 2D transect visualization.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/utils.R")

mvp <- vroom::vroom("/media/data4tb/greenedge/clean/mvp/greenedge_mvp.csv", altrep_opts = TRUE) %>%
  mutate(initial_longitude_deg = -initial_longitude_deg) %>%
  filter(pres <= 100)

glimpse(mvp)

mvp %>%
  distinct(cruise_number, section_number, mission)

mvp %>%
  count(cast_number, section_number, pres, sort = TRUE)

mvp %>%
  filter(section_number == "2016001_01") %>%
  ggplot(aes(x = fluo, y = pres, color = factor(cast_number))) +
  geom_path() +
  scale_y_reverse()

# Check transect ----------------------------------------------------------

mvp %>%
  distinct(initial_longitude_deg, initial_latitude_deg, .keep_all = TRUE) %>%
  ggplot(aes(x = initial_longitude_deg, y = initial_latitude_deg, color = section_number)) +
  geom_point() +
  paletteer::scale_color_paletteer_d(rcartocolor, Vivid)

# Recode the transect number
mvp <- mvp %>%
  mutate(transect = case_when(
    section_number %in% c("2016001_01", "2016001_02") ~ 100,
    section_number %in% c("2016001_05") ~ 400,
    section_number %in% c("2016001_04") ~ 300,
    section_number %in% c("2016001_08", "2016001_09") ~ 600,
    section_number %in% c("2016001_06", "2016001_07") ~ 500,
    section_number %in% c("2016001_03") ~ 200,
    TRUE ~ NA_real_
  ))

mvp %>%
  distinct(initial_longitude_deg, initial_latitude_deg, .keep_all = TRUE) %>%
  ggplot(aes(x = initial_longitude_deg, y = initial_latitude_deg, color = factor(transect))) +
  geom_point() +
  paletteer::scale_color_paletteer_d(rcartocolor, Vivid)

# Interpolate -------------------------------------------------------------

mvp <- mvp %>%
  select(date, longitude = initial_longitude_deg, latitude = initial_latitude_deg, transect, pres, temp, sal, fluo, trans) %>%
  pivot_longer(-c(date:pres), names_to = "variable", values_to = "value") %>%
  group_nest(transect, variable) %>%
  mutate(variable_interpolated = map(data, ~interpolate_to_tibble(., longitude, pres, value, n = 100))) %>%
  select(-data) %>%
  unnest(c(variable_interpolated)) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  drop_na(transect)

# Order facets ------------------------------------------------------------

df <- df %>%
  mutate(transect = fct_relevel(as.character(transect), c("600", "500", "700", "300", "200", "100", "400")))


# Plot --------------------------------------------------------------------

p1 <- mvp %>%
  ggplot(aes(x = longitude, y = pres, fill = temp, z = temp)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1)) +
  geom_isobands(color = NA, bins = 100) +
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

p2 <- mvp %>%
  ggplot(aes(x = longitude, y = pres, fill = sal, z = sal)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = color, limits = c(33, 34), oob = scales::squish, breaks = seq(33, 34, by = 0.5), guide = guide_colorbar(title.position = "top", nrow = 1)) +
  geom_isobands(color = NA, bins = 100) +
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

p3 <- mvp %>%
  ggplot(aes(x = longitude, y = pres, fill = fluo, z = fluo)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colours = color,
    limits = c(0, 10),
    oob = scales::squish,
    , guide = guide_colorbar(title.position = "top", nrow = 1)
  ) +
  geom_isobands(color = NA, bins = 100) +
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

p4 <- mvp %>%
  ggplot(aes(x = longitude, y = pres, fill = trans, z = trans)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = color, limits = c(80, 100), oob = scales::squish, guide = guide_colorbar(title.position = "top", nrow = 1)) +
  geom_isobands(color = NA, bins = 100) +
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

ggsave("graphs/fig_mvp.pdf", device = cairo_pdf, width = 12, height = 12, units = "in")

