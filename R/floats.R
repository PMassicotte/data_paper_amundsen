rm(list = ls())

source("R/utils.R")

float <- feather::read_feather("/mnt/nfs/scratch/pmassicotte/isa/data/clean/float.feather") %>%
  extract(profile_id, into = c("float", "ff"), regex = "(taka[:alnum:]*)_(.*)")

float %>%
  distinct(float)


# Floats positions --------------------------------------------------------

wm <-
  rnaturalearth::ne_download(
    scale = "large",
    type = "countries",
    returnclass = "sf"
  )


p_geo <- float %>%
  distinct(float, date, longitude, latitude) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point() +
  geom_sf(data = wm, inherit.aes = FALSE, size = 0.1) +
  coord_sf(xlim = c(-70.5, -43), ylim = c(66, 75))

p_geo

p_geo <- p_geo +
  facet_wrap(~float, ncol = 2)

# First visualization of the raw data -------------------------------------

p_ts <- float %>%
  # filter(float == "takapm016b") %>%
  filter(depth_m <= 100) %>%
  mutate(date2 = lubridate::yday(date)) %>%
  ggplot(aes(x = date, y = depth_m, color = temperature)) +
  geom_point() +
  scale_y_reverse() +
  scale_color_gradientn(colours = color) +
  facet_wrap(~float, scales = "free", ncol = 2)

p_ts

# Find floats that have not moved a lot spatially. We can use these for time
# series.
p_geo + p_ts

# More visualization ------------------------------------------------------

df_pts <-
  float %>%
  filter(float %in% c("takapm005b", "takapm011b", "takapm013b", "takapm015b")) %>%
  filter(depth_m <= 100) %>%
  mutate(date2 = lubridate::yday(date))

df_viz <- float %>%
  filter(float %in% c("takapm005b", "takapm011b", "takapm013b", "takapm015b")) %>%
  filter(depth_m <= 100) %>%
  mutate(date2 = lubridate::yday(date)) %>%
  pivot_longer(c(temperature, salinity), names_to = "variable", values_to = "value") %>%
  group_nest(float, variable) %>%
  mutate(res = map(data, ~interpolate_to_tibble(., date2, depth_m, value, n = 100))) %>%
  select(-data) %>%
  unnest(c(res)) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(float = fct_relevel(float, c("takapm005b", "takapm011b", "takapm013b", "takapm015b")))

p_temp <- df_viz %>%
  ggplot(aes(x = date2, y = depth_m, fill = temperature, z = temperature)) +
  geom_isobands(bins = 25, color = NA) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4), labels = function(x) as.Date(paste0("2016-", round(x)), "%Y-%j") %>% format("%b-%d"), expand = c(0, 0)) +
  facet_wrap(~float, scales = "free_x", ncol = 1) +
  scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1)) +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "top"
  ) +
  ylab("Depth (m)") +
  labs(
    fill = "Temperature (°C)"
  ) +
  xlab("Date")

p_sal <- df_viz %>%
  ggplot(aes(x = date2, y = depth_m, fill = salinity, z = salinity)) +
  geom_isobands(bins = 25, color = NA) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4), labels = function(x) as.Date(paste0("2016-", round(x)), "%Y-%j") %>% format("%b-%d"), expand = c(0, 0)) +
  facet_wrap(~float, scales = "free_x", ncol = 1) +
  scale_fill_gradientn(colours = color, breaks = seq(20, 34, by = 1), guide = guide_colorbar(title.position = "top", nrow = 1)) +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "top"
  ) +
  ylab("Depth (m)") +
  labs(
    fill = bquote("Salinity"~(g~kg^{-1}))
  ) +
  xlab("Date")

p <- p_temp + p_sal
ggsave("graphs/fig_floats_data.pdf", device = cairo_pdf, width = 6, height = 12, units = "in")


# Map ---------------------------------------------------------------------

p_map <- float %>%
  distinct(float, date, longitude, latitude) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(size = 0.01, color = "gray75") +
  geom_sf(data = wm, inherit.aes = FALSE, size = 0.1) +
  coord_sf(xlim = c(-71, -50), ylim = c(68, 74.5)) +
  geom_point(
    data = float %>% distinct(float, longitude, latitude) %>% filter(float %in% c("takapm005b", "takapm011b", "takapm013b", "takapm015b")),
    aes(color = float),
    size = 0.01
  ) +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(0.98, 0.99),
    legend.text = element_text(size = 6),
    legend.title = element_blank(),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 0, unit = "cm"),
    legend.background = element_rect(fill = "gray90"),
    legend.key = element_rect(fill = "gray90"),
    panel.grid = element_blank()
  )

ggsave("graphs/fig_floats_map.pdf", device = cairo_pdf, width = 8.3, height = 8.3, units = "cm")
