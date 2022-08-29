rm(list = ls())

source("R/utils.R")

# CTD ---------------------------------------------------------------------

ctd <- vroom::vroom("data/clean/greenedge_ctd.csv", delim = ",") %>%
  filter(mission == "amundsen_2016") %>%
  filter(str_starts(station, "G")) %>%
  mutate(transect = parse_number(station) %/% 100 * 100) %>%
  filter(transect %in% c(300, 500)) %>%
  filter(depth_m <= 200) %>%
  mutate(source = "ctd")

ctd %>%
  distinct(transect)

# Interpolate data on regular grid ----------------------------------------

ctd <- ctd %>%
  select(
    source,
    date,
    latitude,
    longitude,
    transect,
    depth_m,
    te90_degrees_c,
    asal_g_kg,
    flor_mg_m3,
    tran_percent,
    cdom_mg_m3
  ) %>%
  pivot_longer(-c(source:depth_m),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_nest(source, transect, variable) %>%
  mutate(variable_interpolated = map(
    data,
    ~ interpolate_to_tibble(., longitude, depth_m, value, n = 100)
  )) %>%
  select(-data) %>%
  unnest(c(variable_interpolated)) %>%
  pivot_wider(names_from = variable, values_from = value)

# Rename to match mvp names -----------------------------------------------

ctd <- ctd %>%
  select(
    source,
    transect,
    longitude,
    pres = depth_m,
    fluo = flor_mg_m3,
    sal = asal_g_kg,
    temp = te90_degrees_c,
    trans = tran_percent,
    cdom_mg_m3
  )

# MVP ---------------------------------------------------------------------

mvp <- vroom::vroom("data/clean/greenedge_mvp.csv", altrep = TRUE) %>%
  mutate(initial_longitude_deg = -initial_longitude_deg) %>%
  filter(pres <= 200)

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
  )) %>%
  filter(transect %in% c(300, 500)) %>%
  add_column(source = "mvp", .before = 1)

# Interpolate -------------------------------------------------------------

mvp <- mvp %>%
  select(source, date, longitude = initial_longitude_deg, latitude = initial_latitude_deg, transect, pres, temp, sal, fluo, trans) %>%
  pivot_longer(-c(source:pres), names_to = "variable", values_to = "value") %>%
  group_nest(source, transect, variable) %>%
  mutate(variable_interpolated = map(data, ~ interpolate_to_tibble(., longitude, pres, value, n = 100))) %>%
  select(-data) %>%
  unnest(c(variable_interpolated)) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  drop_na(transect)

# Merge both CTD and MVP --------------------------------------------------

df <- bind_rows(ctd, mvp)

df

# Order facets ------------------------------------------------------------

df <- df %>%
  mutate(transect = fct_relevel(as.character(transect), c("500", "300")))

# Plot --------------------------------------------------------------------

# Transect 500

p_t500_temp <- df %>%
  filter(transect == 500) %>%
  ggplot(aes(x = longitude, y = pres, fill = temp, z = temp)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W"), breaks = scales::pretty_breaks(n = 3)) +
  scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1)) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~source, ncol = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "none",
    plot.title = element_text(vjust = -18),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  ylab("Depth (m)") +
  labs(
    fill = "Temperature (°C)"
  ) +
  xlab("Longitude")

p_t500_sal <- df %>%
  filter(transect == 500) %>%
  ggplot(aes(x = longitude, y = pres, fill = sal, z = sal)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W"), breaks = scales::pretty_breaks(n = 3)) +
  scale_fill_gradientn(colours = color, limits = c(33, 34), oob = scales::squish, breaks = seq(33, 34, by = 0.5), guide = guide_colorbar(title.position = "top", nrow = 1)) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~source, ncol = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "none",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  ylab("Depth (m)") +
  labs(
    fill = bquote("Salinity" ~ (g ~ kg^{
      -1
    }))
  ) +
  xlab("Longitude")

p_t500_fluo <- df %>%
  filter(transect == 500) %>%
  ggplot(aes(x = longitude, y = pres, fill = fluo, z = fluo)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W"), breaks = scales::pretty_breaks(n = 3)) +
  scale_fill_gradientn(
    colours = color,
    limits = c(0, 10),
    oob = scales::squish,
    guide = guide_colorbar(title.position = "top", nrow = 1)
  ) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~source, strip.position = "top", ncol = 1) +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "none",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  ylab("Depth (m)") +
  labs(
    fill = bquote("Chlorophyll-a" ~ (mg ~ m^{
      -3
    }))
  ) +
  xlab("Longitude")

p_t500_trans <- df %>%
  filter(transect == 500) %>%
  ggplot(aes(x = longitude, y = pres, fill = trans, z = trans)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W"), breaks = scales::pretty_breaks(n = 3)) +
  scale_fill_gradientn(colours = color, limits = c(80, 100), oob = scales::squish, guide = guide_colorbar(title.position = "top", nrow = 1)) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~source, strip.position = "top", ncol = 1) +
  ylab("Depth (m)") +
  xlab("Longitude") +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "none",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  labs(
    fill = "Transmittance (%)"
  ) +
  xlab("Longitude")

p_t500_cdom <- df %>%
  filter(transect == 500) %>%
  ggplot(aes(x = longitude, y = pres, fill = cdom_mg_m3, z = cdom_mg_m3)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W"), breaks = scales::pretty_breaks(n = 3)) +
  scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1)) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~source, ncol = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "none",
    plot.title = element_text(vjust = -18),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  ylab("Depth (m)") +
  labs(
    fill = quote("CDOM" ~ (mg ~ m^{
      -3
    }))
  ) +
  xlab("Longitude")

# Transect 300

p_t300_temp <- df %>%
  filter(transect == 300) %>%
  ggplot(aes(x = longitude, y = pres, fill = temp, z = temp)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W")) +
  scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1, barwidth = unit(5, "cm"))) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~source, ncol = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    plot.title = element_text(vjust = -18),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  ylab("Depth (m)") +
  labs(
    fill = bquote("Temperature" ~ "(°C)")
  ) +
  xlab("Longitude")

p_t300_sal <- df %>%
  filter(transect == 300) %>%
  ggplot(aes(x = longitude, y = pres, fill = sal, z = sal)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W")) +
  scale_fill_gradientn(colours = color, limits = c(33, 34), oob = scales::squish, breaks = seq(33, 34, by = 0.5), guide = guide_colorbar(title.position = "top", nrow = 1, barwidth = unit(5, "cm"))) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~source, ncol = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  ylab("Depth (m)") +
  labs(
    fill = bquote("Salinity" ~ (g ~ kg^{
      -1
    }))
  ) +
  xlab("Longitude")

p_t300_fluo <- df %>%
  filter(transect == 300) %>%
  ggplot(aes(x = longitude, y = pres, fill = fluo, z = fluo)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W")) +
  scale_fill_gradientn(
    colours = color,
    limits = c(0, 10),
    oob = scales::squish,
    guide = guide_colorbar(title.position = "top", nrow = 1, barwidth = unit(5, "cm"))
  ) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~source, strip.position = "top", ncol = 1) +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  ylab("Depth (m)") +
  labs(
    fill = bquote("Chlorophyll-a" ~ (mg ~ m^{
      -3
    }))
  ) +
  xlab("Longitude")

p_t300_trans <- df %>%
  filter(transect == 300) %>%
  ggplot(aes(x = longitude, y = pres, fill = trans, z = trans)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W")) +
  scale_fill_gradientn(colours = color, limits = c(80, 100), oob = scales::squish, guide = guide_colorbar(title.position = "top", nrow = 1, barwidth = unit(5, "cm"))) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~source, strip.position = "top", ncol = 1) +
  ylab("Depth (m)") +
  xlab("Longitude") +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  labs(
    fill = bquote("Transmittance" ~ ("%"))
  ) +
  xlab("Longitude")

p_t300_cdom <- df %>%
  filter(transect == 300) %>%
  ggplot(aes(x = longitude, y = pres, fill = cdom_mg_m3, z = cdom_mg_m3)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W"), breaks = scales::pretty_breaks(n = 3)) +
  scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1, barwidth = unit(5, "cm"))) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~source, ncol = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    plot.title = element_text(vjust = -18),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  ylab("Depth (m)") +
  labs(
    fill = quote("CDOM" ~ (mg ~ m^{
      -3
    }))
  ) +
  xlab("Longitude")

row_1 <- p_t500_temp + p_t500_sal + p_t500_fluo + p_t500_trans + p_t500_cdom +
  plot_layout(nrow = 1)

row_2 <- p_t300_temp + p_t300_sal + p_t300_fluo + p_t300_trans + p_t300_cdom +
  plot_layout(nrow = 1)

p <- row_1 / row_2 +
  plot_layout(nrow = 2)

# SIC ---------------------------------------------------------------------

sic <- read_csv("data/clean/sic_transects_300_500.csv")

p_sic_t300 <- sic %>%
  filter(transect == 300) %>%
  ggplot(aes(x = longitude, y = sea_ice_concentration)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W")) +
  xlab(NULL) +
  ylab("SIC (%)") +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_blank()
  )

p_sic_t500 <- sic %>%
  filter(transect == 500) %>%
  ggplot(aes(x = longitude, y = sea_ice_concentration)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W")) +
  xlab(NULL) +
  ylab("SIC (%)") +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_blank()
  )

p1 <-
  {
    (p_sic_t500 + labs(title = "Transect 500")) + p_sic_t500 + p_sic_t500 + p_sic_t500 + p_sic_t500
  } + plot_layout(ncol = 5) -
  {
    p_t500_temp + p_t500_sal + p_t500_fluo + p_t500_trans + p_t500_cdom + plot_layout(ncol = 5)
  } +
  plot_layout(nrow = 2, heights = c(0.15, 0.75))

p2 <-
  {
    (p_sic_t300 + labs(title = "Transect 300")) + p_sic_t300 + p_sic_t300 + p_sic_t300 + p_sic_t300
  } + plot_layout(ncol = 5) -
  {
    p_t300_temp + p_t300_sal + p_t300_fluo + p_t300_trans + p_t300_cdom + plot_layout(ncol = 5)
  } +
  plot_layout(nrow = 2, heights = c(0.15, 0.75))

p <- p1 / p2 +
  plot_layout(nrow = 2)

ggsave(
  "graphs/fig08.pdf",
  device = cairo_pdf,
  width = 14,
  height = 12,
  units = "in"
)

knitr::plot_crop("graphs/fig08.pdf")
