rm(list = ls())

source("R/utils.R")


# CTD ---------------------------------------------------------------------

hplc <- vroom::vroom("data/clean/greenedge_pigments.csv", delim = ",") %>%
  filter(mission == "amundsen_2016") %>%
  filter(str_starts(station, "G")) %>%
  mutate(transect = parse_number(station) %/% 100 * 100) %>%
  filter(transect %in% c(300, 500)) %>%
  filter(depth_m <= 200) %>%
  filter(method == "HPLC") %>%
  tibble::rowid_to_column() %>%
  select(rowid, date, latitude, longitude, transect, depth_m, pigment, conc_mg_m3)

hplc %>%
  distinct(transect)

# Interpolate data on regular grid ----------------------------------------

hplc <- hplc %>%
  pivot_wider(names_from = pigment, values_from = conc_mg_m3) %>%
  rowwise() %>%
  mutate(diadinoxanthin_diatoxanthin = sum(Diadinoxanthin, Diatoxanthin, na.rm = TRUE)) %>%
  pivot_longer(-c(rowid:depth_m), names_to = "pigment", values_to = "conc_mg_m3") %>%
  filter(
    pigment %in% c("sum Phbd a", "Zeaxanthin", "Total Chlorophyll a", "diadinoxanthin_diatoxanthin")
  ) %>%
  drop_na()

hplc_viz <- hplc %>%
  group_nest(transect, pigment) %>%
  mutate(variable_interpolated = map(data, ~ interpolate_to_tibble(., longitude, depth_m, conc_mg_m3, n = 100))) %>%
  select(-data) %>%
  unnest(c(variable_interpolated)) %>%
  pivot_wider(names_from = pigment, values_from = conc_mg_m3) %>%
  janitor::clean_names()


# Plot --------------------------------------------------------------------

hplc_viz

# p1 <- hplc_viz %>%
#   drop_na(zeaxanthin) %>%
#   ggplot(aes(x = longitude, y = depth_m, fill = zeaxanthin, z = zeaxanthin)) +
#   scale_y_reverse(expand = c(0, 0)) +
#   scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W"), breaks = scales::pretty_breaks(n = 3)) +
#   scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1)) +
#   geom_isobands(color = NA, bins = 100) +
#   facet_wrap(~transect, nrow = 1, strip.position = "top") +
#   theme(
#     panel.spacing = unit(1, "lines"),
#     plot.title = element_text(vjust = -18),
#     legend.margin = margin(0, 0, 0, 0),
#     legend.box.margin = margin(0, 0, 30, 0)
#   ) +
#   ylab("Depth (m)") +
#   xlab("Longitude")

p1 <- hplc_viz %>%
  drop_na(total_chlorophyll_a) %>%
  ggplot(aes(x = longitude, y = depth_m, fill = total_chlorophyll_a, z = total_chlorophyll_a)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W"), breaks = scales::pretty_breaks(n = 3)) +
  scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1, barwidth = unit(5, "cm"))) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~transect, nrow = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(vjust = -18),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  ylab("Depth (m)") +
  xlab("Longitude") +
  labs(
    fill = bquote("Chla"~(mg~m^{-3}))
  )

p2 <- hplc_viz %>%
  drop_na(sum_phbd_a) %>%
  ggplot(aes(x = longitude, y = depth_m, fill = sum_phbd_a, z = sum_phbd_a)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W"), breaks = scales::pretty_breaks(n = 3)) +
  scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1, barwidth = unit(5, "cm"))) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~transect, nrow = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(vjust = -18),
    legend.margin = margin(0, 0, 0, 0),
    legend.position = "bottom",
    legend.box.margin = margin(0, 0, 30, 0)
  ) +
  ylab("Depth (m)") +
  xlab("Longitude")

# p3 <- hplc_viz %>%
#   drop_na(diadinoxanthin_diatoxanthin) %>%
#   ggplot(aes(x = longitude, y = depth_m, fill = diadinoxanthin_diatoxanthin, z = diadinoxanthin_diatoxanthin)) +
#   scale_y_reverse(expand = c(0, 0)) +
#   scale_x_continuous(expand = c(0, 0), labels = function(x) paste0(-x, "°W"), breaks = scales::pretty_breaks(n = 3)) +
#   scale_fill_gradientn(colours = color, guide = guide_colorbar(title.position = "top", nrow = 1)) +
#   geom_isobands(color = NA, bins = 100) +
#   facet_wrap(~transect, nrow = 1, strip.position = "top") +
#   theme(
#     panel.spacing = unit(1, "lines"),
#     plot.title = element_text(vjust = -18),
#     legend.margin = margin(0, 0, 0, 0),
#     legend.box.margin = margin(0, 0, 30, 0)
#   ) +
#   ylab("Depth (m)") +
#   xlab("Longitude")


# Zooplankton -------------------------------------------------------------

zoo <- readxl::read_excel(
  "data/raw/GE_Amundsen_zooplankton_vtow200um (1).xlsx",
  sheet = 1
) %>%
  janitor::clean_names() %>%
  mutate(station = parse_number(station_name), .after = station_name) %>%
  select(
    station,
    depth_m = station_depth_m,
    date = sampling_date,
    type,
    abund_ind_m2
  ) %>%
  mutate(transect = station %/% 100 * 100, .after = station) %>%
  filter(transect %in% c(300, 500))


zoo

p3 <- zoo %>%
  ggplot(aes(x = abund_ind_m2)) +
  geom_histogram(bins = 40) +
  facet_grid(type~transect) +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  labs(
    x = quote("Abundance" ~ (ind~m^{-2})),
    y = "Count"
  )

# Combine plots -----------------------------------------------------------


#
# p <- p1 + p2 + p3 + p4 +
#   plot_layout(ncol = 1)

p <- p1 / p2 / p3 +
  plot_layout(heights = c(0.25, 0.25, 0.5))

ggsave(
  "graphs/fig_hplc.pdf",
  device = cairo_pdf,
  width = 12,
  height = 12,
  units = "in"
)

