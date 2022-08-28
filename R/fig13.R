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
    pigment %in% c(
      "sum Phbd a",
      "Zeaxanthin",
      "Total Chlorophyll a",
      "diadinoxanthin_diatoxanthin"
    )
  ) %>%
  drop_na()

hplc_viz <- hplc %>%
  group_nest(transect, pigment) %>%
  mutate(variable_interpolated = map(
    data,
    ~ interpolate_to_tibble(., longitude, depth_m, conc_mg_m3, n = 100)
  )) %>%
  select(-data) %>%
  unnest(c(variable_interpolated)) %>%
  pivot_wider(names_from = pigment, values_from = conc_mg_m3) %>%
  janitor::clean_names()

# Plot --------------------------------------------------------------------

hplc_viz

p1 <- hplc_viz %>%
  drop_na(total_chlorophyll_a) %>%
  ggplot(aes(
    x = longitude,
    y = depth_m,
    fill = total_chlorophyll_a,
    z = total_chlorophyll_a
  )) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    expand = c(0, 0),
    labels = function(x) {
      paste0(-x, "°W")
    },
    breaks = scales::pretty_breaks(n = 3)
  ) +
  scale_fill_gradientn(
    colours = color,
    guide = guide_colorbar(
      title.position = "top",
      nrow = 1,
      title.hjust = 0.5
    )
  ) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~transect, nrow = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(vjust = -18),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 30, 0),
    strip.text = element_blank()
  ) +
  ylab("Depth (m)") +
  xlab("Longitude") +
  labs(fill = bquote(atop("Chlorophyll-a", (mg ~ m^{
    -3
  }))))

p2 <- hplc_viz %>%
  drop_na(sum_phbd_a) %>%
  ggplot(aes(
    x = longitude,
    y = depth_m,
    fill = sum_phbd_a,
    z = sum_phbd_a
  )) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(
    expand = c(0, 0),
    labels = function(x) {
      paste0(-x, "°W")
    },
    breaks = scales::pretty_breaks(n = 3)
  ) +
  scale_fill_gradientn(
    colours = color,
    guide = guide_colorbar(
      title.position = "top",
      nrow = 1
    )
  ) +
  geom_isobands(color = NA, bins = 100) +
  facet_wrap(~transect, nrow = 1, strip.position = "top") +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.margin = margin(0, 0, 0, 0),
    legend.position = "right",
    legend.box.margin = margin(0, 0, 30, 0),
    strip.text = element_blank(),
    legend.title.align = 0
  ) +
  ylab("Depth (m)") +
  xlab("Longitude") +
  labs(fill = quote(atop("Total\nphaeophorbid\nconcentration", (mg ~ m^{
    -3
  }))))

# Zooplankton -------------------------------------------------------------

zoo <- readxl::read_excel(
  "data/raw/zooplankton_condensed_GE.xlsx",
  sheet = 1
) %>%
  janitor::clean_names() %>%
  mutate(station = parse_number(station_name), .after = station_name) %>%
  select(
    station,
    contains("m3")
  ) %>%
  mutate(transect = station %/% 100 * 100, .after = station) %>%
  filter(transect %in% c(300, 500)) %>%
  pivot_longer(contains("m3"))

zoo

zoo <- zoo %>%
  mutate(zoo_type = case_when(
    str_detect(name, "^copepode") ~ "Copepods",
    str_detect(name, "^non_copepode") ~ "Non-Copepods"
  ))

p3 <- zoo %>%
  ggplot(aes(x = factor(station), y = value, fill = zoo_type)) +
  geom_col(position = "dodge") +
  facet_wrap(~transect, scales = "free_x") +
  labs(
    y = quote("Abundance" ~ (ind ~ m^{
      -3
    })),
    x = "Stations"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    fill = element_blank()
  ) +
  theme(
    legend.position = "right",
    strip.text = element_blank()
  )

# SIC ---------------------------------------------------------------------

sic <- read_csv("data/clean/sic_transects_300_500_v2.csv")

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
  ylab(NULL) +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_blank()
  )

# Combine plots -----------------------------------------------------------

p <-
  {
    (p_sic_t300 + labs(title = "Transect 300")) + (p_sic_t500 + labs(title = "Transect 500"))
  } + plot_layout(ncol = 2) -
  {
    p1 / p2 / p3
  } +
  plot_layout(nrow = 2, heights = c(0.1, 1))


# p <- p1 / p2 / p3 +
#   plot_layout(heights = c(1, 1, 1)) +
#   plot_annotation(tag_levels = "A")

ggsave(
  "graphs/fig13.pdf",
  device = cairo_pdf,
  width = 20,
  height = 20,
  units = "cm"
)
