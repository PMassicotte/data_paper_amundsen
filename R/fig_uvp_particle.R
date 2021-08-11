# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Figures for the UVP data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

uvp <- vroom::vroom("data/raw/greenedge_uvp_particles.gz") %>%
  filter(mission == "amundsen_2016") %>%
  filter(str_detect(site, "^g\\d{3}")) %>%
  mutate(station = parse_number(site)) %>%
  mutate(transect = station %/% 100 * 100) %>%
  filter(transect <= 700)

uvp %>%
  count(profile, particle_size_range, sample_type)

uvp %>%
  count(particle_size_range)

# Calculate median particle size ------------------------------------------

uvp <- uvp %>%
  extract(
    particle_size_range,
    into = c("particle_min_size", "particle_max_size", "particle_unit_size"),
    regex = "(\\d*\\.?\\d*)-(\\d*\\.?\\d*)\\s*(.*)",
    remove = FALSE,
    convert = TRUE
  ) %>%
  # select(particle_size_range, contains("size")) %>%
  mutate(particle_min_size = ifelse(is.na(particle_min_size), parse_number(particle_size_range), particle_min_size)) %>%
  mutate(particle_max_size = ifelse(is.na(particle_max_size), parse_number(particle_size_range), particle_max_size)) %>%
  mutate(particle_unit_size = ifelse(is.na(particle_unit_size), "mm", particle_unit_size)) %>%
  mutate_at(c("particle_min_size", "particle_max_size"), ~ ifelse(particle_unit_size == "µm", . / 1000, .)) %>%
  mutate(particle_unit_size = "mm") %>%
  rename_at(vars(ends_with("size")), ~ paste0(., "_mm")) %>%
  mutate(particle_median_size_mm = (particle_max_size_mm + particle_min_size_mm) / 2) %>%
  select(station, transect, longitude, latitude, depth_m, contains("size"), count_per_liter)

# Select the most abundant particle class size ----------------------------

uvp <- uvp %>%
  group_by(particle_median_size_mm) %>%
  mutate(total_number_particle = sum(count_per_liter)) %>%
  ungroup() %>%
  filter(dense_rank(desc(total_number_particle)) <= 4) %>%
  mutate(particle_size_range = fct_reorder(particle_size_range, particle_median_size_mm))

# Associate day of open water ---------------------------------------------

owd <- read_csv(pins::pin("https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_5.csv")) %>%
  janitor::clean_names() %>%
  select(station, owd)

uvp <- left_join(uvp, owd, by = "station") %>%
  mutate(station_status = ifelse(owd >= 0, "Open water stations", "Underice stations"))

# Visualize ---------------------------------------------------------------

p <- uvp %>%
  group_by(station_status, depth_m, particle_size_range) %>%
  summarise(average_particle_count = mean(count_per_liter)) %>%
  ungroup() %>%
  filter(depth_m <= 350) %>%
  ggplot(aes(x = depth_m, y = average_particle_count, fill = particle_size_range)) +
  geom_area() +
  coord_flip() +
  scale_x_reverse(expand = c(0, 0), breaks = seq(0, 350, by = 50)) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.2)), breaks = scales::pretty_breaks(3)) +
  facet_grid(station_status~particle_size_range, scales = "free") +
  paletteer::scale_fill_paletteer_d("ggthemes::wsj_rgby") +
  xlab("Depth (m)") +
  ylab(bquote("Average particle count (per mL)")) +
  theme(
    legend.position = "none",
    panel.spacing = unit(1, "lines")
  )

ggsave(
  "graphs/fig_uvp_particles.pdf",
  device = cairo_pdf,
  width = 12,
  height = 7,
  units = "cm"
)
