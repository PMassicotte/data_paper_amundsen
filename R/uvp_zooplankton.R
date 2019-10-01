# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# UVP zooplankton data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

uvp <- vroom::vroom("/media/data4tb/greenedge/clean/uvp/greenedge_uvp_zooplankton.csv") %>%
  filter(mission == "amundsen_2016") %>%
  filter(str_detect(site, "^g\\d{3}")) %>%
  mutate(station = parse_number(site)) %>%
  mutate(transect = station %/% 100 * 100) %>%
  filter(transect <= 700)

glimpse(uvp)

# Filter to keep only zooplankton data ------------------------------------

# Filter pour garder uniquement zooplancton. Cyril ma dit qu'il y avait aussi du
# phyto (Harosa). Finalement, Harosa c'est un super groupe qui dans ce cas-ci
# devrait uniquement contenir du zooplancton. Pas besoin de filtrer.

uvp_cleaned <- uvp %>%
  filter(living_category == "living" & class != "duplicate") %>%
  select(station, transect, depth_m, contains("taxon"), biovolume_ppm, number) %>%
  filter(taxon_a != "other")

uvp_cleaned

# Complete the data -------------------------------------------------------

# Here, when a taxon is not identified (NA), I append it with ("unidentified")

uvp_cleaned <- uvp_cleaned %>%
  mutate(new = row_number()) %>%
  gather(var, val, contains("taxo")) %>%
  group_by(new) %>%
  mutate(flag = as.integer(is.na(val))) %>%
  fill(val) %>%
  mutate(val = replace(val, flag == 1, paste(val[flag == 1], "(unidentified)")))  %>%
  select(-flag) %>%
  spread(var, val) %>%
  ungroup() %>%
  select(-new)

# Outliers ----------------------------------------------------------------

# We have VERY large biovolume, almost impossible.
uvp_cleaned %>%
  ggplot(aes(x = biovolume_ppm)) +
  geom_histogram() +
  scale_x_log10() +
  annotation_logticks(sides = "b")

uvp_cleaned <- uvp_cleaned %>%
  filter(biovolume_ppm <= 3000)

# Replicate? --------------------------------------------------------------

uvp_cleaned %>%
  filter(station == 100) %>%
  mutate(depth_m = fct_reorder(as.character(depth_m), -depth_m)) %>%
  ggplot(aes(x = depth_m, y = biovolume_ppm)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "There are replicates at each depth (station #100)",
    subtitle = "Should we calculate an average value?"
  )

# Remove anectodal observations
uvp_cleaned <- uvp_cleaned %>%
  add_count(taxon_d) %>%
  filter(n >= 100)

# I will calculate an average biovolume per depth
uvp_cleaned_averaged <- uvp_cleaned %>%
  group_nest(station, transect, depth_m, taxon = taxon_c) %>%
  mutate(average_biovolume_ppm = map_dbl(data, ~mean(.$biovolume_ppm, na.rm = TRUE)))

# Associate day of open water ---------------------------------------------

owd <- read_csv(pins::pin("https://raw.githubusercontent.com/poplarShift/ice-edge/master/nb_data/FIGURE_5.csv")) %>%
  janitor::clean_names() %>%
  select(station, owd)

uvp_cleaned_averaged <- left_join(uvp_cleaned_averaged, owd, by = "station") %>%
  mutate(station_status = ifelse(owd >= 0, "Open water\nstations", "Underice\nstations"))

# Visualize ---------------------------------------------------------------

uvp_cleaned_averaged %>%
  count(taxon, sort = TRUE)

uvp_cleaned_averaged <- uvp_cleaned_averaged %>%
  add_count(station, depth_m, name = "number_of_sampled_depth") # How many depths were sampled?

uvp_cleaned_viz <- uvp_cleaned_averaged %>%
  # filter(number_of_sampled_depth >= 5) %>% # Need at least 5 depths at which we have measurement (we want a vertical profile)
  group_by(station_status, depth_m, taxon) %>%
  summarise(average_biovolume = mean(average_biovolume_ppm)) %>%
  ungroup() %>%
  filter(depth_m <= 350)

uvp_cleaned_viz %>%
  mutate(average_biovolume = average_biovolume + 1) %>% # Because of the log10
  ggplot(aes(x = depth_m, y = average_biovolume, fill = taxon)) +
  geom_area() +
  coord_flip() +
  scale_x_reverse(expand = c(0, 0), breaks = seq(0, 350, by = 50)) +
  scale_y_log10(expand = expand_scale(mult = c(0, 0.2))) +
  facet_grid(station_status~taxon, scales = "free") +
  paletteer::scale_fill_paletteer_d(ggthemes, wsj_rgby) +
  xlab("Depth (m)") +
  ylab("Average biovolume (ppm)") +
  theme(
    legend.position = "none"
  )
