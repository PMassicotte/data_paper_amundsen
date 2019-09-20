mvp <- vroom::vroom("/media/data4tb/greenedge/clean/mvp/greenedge_mvp.csv", altrep_opts = TRUE) %>%
  mutate(initial_longitude_deg = -initial_longitude_deg)

glimpse(mvp)

mvp %>%
  distinct(cruise_number, section_number, mission)

mvp %>%
  distinct(initial_longitude_deg, initial_latitude_deg, .keep_all = TRUE) %>%
  ggplot(aes(x = initial_longitude_deg, y = initial_latitude_deg, color = section_number)) +
  geom_point()

mvp %>%
  filter(section_number == "2016001_01") %>%
  select(initial_longitude_deg, pres, temp) %>%
  mba.surf(no.X = 200, no.Y = 200, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  rename(temp = xyz.est.z, longitude = xyz.est.x, pres = xyz.est.y) %>%
  ggplot(aes(x = longitude, y = pres, fill = temp, z = temp)) +
  geom_raster() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(
    # limits = c(31.5, 33),
    # oob = scales::squish,
    breaks = seq(-2, 4, by = 1),
    direction = 1
  ) +
  geom_isobands(breaks = seq(-2, 4, by = 0.25), color = NA)
