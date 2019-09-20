# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Figure for CTD transects.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- vroom::vroom("/media/data4tb/greenedge/clean/ctd/greenedge_ctd.csv", delim = ",") %>%
  filter(mission == "amundsen_2016") %>%
  filter(str_starts(station, "G")) %>%
  mutate(transect = str_sub(station, 1, 2)) %>%
  filter(depth_m <= 100)

df %>%
  distinct(transect)

interpolate_to_tibble <- function(df, x, y, z) {

  df <- df %>%
    select({{x}}, {{y}}, {{z}}) %>%
    drop_na() %>%
    mba.surf(no.X = 200, no.Y = 200, sp = TRUE, extend = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(
      {{x}} := xyz.est.x,
      {{y}} := xyz.est.y,
      {{z}} := xyz.est.z
    )

  return(df)

}

p1 <- df %>%
  group_by(transect) %>%
  nest() %>%
  mutate(res = map(data, ~interpolate_to_tibble(., longitude, depth_m, te90_degrees_c))) %>%
  unnest(c(res)) %>%
  ggplot(aes(x = longitude, y = depth_m, fill = te90_degrees_c, z = te90_degrees_c)) +
  # geom_raster() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c() +
  geom_isobands(breaks = seq(-3, 5, by = 1), color = NA) +
  facet_wrap(~transect, scales = "free", nrow = 1) +
  theme(
    axis.title.x = element_blank()
  ) +
  ylab("Depth (m)")

p2 <- df %>%
  group_by(transect) %>%
  nest() %>%
  mutate(res = map(data, ~interpolate_to_tibble(., longitude, depth_m, asal_g_kg))) %>%
  unnest(c(res)) %>%
  ggplot(aes(x = longitude, y = depth_m, fill = asal_g_kg, z = asal_g_kg)) +
  # geom_raster() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(
    limits = c(31.5, 35),
    oob = scales::squish,
    breaks = seq(30, 35, by = 1),
    direction = 1
  ) +
  geom_isobands(breaks = seq(30, 35, by = 0.25), color = NA) +
  facet_wrap(~transect, scales = "free", nrow = 1) +
  theme(
    axis.title.x = element_blank()
  ) +
  ylab("Depth (m)")

p3 <- df %>%
  group_by(transect) %>%
  nest() %>%
  mutate(res = map(data, ~interpolate_to_tibble(., longitude, depth_m, flor_mg_m3))) %>%
  unnest(c(res)) %>%
  mutate(flor_mg_m3 = ifelse(flor_mg_m3 < 0, 0, flor_mg_m3)) %>%
  ggplot(aes(x = longitude, y = depth_m, fill = flor_mg_m3, z = flor_mg_m3)) +
  # geom_raster() +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(
    limits = c(0, 10),
    oob = scales::squish
  ) +
  geom_isobands(breaks = seq(0, 30, by = 1), color = NA) +
  facet_wrap(~transect, scales = "free", nrow = 1) +
  theme(
    axis.title.x = element_blank()
  ) +
  ylab("Depth (m)")

p4 <- df %>%
  group_by(transect) %>%
  nest() %>%
  mutate(res = map(data, ~interpolate_to_tibble(., longitude, depth_m, tran_percent))) %>%
  unnest(c(res)) %>%
  # filter(transect == "G7") %>%
  ggplot(aes(x = longitude, y = depth_m, fill = tran_percent, z = tran_percent)) +
  scale_y_reverse(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  geom_isobands(breaks = seq(40, 120, by = 2), color = NA) +
  scale_fill_viridis_c(
    limits = c(70, 100),
    oob = scales::squish
  ) +
  facet_wrap(~transect, scales = "free", nrow = 1) +
  ylab("Depth (m)")

p <- p1 / p2 / p3 / p4

ggsave("graphs/fig03.pdf", device = cairo_pdf, width = 16, height = 8)
