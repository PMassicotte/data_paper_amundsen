rm(list = ls())

source("R/utils.R")

float <- feather::read_feather("/mnt/nfs/scratch/pmassicotte/isa/data/clean/float.feather") %>%
  extract(profile_id, into = c("float", "ff"), regex = "(taka[:alnum:]*)_(.*)")

float %>%
  distinct(float)

# I need to re-process float data to keep the date time date, convert it to
# numeric for the interpolation and then back into date time for plotting.

p <- float %>%
  filter(float == "takapm016b") %>%
  filter(depth_m <= 100) %>%
  mutate(date2 = lubridate::yday(date)) %>%
  group_nest(float) %>%
  mutate(res = map(data, ~interpolate_to_tibble(., date2, depth_m, temperature, n = 100))) %>%
  select(-data) %>%
  unnest(c(res)) %>%
  mutate(date = as.Date(paste0("2016-", round(date2)), "%Y-%j")) %>%
  ggplot(aes(x = date2, y = depth_m, fill = temperature, color = temperature, z = temperature)) +
  # geom_tile() +
  geom_isobands(bins = 50, color = NA) +
  scale_y_reverse() +
  facet_wrap(~float, scales = "free_x", ncol = 4) +
  scale_fill_gradientn(colours = color)

ggsave("graphs/fig04.pdf", device = cairo_pdf, width = 16, height = 8)
