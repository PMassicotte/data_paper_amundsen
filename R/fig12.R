# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualization of phytoplankton relative abundance per taxon.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

n <- 19
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == "qual", ]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

dow <- read_csv("data/raw/dow_amundsen_2016.csv") %>%
  filter(sic_target == 0.5) %>%
  mutate(dow_class = cut(dow_length, c(-100, 0, 100), labels = c("ice_covered", "open_water"))) %>%
  mutate(dow_class = as.character(dow_class))

taxo <- read_csv("data/raw/greenedge_taxo_phyto_amundsen.csv", guess_max = 1e5) %>%
  filter(magnification_x == 400) %>%
  mutate(nb_cell_l = replace_na(nb_cell_l, 0))

df <- left_join(taxo, dow)

# Sum up the phytoplankton cells by taxonomic group
res <- df %>%
  unite(label, class, order) %>%
  group_by(date, station, latitude, longitude, depth_m, label, dow_class, dow_length) %>%
  summarise(sum_nb_cell_l = sum(nb_cell_l, na.rm = TRUE), n = n())

res2 <- res %>%
  ungroup() %>%
  mutate(layer = ifelse(depth_m < 3, "surface", "scm")) %>%
  complete(dow_length = seq(min(dow_length), max(dow_length)), nesting(layer, label))

res2

# Use 3 meters as the threshold for the SCM. Then, calculate the total number of
# cell for each layer (surface/scm)
res3 <- res %>%
  mutate(layer = ifelse(depth_m < 3, "surface", "scm")) %>%
  group_by(dow_length, layer) %>%
  mutate(total = sum(sum_nb_cell_l)) %>%
  ungroup() %>%
  mutate(percent_abundance = sum_nb_cell_l / total) %>%
  mutate(layer = factor(layer, levels = c("surface", "scm")))

unique(res3$label)

res3 <- res3 %>%
  mutate(label = str_replace(label, "_", " ")) %>%
  mutate(label = str_remove(label, " NA")) %>%
  mutate(label = str_replace(label, "Dinophyceae Dinophyceae", "Dinophyceae"))

p1 <- res3 %>%
  ggplot(aes(x = factor(dow_length), y = percent_abundance, fill = label)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~layer) +
  scale_fill_manual(values = col_vector) +
  xlab("Open Water Days sampling (50% ice-covered)") +
  ylab("Relative abundance (%)") +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    axis.title.x = element_text(
      margin = margin(t = 0, r = 0, b = 20, l = 0),
      vjust = -5
    ),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.key.size = unit(0.5, "cm")
  )
# Add chl on top of the figs ----------------------------------------------

chl <- vroom::vroom("data/raw/greenedge_pigments.csv") %>%
  filter(mission == "amundsen_2016") %>%
  filter(method == "HPLC") %>%
  filter(pigment == "Total Chlorophyll a") %>%
  select(station, depth_m, total_chla_mg_m3 = conc_mg_m3) %>%
  distinct(station, depth_m, .keep_all = TRUE)

res <- data.table(df, key = c("station", "depth_m")) %>%
  distinct(station, depth_m, dow_class, dow_length)

chl <- data.table(chl, key = c("station", "depth_m"))

res <- chl[res, roll = "nearest"] %>%
  mutate(layer = ifelse(depth_m < 3, "surface", "scm")) %>%
  group_by(layer, dow_length) %>%
  summarise(total_chla_mg_m3 = mean(total_chla_mg_m3), n = n())

p2 <- res %>%
  ungroup() %>%
  mutate(layer = factor(layer, levels = c("surface", "scm"))) %>%
  ggplot(aes(
    x = factor(dow_length),
    y = total_chla_mg_m3,
    group = 1
  )) +
  geom_point(stat = "summary", fun = sum) +
  stat_summary(fun = sum, geom = "line") +
  xlab("Open Water Days sampling (50% ice-covered)") +
  ylab(bquote("Chlorophyll a" ~ (mg ~ m^{
    -3
  }))) +
  geom_vline(xintercept = which(res$dow_length == 0), color = "red", lty = 2) +
  facet_wrap(~layer) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

p <- p2 + p1 +
  plot_layout(ncol = 1, heights = c(0.5, 1), guides = "collect")

ggsave(
  "graphs/fig12.pdf",
  device = cairo_pdf,
  width = 30,
  height = 15,
  units = "cm"
)
