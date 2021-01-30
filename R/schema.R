# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Size continuum schema.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <-
  readxl::read_excel("data/raw/size_continuum_amundsen.xlsx") %>%
  mutate(middle_size = (maximum_size_um + minimum_size_um) / 2)

df %>%
  mutate(technique = str_to_title(technique)) %>%
  mutate(technique = fct_reorder(technique, minimum_size_um)) %>%
  ggplot(aes(color = technique)) +
  geom_dumbbell(
    aes(
      x = minimum_size_um,
      y = technique,
      xend = maximum_size_um
    ),
    size_x = 2,
    size_xend = 2,
    size = 1
  ) +
  geom_text(
    aes(x = minimum_size_um, y = technique, label = technique),
    hjust = "left",
    vjust = -1,
    size = 3
  ) +
  # geom_point(aes(x = minimum_size_um, y = technique)) +
  scale_x_log10(
    labels = function(x) {
      glue("{scales::scientific(x)} μm")
    },
    expand = expand_scale(mult = c(0.1, 0.4))
  ) +
  annotation_logticks(sides = "b", size = 0.25) +
  ylab(NULL) +
  xlab("Size measurement continuum") +
  theme(
    panel.border = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

ggsave(
  "graphs/schema.pdf",
  device = cairo_pdf,
  width = 12,
  height = 12,
  units = "cm"
)
