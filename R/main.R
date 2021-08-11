#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Load packages and setup for the project.
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

library(raster)
library(tidyverse)

# renv::install("MilesMcBain/breakerofchains")
# renv::install("PMassicotte/ggpmthemes")
library(ggpmthemes)

library(MBA)
library(ggisoband)
library(sf)
library(rgdal)
library(glue)
library(furrr)
library(ggspatial)
library(rnaturalearth)
library(patchwork)
library(ggforce)
library(scales)
library(tidync)
library(ggquiver)
library(ggalt)
library(stars)

plan(multisession(workers = availableCores() - 1))

## Set default ggplot2 font size and font family
theme_set(theme_poppins(base_size = 10))

theme_update(
  strip.background = element_rect(fill = "white"),
  strip.text = element_text(face = "bold")
)

