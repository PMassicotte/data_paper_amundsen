#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Load packages and setup for the project.
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

# renv::install("MilesMcBain/breakerofchains")
# renv::install("PMassicotte/ggpmthemes")
# renv::install("mcguinlu/pathformatr")
# renv::install("ropensci/rnaturalearthdata")
# renv::install("ropensci/rnaturalearthhires")
# renv::install("clauswilke/ggisoband")

library(tidyverse)
library(ggpmthemes)
library(MBA)
library(ggisoband)
library(sf)
library(glue)
library(furrr)
library(ggspatial)
library(rnaturalearth)
library(patchwork)
library(ggforce)
library(scales)
library(ggquiver)
library(ggalt)
library(rnaturalearth)
library(rnaturalearthhires)
library(terra)
library(tidync)
library(data.table)
library(piggyback)

sf_use_s2(FALSE)

plan(multisession(workers = availableCores() - 1))

## Set default ggplot2 font size and font family
theme_set(theme_montserrat(base_size = 10))

theme_update(
  strip.background = element_rect(fill = "white"),
  strip.text = element_text(face = "bold")
)

# Scripts -----------------------------------------------------------------

source("R/01_extract_sic.R")

source("R/fig01.R")
source("R/fig02.R")
source("R/fig03.R")
source("R/fig04.R")
source("R/fig05.R")
source("R/fig06.R")
source("R/fig07.R")
source("R/fig08.R")
source("R/fig09.R")
source("R/fig10.R")
source("R/fig11.R")
source("R/fig12.R")

source("R/02_pdf2png.R")
