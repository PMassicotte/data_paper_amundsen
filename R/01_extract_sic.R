# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract sea ice concentration for transect 300 and 500 to plot
# the data as a function of the longitude.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# I will use the coordinate of the CTD and MVP to extract the SIC that will be
# plotted.

ctd <- read_delim("data/clean/greenedge_ctd.csv", delim = ",") %>%
  filter(mission == "amundsen_2016") %>%
  filter(str_starts(station, "G")) %>%
  mutate(transect = parse_number(station) %/% 100 * 100) %>%
  filter(transect %in% c(300, 500)) %>%
  filter(depth_m <= 200) %>%
  mutate(source = "ctd") %>%
  distinct(date, transect, longitude, latitude)

mvp <- read_csv("data/clean/greenedge_mvp.csv") %>%
  mutate(initial_longitude_deg = -initial_longitude_deg) %>%
  filter(pres <= 200)

# Recode the transect number
mvp <- mvp %>%
  mutate(transect = case_when(
    section_number %in% c("2016001_01", "2016001_02") ~ 100,
    section_number %in% c("2016001_05") ~ 400,
    section_number %in% c("2016001_04") ~ 300,
    section_number %in% c("2016001_08", "2016001_09") ~ 600,
    section_number %in% c("2016001_06", "2016001_07") ~ 500,
    section_number %in% c("2016001_03") ~ 200,
    TRUE ~ NA_real_
  )) %>%
  filter(transect %in% c(300, 500)) %>%
  add_column(source = "mvp", .before = 1) %>%
  distinct(date, transect, longitude = initial_longitude_deg, latitude = initial_latitude_deg)

stations <- bind_rows(ctd, mvp)

stations

dest_proj <- "EPSG:3413"

extract_sic <- function(df, date) {
  message("Extracting SIC for ", date, " ...")

  # Build the filename that contains the SIC for a specific date
  # f <- glue(
  #   "/vsigzip//vsicurl/ftp://ftp-projects.cen.uni-hamburg.de/seaice/AMSR2/3.125km/Arc_{ymd}_res3.125_pyres.nc.gz",
  #   ymd = format(date, "%Y%m%d")
  # )

  f <- glue(
    "ftp://ftp-projects.cen.uni-hamburg.de/seaice/AMSR2/3.125km/Arc_{ymd}_res3.125_pyres.nc.gz",
    ymd = format(date, "%Y%m%d")
  )

  f <- curl::curl_download(f, destfile = tempfile(fileext = "nc.gz"))

  r <- rast(paste0("/vsigzip/", f), "sea_ice_concentration")

  # Set the extent
  ext(r) <- ext(-3850000, 3750000, -5350000, 5850000)

  # Set the polar stereographic projection
  crs(r) <- dest_proj

  df |>
    vect(geom = c("longitude", "latitude"), crs = "EPSG:4326") |>
    project(dest_proj) |>
    extract(r, y = _) |>
    select(-ID)
}

sic <- stations |>
  group_nest(transect, date) |>
  mutate(sic = map2(data, date, extract_sic)) |>
  unnest(cols = c(data, sic))

sic

# Save the results
write_csv(sic, "data/clean/sic_transects_300_500_v2.csv")
