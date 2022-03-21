# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  After a discussion with people from ESSD, it was decided that we
# would create data files that only contains Amundsen data (i.e. filter out the
# ice camp data).This script reads all the data files on the SEANOE of the ice
# camp data paper and filter out to only keep data from the Amundsen. This data
# was sent to Flavienne to be added on a new SEANOE DOI that is exclusive to the
# Amundsen data paper.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

library(tidyverse)
library(rvest)
library(glue)

url <- "https://www.seanoe.org/data/00487/59892/"

urls <- read_html(url) %>%
  html_nodes("a") %>%
  html_attr("href")

titles <- read_html(url) %>%
  html_nodes("a") %>%
  html_text()

df <- tibble(titles, urls) %>%
  filter(str_detect(urls, "\\.csv$")) %>%
  distinct()

df %>%
  write_csv("~/Desktop/seanoe/lookup_table.csv")

# Download the data -------------------------------------------------------

destdir <- "~/Desktop/seanoe/"

res <- df %>%
  mutate(destfiles = fs::path(destdir, basename(urls)))

walk2(res$urls, res$destfiles, ~curl::curl_download(.x, .y))

# Read and re-export in csv -----------------------------------------------

files <- fs::dir_ls("~/Desktop/seanoe/")

filter_export_amundsen <- function(file) {
  print(file)
  data.table::fread(file) %>%
    filter(str_detect(mission, "amundsen")) %>%
    data.table::fwrite(fs::path(fs::path_dir(file), "amundsen", basename(file)))
}

walk(files, filter_export_amundsen)
