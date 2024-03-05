## code to prepare `glaciers_on_volcanos` dataset goes here

library(sf)
library(dplyr)
if (!file.exists("data-raw/rgi-Volcanos_de_Peru_y_Chile-proj1.gpkg")) {
  library(osfr) # need to have a valid OSF_PAT entry in .Renviron
  osfcode <- "432sb"
  osf_project <- osf_retrieve_node(sprintf("https://osf.io/%s", osfcode))
  osf_all_files <- osf_ls_files(osf_project, "group-polygons", n_max=100)
  osf_target_file <- filter(osf_all_files, grepl("rgi-Volcanos_de_Peru_y_Chile",name))
  osf_download(
    osf_target_file,
    path = "data-raw/")
}

glaciers_on_volcanos <- read_sf("data-raw/rgi-Volcanos_de_Peru_y_Chile-proj1.gpkg") %>% transmute(ecosystem_name = "Tropical glaciers on the Volcanos of Peru and Chile")
Encoding(st_crs(glaciers_on_volcanos)$wkt) <- "UTF-8"

usethis::use_data(glaciers_on_volcanos, overwrite = TRUE)
