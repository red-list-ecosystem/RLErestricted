## code to prepare `glaciers_on_volcanos` dataset goes here

## Download data from NSIDC using earthdata
## echo 'machine urs.earthdata.nasa.gov login <uid> password <password>' >> ~/.netrc
## chmod 0600 ~/.netrc
## cd data-raw
## curl -b ~/.urs_cookies -c ~/.urs_cookies -L -n -O https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0770_rgi_v7/regional_files/RGI2000-v7.0-C/RGI2000-v7.0-C-16_low_latitudes.zip

library(sf)
library(dplyr)
if (!file.exists("data-raw/trop-glacier-groups-labelled.gpkg")) {
  library(osfr) # need to have a valid OSF_PAT entry in .Renviron
  osfcode <- "432sb"
  osf_project <- osf_retrieve_node(sprintf("https://osf.io/%s", osfcode))
  osf_target_files <- osf_ls_files(osf_project, pattern = "gpkg", n_max=10)
  osf_download(
    osf_target_files,
    path = "data-raw/")
}

if (!file.exists("data-raw/RGI2000-v7.0-C-16_low_latitudes.zip")) {
  download.file(
    url = "https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0770_rgi_v7/regional_files/RGI2000-v7.0-C/RGI2000-v7.0-C-16_low_latitudes.zip",
    destfile = "data-raw/RGI2000-v7.0-C-16_low_latitudes.zip",
    method = "curl",
    extra = "-b ~/.urs_cookies -c ~/.urs_cookies -L -n"
  )
} else {
  unzip("data-raw/RGI2000-v7.0-C-16_low_latitudes.zip", exdir = "data-raw")
  file.remove("data-raw/RGI2000-v7.0-C-16_low_latitudes.zip")
}



## make valid and drop z dimension to reduce size of data
rgi7_glaciers <- read_sf("data-raw/RGI2000-v7.0-C-16_low_latitudes.shp") %>%
  st_make_valid() %>%
  st_zm(drop = TRUE, what = "ZM")

calculated_area <- st_area(rgi7_glaciers) %>% set_units('km2')
## exact match
cor(rgi7_glaciers$area_km2, calculated_area)

## select a dTolerance value (in meters) that does not distort areas too much
simple_glaciers <- rgi7_glaciers %>% st_simplify(dTolerance = 100)
calculated_area <- st_area(simple_glaciers) %>% set_units('km2') %>% drop_units()
## still >0.99
cor(rgi7_glaciers$area_km2, calculated_area)
## but several with zero area
plot(rgi7_glaciers$area_km2, calculated_area, log="xy")

exclude_groups <- c("Temperate Glacier Ecosystems", "Norte de Argentina", "Famatina", "Zona Volcanica Central")
glaciers_groups <- read_sf("data-raw/trop-glacier-groups-labelled.gpkg") %>%
  filter(!group_name %in% exclude_groups)  %>%
  select(group_name)

# st_agr
grouped_simple <- st_contains(glaciers_groups, simple_glaciers)
grouped_glaciers <- st_contains(glaciers_groups, rgi7_glaciers)

glaciers_groups <- glaciers_groups %>% mutate(n_simple = lengths(grouped_simple),
                           n_orig = lengths(grouped_glaciers))

glaciers_groups %>% st_drop_geometry() %>% group_by(group_name) %>% summarise(sum(n_simple), sum(n_orig))

plot(st_geometry(glaciers_groups[2,]))
plot(st_geometry(simple_glaciers), add = TRUE, border=2)
plot(st_geometry(rgi7_glaciers), add = TRUE)

groups <- glaciers_groups %>% distinct(group_name) %>% pull

simple_glaciers <- simple_glaciers %>% mutate(group_name = "ungrouped")
for (gg in groups) {
  lst <- unlist(grouped_simple[glaciers_groups$group_name %in% gg])
  simple_glaciers <- simple_glaciers %>% mutate(group_name = replace(group_name,lst,gg))
}
simple_glaciers %>% filter(group_name %in% "ungrouped") %>% st_area

tropical_glaciers <- simple_glaciers %>%
  filter(!group_name %in% "ungrouped") %>%
  select(rgi_id, ecosystem_name = group_name, utm_zone) %>%
  st_make_valid()

st_agr(tropical_glaciers) <- "identity"

tropical_glaciers$utm_zone <- sprintf("%s%s",
                                      pull(tropical_glaciers,utm_zone),
                                      ifelse(st_coordinates(st_centroid(tropical_glaciers))[,2]>0, "N", "S"))

tropical_glaciers %>%
  st_drop_geometry() %>%
  group_by(ecosystem_name,utm_zone) %>%
  summarise(n())

# based on this reference: https://docs.up42.com/data/reference/utm
crs_codes <-
  c("35N" = 32635,
    "19N" = 32619,
    "18N" = 32618,
    "14N" = 32614,
    "17S" = 32717,
    "18S" = 32718,
    "19S" = 32719,
    "37S" = 32737,
    "53S" = 32753)

tropical_glaciers <-
  tropical_glaciers |>
    transmute(rgi_id, ecosystem_name, utm_zone, crs_code = crs_codes[utm_zone])
st_agr(tropical_glaciers) <- "identity"
# try to avoid problems with check()
Encoding(st_crs(tropical_glaciers)$wkt) <- "UTF-8"

usethis::use_data(tropical_glaciers, overwrite = TRUE)

for (to_be_removed in dir("data-raw", "RGI2000", full.names = T)) {
  file.remove(to_be_removed)
}

for (to_be_removed in dir("data-raw", "gpkg", full.names = T)) {
  file.remove(to_be_removed)
}

