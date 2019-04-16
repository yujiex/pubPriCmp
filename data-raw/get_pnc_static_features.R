library("dplyr")
library("readr")

pnc_static =
  readr::read_csv("pnc_building_meta_data_from_pi_system.csv") %>%
  dplyr::select(Name, starts_with("|")) %>%
  dplyr::rename_all(funs(gsub("\\|", "", .))) %>%
  dplyr::rename(`latitude`=`Latitude`, `longitude`=`Longitude`, `Building Type`=`Building Class`) %>%
  {.}

load("../data/allData.rda")

## check whether it matched every building
allData %>%
  dplyr::filter(Organization=="PNC") %>%
  distinct(Name, latitude, longitude) %>%
  dplyr::left_join(pnc_static, by=c("latitude", "longitude", "Name")) %>%
  dplyr::filter(is.na(`Building Type`)) %>%
  head()

pnc.buildings = allData %>%
  dplyr::filter(Organization=="PNC") %>%
  distinct(Name) %>%
  .$Name

pnc_static_studyset <-
  pnc_static %>%
  dplyr::filter(Name %in% pnc.buildings) %>%
  dplyr::select(-latitude, -longitude) %>%
  {.}

head(pnc_static_studyset)

devtools::use_data(pnc_static_studyset, pkg="../../pubPriCmp", overwrite = TRUE)
