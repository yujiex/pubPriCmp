load("../data/allData.rda")
library("dplyr")

buildingLatlng <- allData %>%
  dplyr::select(`Name`, `latitude`, `longitude`) %>%
  dplyr::group_by_all() %>%
  slice(1) %>%
  dplyr::ungroup() %>%
  {.}

devtools::use_data(buildingLatlng, pkg="../../pubPriCmp/", overwrite = TRUE)

buildingLatlng %>%
  readr::write_csv("../data/buildingLatlng.csv")
