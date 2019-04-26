library("dplyr")
library("feather")

load("~/Dropbox/thesis/code/pubPriCmp/data/buildingData.rda")
hddcdd = feather::read_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/hddcdd.feather")
avgminmax_bin = feather::read_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/avgminmax_bin.feather")

allData = buildingData %>%
  dplyr::mutate(year = as.character(year), month=sprintf("%02d", month)) %>%
  dplyr::left_join(hddcdd, by=c("Name", "year", "month")) %>%
  dplyr::left_join(avgminmax_bin, by=c("Name", "year", "month")) %>%
  dplyr::mutate(`eui_elec` = `Electric_(kBtu)` / `GSF`,
                `eui_gas` = `Gas_(kBtu)` / `GSF`,
                `eui_total` = (`Electric_(kBtu)` + `Gas_(kBtu)`) / `GSF`) %>%
  dplyr::mutate(`eui_elec_ln` = log(`eui_elec` + 1),
                `eui_gas_ln` = log(`eui_gas` + 1),
                `eui_total_ln` = log(`eui_total` + 1)) %>%
  dplyr::mutate(`HDD2`=`HDD`^2, `CDD2`=`CDD`^2) %>%
  dplyr::mutate(`Date`=as.character(`Date`)) %>%
  {.}

## check how many buildings in each portfolio
allData %>%
  distinct(Organization, Name) %>%
  dplyr::group_by(Organization) %>%
  dplyr::summarise(n())

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", allData, overwrite = TRUE)

allData %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data/allData.csv")
