library("dplyr")
library("readr")

devtools::load_all("~/Dropbox/gsa_2017/db.interface")

load("../data/allData.rda")
gsa.buildings = allData %>%
  dplyr::filter(Organization=="GSA") %>%
  distinct(Name) %>%
  .$Name

gsa_area = allData %>%
  dplyr::filter(Organization=="GSA") %>%
  dplyr::group_by(Name) %>%
  dplyr::summarise(GSF=mean(GSF)) %>%
  {.}

gsa.buildings.shortname =
  substr(gsa.buildings, 1, 6)

gsa_type =
  db.interface::read_table_from_db(dbname="all", tablename = "EUAS_type_recode") %>%
  dplyr::mutate_at(vars(`Building_Type`), recode, "Other - Public Services"="Public Services", "Other - Services"="Service") %>%
  dplyr::select(-`data_source`) %>%
  dplyr::rename(`Name`=`Building_Number`, `Building Type`=`Building_Type`) %>%
  dplyr::filter(Name %in% gsa.buildings) %>%
  {.}

## built year is not identifiable due to formatting issue in "Entire GSA
## Portfolio..." table

## gsa_ownership_1 =
##   db.interface::read_table_from_db(dbname = "other_input", tablename = "Entire_GSA_Building_Portfolio_input") %>%
##   dplyr::select(`Building Number`, `Owned or Leased Indicator`) %>%
##   dplyr::rename(`Name`=`Building Number`) %>%
##   dplyr::filter(Name %in% gsa.buildings) %>%
##   {.}

## gsa_ownership_2 =
##   readxl::read_excel("ownership from the web/iolp-buildings.xlsx") %>%
##   dplyr::filter(`LOCATION_CODE` %in% gsa.buildings.shortname) %>%
##   dplyr::select(`Building Number`, `Owned or Leased Indicator`) %>%
##   dplyr::rename(`Name`=`Building Number`) %>%
##   dplyr::filter(Name %in% gsa.buildings) %>%
##   {.}

gsa_ownership_3 =
  db.interface::read_table_from_db(dbname = "all", tablename="EUAS_monthly",
                                   cols = c("Building_Number", "year", "Cat")) %>%
  dplyr::filter(2010 < year, year < 2014) %>%
  {.}

gsa_ownership_3 <- gsa_ownership_3 %>%
  dplyr::mutate(Ownership=ifelse(Cat %in% c("C", "D"), "Leased", "Owned")) %>%
  dplyr::group_by(`Building_Number`, `Ownership`) %>%
  dplyr::summarise(cnt=n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(`Building_Number`, desc(`cnt`)) %>%
  dplyr::group_by(`Building_Number`) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-cnt) %>%
  dplyr::rename(`Name`=`Building_Number`) %>%
  dplyr::filter(Name %in% gsa.buildings) %>%
  {.}

gsa_static_studyset <-
  data.frame(Name=gsa.buildings) %>%
  dplyr::left_join(gsa_type, by="Name") %>%
  dplyr::left_join(gsa_ownership_3, by="Name") %>%
  dplyr::left_join(gsa_area, by="Name") %>%
  {.}

## add area to the static set

devtools::use_data(gsa_static_studyset, pkg="../../pubPriCmp", overwrite = TRUE)
