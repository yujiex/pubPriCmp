library("dplyr")
library("readr")
library("readxl")
library("pipeR")

devtools::load_all("~/Dropbox/gsa_2017/db.interface")

## load("../data/allData.rda")
## gsa.buildings = allData %>%
##   dplyr::filter(Organization=="GSA") %>%
##   distinct(Name) %>%
##   .$Name

devtools::load_all("~/Dropbox/gsa_2017/db.interface")
gsa_energy = db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly",
                                              cols=c("Building_Number", "state_abbr", "Gross_Sq.Ft", "year", "month",
                                                     "Electric_(kBtu)", "Gas_(kBtu)")) %>%
  dplyr::rename(`Name`=`Building_Number`) %>%
  {.}

gsa_zipcode =
  db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_address", cols=c("Building_Number", "Zip_Code", "source")) %>%
  dplyr::filter(!is.na(`Zip_Code`)) %>%
  dplyr::mutate(`Zip_Code`=substr(`Zip_Code`, 1, 5)) %>%
  dplyr::group_by(`Building_Number`, `Zip_Code`) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  ## randomly checked a few buildings, seems the "Entire Portfolio*" file has more accurate zip code record
  dplyr::mutate(`source` = factor(`source`,
                                       levels = c("Entire_GSA_Building_Portfolio_input",
                                                  "PortfolioManager_sheet0_input"))) %>%
  dplyr::arrange(`Building_Number`, `source`) %>%
  dplyr::group_by(`Building_Number`) %>%
  slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-source) %>%
  dplyr::rename(`Name`=`Building_Number`,
                `Zip Code`=`Zip_Code`) %>%
  {.}

gsa_type_general =
  db.interface::read_table_from_db(dbname="all", tablename = "EUAS_type_recode") %>%
  dplyr::mutate_at(vars(`Building_Type`), recode, "Other - Public Services"="Public Services", "Other - Services"="Service") %>%
  dplyr::select(-`data_source`) %>%
  dplyr::rename(`Name`=`Building_Number`, `type_general`=`Building_Type`) %>%
  {.}

gsa_type_detail =
  readxl::read_excel("Unlocked GSA Energy Start Data Entire Portfolio_09142015.xlsx", sheet=3, skip=5) %>%
  dplyr::mutate(`Name` = substr(`Property Name`, 1, 8)) %>%
  dplyr::select(`Name`, `Property Use Name`, `Gross Floor Area for Use`) %>%
  ## select the dominant use which has the largest floor area
  dplyr::arrange(`Name`, desc(`Gross Floor Area for Use`)) %>%
  dplyr::group_by(`Name`) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`Gross Floor Area for Use`) %>%
  {.}

head(gsa_type_detail)

gsa_other_detail <-
  readxl::read_excel("Unlocked GSA Energy Start Data Entire Portfolio_09142015.xlsx", sheet=4, skip=5) %>%
  dplyr::mutate(`Name` = substr(`Property Name`, 1, 8)) %>%
  dplyr::select(`Name`, `Property Use Name`, `Detail Name`, `Detail Current as of Date`, `Detail Value`) %>%
  dplyr::arrange(`Name`, `Property Use Name`, `Detail Name`, desc(`Detail Current as of Date`)) %>%
  dplyr::group_by(`Name`, `Property Use Name`, `Detail Name`) %>%
  slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(gsa_type_detail, by=c("Name", "Property Use Name")) %>%
  ## filter out the detail fields with large enough number of buildings having that field (above 25)
  dplyr::filter(`Detail Name` %in% c("Number of Workers on Main Shift", "Weekly Operating Hours", "Number of Computers",
                                     "Percent That Can Be Cooled", "Percent That Can Be Heated")) %>%
  dplyr::select(-`Detail Current as of Date`) %>%
  tidyr::spread(`Detail Name`, `Detail Value`) %>%
  dplyr::rename(`type_detail`=`Property Use Name`) %>%
  dplyr::rename(`Sum of Employees`=`Number of Workers on Main Shift`) %>%
  dplyr::mutate_at(vars(`Sum of Employees`, `Number of Computers`, `Weekly Operating Hours`), as.numeric) %>%
  {.}

head(gsa_other_detail)

gsa_built_year =
  readxl::read_excel("Unlocked GSA Energy Start Data Entire Portfolio_09142015.xlsx", sheet=1, skip=5) %>%
  dplyr::select(`Property Name`, `Year Built`) %>%
  dplyr::mutate(`Name` = substr(`Property Name`, 1, 8)) %>%
  dplyr::select(-`Property Name`) %>%
  ## only keep non-conflicting record
  dplyr::group_by(`Name`) %>%
  filter(n()==1) %>%
  dplyr::ungroup() %>%
  {.}

gsa_ownership_gsf =
  db.interface::read_table_from_db(dbname = "all", tablename="EUAS_monthly",
                                   cols = c("Building_Number", "year", "month", "Cat", "Gross_Sq.Ft")) %>%
  dplyr::filter(2010 < year, year < 2014) %>%
  dplyr::mutate(Ownership=ifelse(Cat %in% c("C", "D"), "Leased", "Owned")) %>%
  dplyr::select(-Cat) %>%
  dplyr::rename(`Name`=`Building_Number`, `GSF`=`Gross_Sq.Ft`) %>%
  {.}

## take the majority vote for building ownership, checked, there's no cases for tie breaking
gsa_ownership_gsf_alltime <-
  gsa_ownership_gsf %>%
  dplyr::group_by(`Name`) %>%
  ## there's no equal cases, so did not consider random vote
  dplyr::do(tibble::tibble(GSF=mean(.$GSF),
                           Ownership=names(which.max(table(.$Ownership))))) %>%
  dplyr::ungroup() %>%
  ## dplyr::filter(Name %in% gsa.buildings) %>%
  {.}

## read recode table for the building types
## I manually unified some non-standard building type names
gsa_detail_use_recode <-
  readr::read_csv("recode_type_detail_gsa.csv") %>%
  na.omit() %>%
  {.}

## static informaiton by month
gsa_static_monthly <-
  gsa_ownership_gsf %>%
  dplyr::left_join(gsa_type_general, by="Name") %>>%
  (?nrow(.)) %>>%
  dplyr::left_join(gsa_type_detail, by="Name") %>>%
  (?nrow(.)) %>>%
  dplyr::left_join(gsa_other_detail, by="Name") %>>%
  (?nrow(.)) %>>%
  dplyr::select(-`Property Use Name`) %>%
  ## recode detailed use
  dplyr::left_join(gsa_detail_use_recode, by=c("type_general", "type_detail")) %>%
  dplyr::mutate(`type_detail`=ifelse(is.na(`recode_detail`), `type_detail`, `recode_detail`)) %>>%
  (?head(.)) %>>%
  ## remove building ID from the use type
  dplyr::mutate(`type_detail`=ifelse(grepl("[A-Z]{2}[0-9]{4}[A-Z]{2}[ -]{1,3}", `type_detail`), substr(`type_detail`, attr(regexpr("[A-Z]{2}[0-9]{4}[A-Z]{2}[ -]{1,3}", `type_detail`), "match.length") + 1, nchar(`type_detail`)), `type_detail`)) %>%
  dplyr::select(-`recode_detail`) %>%
  {.}

gsa_static_monthly %>%
  distinct(`type_detail`) %>%
  print(n=Inf)

## static information throughout the period, with aggregated ownership and average GSF
gsa_static_alltime <-
  gsa_ownership_gsf_alltime %>%
  dplyr::left_join(gsa_type_general, by="Name") %>>%
  (?nrow(.)) %>>%
  dplyr::left_join(gsa_type_detail, by="Name") %>>%
  (?nrow(.)) %>>%
  dplyr::left_join(gsa_other_detail, by="Name") %>>%
  (?nrow(.)) %>>%
  dplyr::select(-`Property Use Name`) %>%
  ## recode detailed use
  dplyr::left_join(gsa_detail_use_recode, by=c("type_general", "type_detail")) %>%
  dplyr::mutate(`type_detail`=ifelse(is.na(`recode_detail`), `type_detail`, `recode_detail`)) %>>%
  (?head(.)) %>>%
  ## remove building ID from the use type
  dplyr::mutate(`type_detail`=ifelse(grepl("[A-Z]{2}[0-9]{4}[A-Z]{2}[ -]{1,3}", `type_detail`), substr(`type_detail`, attr(regexpr("[A-Z]{2}[0-9]{4}[A-Z]{2}[ -]{1,3}", `type_detail`), "match.length") + 1, nchar(`type_detail`)), `type_detail`)) %>%
  dplyr::select(-`recode_detail`) %>%
  {.}

## save to data files
devtools::use_data(gsa_static_monthly, pkg="../../pubPriCmp", overwrite = TRUE)
devtools::use_data(gsa_static_alltime, pkg="../../pubPriCmp", overwrite = TRUE)
