library("dplyr")
library("DBI")
library("readr")
library("rlang")
library("pipeR")
library("leaflet")

## #### ## ## ## ## ## ## ## ## ##
## Get GSA data start
## #### ## ## ## ## ## ## ## ## ##

## reload all data
devtools::load_all("~/Dropbox/gsa_2017/db.interface")
gsa_energy = db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly",
                                              cols=c("Building_Number", "state_abbr", "Gross_Sq.Ft", "year", "month",
                                                     "Electric_(kBtu)", "Gas_(kBtu)")) %>%
  dplyr::rename(`Name`=`Building_Number`) %>%
  {.}

## checked for duplicates, no duplicates
## gsa_energy %>%
##   dplyr::group_by(`Name`, `year`, `month`) %>%
##   dplyr::filter(n() > 1) %>%
##   print()

## get a dataframe with retrofit year month and indicator 1
devtools::load_all("~/Dropbox/gsa_2017/db.interface")
gsa_retrofit =
  db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_ecm", cols=c("Building_Number", "Substantial_Completion_Date")) %>%
  as_data_frame() %>%
  na.omit() %>%
  dplyr::rename(`Name`=`Building_Number`) %>%
  dplyr::mutate(`year` = as.integer(substr(`Substantial_Completion_Date`, start=1, stop=4)),
                `month` = as.integer(substr(`Substantial_Completion_Date`, start=6, stop=7))) %>%
  dplyr::select(-`Substantial_Completion_Date`) %>%
  dplyr::group_by(`Name`, `year`, `month`) %>%
  slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`withRetrofit` = 1) %>%
  {.}

gsa_lat_lon =
  db.interface::get_lat_lon_df() %>%
    dplyr::rename(`Name`=`Building_Number`) %>%
    dplyr::select(-`index`, -`source`) %>>%
    {.}

gsa_city =
  db.interface::read_table_from_db(dbname="all", tablename="EUAS_city") %>%
  dplyr::select(-`source`) %>%
  dplyr::rename(`Name`=`Building_Number`) %>%
  {.}

load("../data/gsa_static_monthly.rda")

gsa_static_monthly <- gsa_static_monthly %>%
  dplyr::select(-GSF) %>%
  {.}

summary(gsa_static_monthly)

gsa_building = gsa_energy %>%
  dplyr::left_join(gsa_lat_lon, by="Name") %>>%
  (?nrow(.)) %>>%
  dplyr::left_join(gsa_city, by="Name") %>>%
  (?nrow(.)) %>>%
  dplyr::left_join(gsa_static_monthly, by=c("Name", "year", "month")) %>>%
  (?nrow(.)) %>>%
  dplyr::rename(`State`=`state_abbr`) %>%
  dplyr::mutate(`Organization`="GSA") %>%
  dplyr::rename(`GSF`=`Gross_Sq.Ft`) %>%
  {.}

## #### ## ## ## ## ## ## ## ## ##
## Get GSA data end
## #### ## ## ## ## ## ## ## ## ##

## #### ## ## ## ## ## ## ## ## ##
## Get PNC data start
## #### ## ## ## ## ## ## ## ## ##
## Read individual energy csv files for pnc buildings
read_pnc_single_data <- function(pathname, filename, energyType, outColName, unitConversionFactor) {
  print(filename)
  df <- readr::read_csv(paste(pathname, filename, sep="/"), col_types = cols()) %>>%
    as.data.frame() %>>%
    ## ("data head"?head(.)) %>>%
    dplyr::filter(`Good`=="True") %>>%
    {.}
  if (nrow(df) == 0) {
    return(NULL)
  } else {
    df <- df %>>%
      dplyr::mutate(`year`=substr(`Timestamp`, start=1, stop=4),
                    `month`=substr(`Timestamp`, start=6, stop=7),
                    `Name`=gsub(".csv", "", filename)) %>>%
      dplyr::select(`Name`, `year`, `month`, `UnitsAbbreviation`, `Value`) %>>%
      dplyr::mutate(!!rlang::sym(outColName):=`Value`*unitConversionFactor) %>>%
      dplyr::select(-`UnitsAbbreviation`, -`Value`) %>>%
    {.}
    return(df)
  }
}

files = list.files(path="pnc_electric", pattern = "*.csv")
acc_elec <- lapply(files, function(f) read_pnc_single_data(pathname="pnc_electric", filename=f, energyType="electric", outColName="Electric_(kBtu)", unitConversionFactor=3.14))

pnc_electric = do.call(rbind, acc_elec) %>>%
  dplyr::group_by(`Name`, `year`, `month`) %>>%
  (?nrow(.)) %>>%
  slice(1) %>>%
  (?nrow(.)) %>>%
  dplyr::ungroup() %>>%
  {.}

files <- list.files(path="pnc_gas", pattern = "*.csv")
acc_gas <- lapply(files, function(f) read_pnc_single_data(pathname="pnc_gas", filename=f, energyType="gas", outColName="Gas_(kBtu)", unitConversionFactor=100))

pnc_gas = do.call(rbind, acc_gas) %>>%
  dplyr::group_by(`Name`, `year`, `month`) %>>%
  (?nrow(.)) %>>%
  slice(1) %>>%
  (?nrow(.)) %>>%
  dplyr::ungroup() %>>%
  {.}

pnc_energy = pnc_electric %>>%
  dplyr::full_join(pnc_gas, by=c("Name", "year", "month")) %>>%
  dplyr::mutate(`Organization`="PNC") %>>%
  {.}

## no duplicates now
## pnc_energy %>>%
##   dplyr::group_by(`Name`, `year`, `month`) %>>%
##   dplyr::filter(n() > 1) %>>%
##   head()

pnc_city_state =
  readr::read_csv("~/Dropbox/thesis/writeups/policy_cmp/tables/pnc_static.csv") %>>%
  tibble::as_data_frame() %>>%
  dplyr::select(Name, state, city) %>%
  dplyr::rename(`State`=`state`,
                `City`=`city`) %>>%
  {.}

## changed source of static from pnc_static.csv to pnc_building_meta_data_from_pi_system.csv
pnc_static =
  readr::read_csv("pnc_building_meta_data_from_pi_system.csv") %>%
  dplyr::select(Name, starts_with("|")) %>%
  dplyr::rename_all(funs(gsub("\\|", "", .))) %>%
  dplyr::rename(`latitude`=`Latitude`, `longitude`=`Longitude`,
                `type_general`=`Building Class`,
                `type_detail`=`Building Use`,
                ) %>%
  ## remove state record here as this source does not have the correct state and city info
  dplyr::select(-`Building Address`, -Region, -Status, -`Building Code`, -`State`, -`City`) %>%
  dplyr::left_join(pnc_city_state, by="Name") %>%
  {.}

pnc_building =
  pnc_energy %>>%
  dplyr::left_join(pnc_static, by="Name") %>>%
  dplyr::mutate(`Organization`="PNC",
                `year`=as.numeric(`year`),
                `month`=as.numeric(`month`),
                ) %>>%
  {.}

## #### ## ## ## ## ## ## ## ## ##
## Get PNC data end
## #### ## ## ## ## ## ## ## ## ##

buildingData =
  gsa_building %>>%
  dplyr::bind_rows(pnc_building) %>>%
  dplyr::left_join(gsa_retrofit, by=c("Name", "year", "month")) %>%
  dplyr::mutate(withRetrofit = ifelse(is.na(withRetrofit), 0, 1)) %>%
  dplyr::mutate(`private`=ifelse(`Organization`=="PNC", 1, 0)) %>%
  dplyr::arrange(`private`, `Name`, `year`, `month`) %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::filter(!is.na(longitude)) %>%
  dplyr::filter(!((`latitude` == 0) & (`longitude` == 0))) %>%
  tidyr::replace_na(list(`Electric_(kBtu)`=0, `Gas_(kBtu)`=0)) %>%
  dplyr::group_by(`Name`) %>%
  dplyr::mutate(`hasRetrofitBeforeThisMonth` = cumsum(`withRetrofit`)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-`withRetrofit`) %>%
  {.}

## print earliest and latest energy date, to select common time range for analysis
buildingData %>%
  dplyr::arrange(`year`, `month`) %>%
  dplyr::group_by(`private`) %>%
  ## show latest time
  slice(n()) %>%
  ## show earliest time
  ## slice(1) %>%
  print()

## earliest time for pnc and gsa are:
##   Name         GSF  year month `Electric_(kBtu… `Gas_(kBtu)` latitude longitude
##   <chr>      <dbl> <dbl> <dbl>            <dbl>        <dbl>    <dbl>     <dbl>
## 1 AK0000AA  727340  2002  10.0          2522150      1939140     61.2    -150
## 2 Hampstea…   4291  2010  10.0               NA            0     39.6    - 76.9

## latest time for pnc and gsa are:
## Name         GSF  year month `Electric_(kBtu… `Gas_(kBtu)` latitude longitude
##   <chr>      <dbl> <dbl> <dbl>            <dbl>        <dbl>    <dbl>     <dbl>
## 1 WY0030ZZ  174831  2017  9.00           307899       139530     42.9    -106
## 2 ZionOffi…   4635  2014  3.00               NA        49400     42.4    - 87.8

## restrict the analysis time range to the common range with full year: 2011-01 to 2013-12
buildingData <-
  buildingData %>%
  dplyr::arrange(`year`, `month`) %>%
  dplyr::mutate(`Date`=zoo::as.yearmon(paste(`year`, `month`), "%Y %m")) %>%
  dplyr::filter((zoo::as.yearmon("201101", "%Y %m") <= Date) & (Date <= zoo::as.yearmon("201312", "%Y%m"))) %>%
  {.}

buildingLatlng = buildingData %>%
  dplyr::select(-`Electric_(kBtu)`, -`Gas_(kBtu)`) %>%
  dplyr::group_by(`Name`) %>%
  slice(1) %>%
  ungroup() %>%
  {.}

pal <- colorFactor(palette = 'Dark2', domain = buildingLatlng$private)

## plot data points on a map
buildingLatlng %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lat=~latitude, lng=~longitude, color=~pal(private),
             label=~sprintf("%s,%s,(%s,%s)", Name, Organization, latitude, longitude)) %>%
  addProviderTiles(providers$CartoDB.Positron)

## From the map, we saw GSA portfolio is located throughout the US, but PNC
## buildings are only on the east side, we want to restrict the geographical
## region of analysis to continental US east side. The most west point of PNC
## building is at about longitude -91.15, we want buildings to have greater
## longitude than this

## plot data points on a map after filtering the longitude, and excluding non-continental US
buildingLatlng %>%
  ## restrict to east US
  dplyr::filter(`longitude` > -100) %>%
  ## restrict to continental US
  dplyr::filter(!State %in% c("AK", "HI", "VI", "GU", "PR", "VI")) %>%
  ## restrict to states completely east of 100th meridian
  dplyr::filter(!State %in% c("ND", "SD", "NE", "KS", "OK", "TX")) %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(lat=~latitude, lng=~longitude, color=~ pal(private),
             label=~sprintf("%s,%s,%s,%s(%s,%s)", Name, Organization, City, State, latitude, longitude)) %>%
  addProviderTiles(providers$CartoDB.Positron)

## apply the same spacial filter
buildingData <-
  buildingData %>%
  ## restrict to east US
  dplyr::filter(`longitude` > -100) %>%
  ## restrict to continental US
  dplyr::filter(!State %in% c("AK", "HI", "VI", "GU", "PR", "VI")) %>%
  ## restrict to states completely east of 100th meridian
  dplyr::filter(!State %in% c("ND", "SD", "NE", "KS", "OK", "TX")) %>%
  {.}

## filter area greater than 0
buildingData <- buildingData %>%
  dplyr::filter(`GSF` > 0) %>%
  {.}

## filter by building with positive electricity and gas consumption during any months in the 3 year period
buildingData <- buildingData %>%
  dplyr::group_by(`Organization`, `Name`) %>%
  dplyr::mutate(positiveElec=sum(`Electric_(kBtu)`)>0, positiveGas=sum(`Gas_(kBtu)`)>0) %>%
  dplyr::ungroup() %>%
  dplyr::filter(positiveElec, positiveGas) %>%
  {.}

climateRegionLookup = readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/climateRegionLookup.csv") %>%
  as.data.frame() %>%
  {.}

# get climate region
buildingData <- buildingData %>%
  dplyr::left_join(climateRegionLookup, by="State") %>%
  {.}

buildingData %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data/buildingData.csv")

buildingLatlng = buildingData %>%
  dplyr::select(-`Electric_(kBtu)`, -`Gas_(kBtu)`) %>%
  dplyr::group_by(`Name`) %>%
  slice(1) %>%
  ungroup() %>%
  {.}

load("../data/buildingData.rda")
load("../data/warmthClass_lat.rda")

USregion = warmthClass_lat %>%
  dplyr::select(`State`, `warmth`) %>%
  dplyr::mutate(`USRegion`=ifelse(`warmth`=="hot", "South", "North")) %>%
  dplyr::select(-`warmth`) %>%
  {.}

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", USregion, overwrite = TRUE)

southern_states =
  c("TX", "OK", "AR", "LA", "MS", "TN", "AL", "GA", "SC", "NC", "FL", "KY", "WV", "VA", "MD", "DE")
USregionWiki = warmthClass_lat %>%
  dplyr::select(`State`) %>%
  dplyr::mutate(`region_wiki`=ifelse(`State` %in% southern_states, "South", "North")) %>%
  {.}

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", USregionWiki, overwrite = TRUE)

buildingData %>%
  nrow()

buildingData <-
  buildingData %>%
  ## dplyr::left_join(USregion, by="State") %>%
  dplyr::left_join(USregionWiki, by="State") %>%
  {.}

## don't know why the data from running the code does not have month in Date
buildingData <-
  readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data/buildingData.csv") %>%
  {.}

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", buildingData, overwrite = TRUE)

ghcnd_data_full = rnoaa::ghcnd_stations(refresh = TRUE)

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", ghcnd_data_full)

buildings = buildingLatlng$Name

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", buildings, overwrite=TRUE)

