library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(xtable)
library(nnet)
library(readxl)
library(pipeR)

df_state =
  readr::read_csv("state_abbr.csv") %>%
  as_data_frame() %>%
  dplyr::rename(`State`=`state_full_name`) %>%
  dplyr::mutate(`State`=toupper(`State`)) %>%
  {.}

df1 = readr::read_csv("renewablePortfolio.csv") %>%
  as_data_frame() %>%
  dplyr::mutate(`Greeness`=`Standard`) %>%
  dplyr::mutate_at(vars(`Greeness`), recode,
                    "renewable portfolio standards" = "Most Green",
                    "voluntary renewable energy standard or target" = "Moderate Green",
                    "no standard or target" = "Least Green") %>%
  dplyr::select(`State`, `Greeness`) %>%
  dplyr::mutate(`Source`="NCSL") %>%
  {.}

df2 =
  readr::read_csv("wallethubGreenessScore.csv") %>%
  as_data_frame() %>%
  dplyr::mutate(`Greeness` = cut(`Overall Rank`, breaks = c(0, 17, 34, 50))) %>%
  dplyr::mutate_at(vars(`Greeness`), recode, `(0,17]`="Most Green",
                        `(17,34]`="Moderate Green",
                        `(34,50]`="Least Green",
                        ) %>%
  dplyr::mutate(`State`=toupper(`State`)) %>%
  inner_join(df_state, by="State") %>%
  dplyr::select(`state_abbr`, `Greeness`) %>%
  dplyr::rename(`State`=`state_abbr`) %>%
  dplyr::mutate(`Source`="WalletHub") %>%
  {.}

df3 =
  readr::read_csv("forbesGreenessScore.csv") %>%
  as_data_frame() %>%
  dplyr::mutate(`Greeness` = cut(`Rank`, breaks = c(0, 17, 34, 50))) %>%
  dplyr::mutate_at(vars(`Greeness`), recode, `(0,17]`="Most Green",
                        `(17,34]`="Moderate Green",
                        `(34,50]`="Least Green",
                        ) %>%
  dplyr::mutate(`State`=toupper(`State`)) %>%
  inner_join(df_state, by="State") %>%
  dplyr::select(`state_abbr`, `Greeness`) %>%
  dplyr::rename(`State`=`state_abbr`) %>%
  dplyr::mutate(`Source`="Forbes") %>%
  {.}

df = rbind(df1, df2, df3) %>%
  {.}

## combine the three categorical variable of greeness into one majority vote has
## the property that if the three variables comes in with the three category
## each occupying 1/3 of the mass, after combining them, the three categories
## are still 1/3 each, as for averaging and rounding, if we use the usuall
## rounding setting, it could make group 2 inflate.
## Following is majority voting method to combine the three
set.seed(0)
majority = df %>% dplyr::group_by(`State`) %>%
    summarize(`Majority`=names(table(`Greeness`)[nnet::which.is.max(table(`Greeness`))])) %>%
    {.}
head(majority, n=10)

majority %>%
    group_by(`Majority`) %>%
    summarize(n())

## this one doesn't resolve ties
## majority = df %>% dplyr::group_by(`State`) %>%
##     summarize(`Majority`=names(which.max(table(`Greeness`)))) %>%
##     {.}

df_green_majority = df %>%
  tidyr::spread(`Source`, `Greeness`) %>%
  dplyr::select(`State`, `NCSL`, `WalletHub`, `Forbes`) %>%
  left_join(majority) %>%
  dplyr::arrange(desc(Majority), desc(`NCSL`), desc(`WalletHub`),
                  desc(`Forbes`), State) %>%
  dplyr::select(c(1, 5, 2:4)) %>%
  {.}

df_green_majority %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/greeness_combine_source_majority_vote.csv")

df_green =
  df_green_majority %>%
  dplyr::rename(`Greeness`=`Majority`) %>%
  dplyr::select(State, Greeness) %>%
  {.}

df_green %>%
  readr::write_csv("greeness.csv")

## Fixme: try this
## Another way to combine the three ratings: convert them all to continuous:
## transform each of them to Gaussian Corpula: phi_{Gaussian}(inverse of
## empirical CDF(x))

load("~/Dropbox/thesis/code/pubPriCmp/data/allData.rda")

allDataGreen = allData %>%
  left_join(df_green, by="State") %>%
  {.}

## add new greeness source
load("~/Dropbox/thesis/code/pubPriCmp/data/allDataGreen.rda")

df_green_howe_state <- readxl::read_excel("Howe_et_al_NatClimCh_2015_SI2.xls") %>%
  dplyr::select(`State_abb`, `x65_happening`) %>%
  dplyr::rename(`State`=`State_abb`) %>%
  dplyr::arrange(desc(`x65_happening`)) %>%
  dplyr::mutate(Rank=as.numeric(rownames(.))) %>%
  dplyr::mutate(`greeness.howe.state` = cut(`Rank`, breaks = c(0, 17, 34, 51), labels=c("Most Green", "Moderate Green", "Least Green"))) %>%
  dplyr::select(-Rank) %>%
  dplyr::rename(`x65_happening_state`=`x65_happening`) %>%
  {.}

df_green_howe_state %>%
  readr::write_csv("greeness_howe_state.csv")

allDataGreen <- allDataGreen %>%
  dplyr::left_join(df_green_howe_state, by="State") %>%
  {.}

df_green_howe_county <- readxl::read_excel("Howe_et_al_NatClimCh_2015_SI3.xls") %>%
  dplyr::select(`County_FIPS`, `County_name`, `x65_happening`) %>%
  dplyr::mutate(`County_FIPS`=sprintf("%05d", `County_FIPS`)) %>%
  dplyr::arrange(desc(`x65_happening`)) %>%
  ## add back in DC data
  dplyr::bind_rows(data.frame(`County_FIPS`="11001", `County_name`="Washington, District of Columbia",
                              `x65_happening`=81.4)) %>%
  dplyr::mutate(Rank=as.numeric(rownames(.))) %>%
  dplyr::mutate(`greeness.howe.county` = cut(`Rank`, breaks = c(0, floor(nrow(.)/3), floor(nrow(.)*2/3), nrow(.)), labels=c("Most Gree", "Moderate Green", "Least Green"))) %>%
  dplyr::select(-Rank) %>%
  dplyr::rename(`x65_happening_county`=`x65_happening`) %>%
  {.}

latlon <- allDataGreen %>%
  distinct(latitude, longitude) %>%
  {.}

latlon %>%
  readr::write_csv("latlon.csv")

## ## use http://www.datasciencetoolkit.org/ to find county fips code

## latlon.coded =
##   readr::read_csv("latlon_geocoded.csv") %>%
##   dplyr::filter(`friendly_type`=="county") %>%
##   dplyr::mutate(`County_FIPS`=gsub("_", "", code)) %>%
##   dplyr::select(latitude, longitude, `County_FIPS`, name) %>%
##   {.}

## ## there are duplicated result for the same lat lon
## latlon.coded %>%
##   dplyr::group_by(latitude, longitude) %>%
##   dplyr::filter(n()>1) %>%
##   nrow()

## latlon.coded %>%
##   readr::write_csv("latlon2county_datasciencetoolkit.csv")

library(httr)
## This is for checking on individual request
for (i in 3:3) {
  ## for (i in 1:nrow(latlon)) {
  lat = latlon$latitude[i]
  lon = latlon$longitude[i]
  query_str = sprintf("https://geo.fcc.gov/api/census/area?lat=%f&lon=%f&format=json", lat, lon)
  print(query_str)
  response = httr::GET(query_str)
  if (httr::status_code(response) == "200") {
    print("good response")
    cont = httr::content(response)
    ## print(class(cont))
    print((cont$results)[[1]]$`county_fips`)
  }
}

## This is for actually getting county from FCC API: https://geo.fcc.gov/api/census/#!/area/get_area
library("jsonlite")

acc = NULL
## for (i in 2172:2173) {
for (i in 1:nrow(latlon)) {
  lat = latlon$latitude[i]
  lon = latlon$longitude[i]
  query_str = sprintf("https://geo.fcc.gov/api/census/area?lat=%f&lon=%f&format=json", lat, lon)
  print(query_str)
  tryCatch(
    {
      cont = jsonlite::fromJSON(query_str)$results %>%
                                         dplyr::mutate(latitude = lat, longitude=lon)
      acc <- rbind(acc, cont)
    },
    warning = function (w) {
      print(w)
    },
    error = function (e) {
      print(e)
    },
    finally = {
    }
  )
}

acc %>%
  tibble::as_tibble() %>%
  dplyr::select(-bbox) %>%
  readr::write_csv("area_info_from_fcc.csv")

acc <- readr::read_csv("area_info_from_fcc.csv") %>%
  tibble::as_tibble()

latlon2county <- acc %>%
  tibble::as_tibble() %>%
  dplyr::distinct(latitude, longitude, `county_fips`, `county_name`, `state_code`) %>%
  {.}

latlon2county.nodup <- latlon2county %>%
  dplyr::group_by(latitude, longitude) %>%
  dplyr::filter(n()==1) %>%
  dplyr::ungroup() %>%
  {.}

latlon2county.dup <- latlon2county %>%
  dplyr::group_by(latitude, longitude) %>%
  dplyr::filter(n()>1) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(latitude, longitude) %>%
  {.}

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", latlon2county.nodup, overwrite = TRUE)

## output the counties with multiple county results, currently no way to resolve
## them yet, for now just remove those buildings
## latlon2county %>%
##   dplyr::group_by(latitude, longitude) %>%
##   dplyr::filter(n()>1) %>%
##   dplyr::ungroup() %>%
##   print()

## latlon2county.dup %>%
##   readr::write_csv("latlon2county_conflicting.csv")

## following uses sf package to find the county id, according to
## https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package,
## There's lots of NA's in the result for some reason, if I get the first returned result
library("maptools")
library("sf")

county_shape = sf::st_read("geo_data/tl_2018_us_county/tl_2018_us_county.shp")
## copied from here

# create a points collection
pnts_sf <- do.call(sf::st_sfc,c(lapply(1:nrow(latlon),
                                       function(i) {sf::st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326)))

pnts_trans <- sf::st_transform(pnts_sf, 2163) # apply transformation to pnts sf
county_trans <- sf::st_transform(county_shape, 2163)      # apply transformation to polygons sf

                                        # intersect and extract state name
latlon$county.fips <- apply(sf::st_contains(county_trans, pnts_trans, sparse = FALSE), 2,
                     function(col) {
                       county_trans[which(col), ]$GEOID[1]
                     })

latlon %>%
  readr::write_csv("results_from_sf.csv")

## print all columns
options(tibble.width = Inf)

## this checks whether there's wrong matching
## latlon.coded %>>%
##   (?nrow(.)) %>>%
##   dplyr::left_join(df_green_howe_county, by="County_FIPS") %>>%
##   (?nrow(.)) %>>%
##   dplyr::rowwise() %>%
##   dplyr::mutate(`county.short`=substr(`County_name`, 1, max(max(regexpr(" County", `County_name`)[[1]] - 1,
##                                                             regexpr(" Parish", `County_name`)[[1]] - 1),
##                                       regexpr(" city", `County_name`)[[1]] - 1))) %>%
##   dplyr::ungroup() %>%
##   dplyr::filter(name != county.short) %>%
##   head()

## 91 records with non-agree State in EUAS and state from lat lon

building.conflicting.state <- allDataGreen %>>%
  (?nrow(.)) %>>%
  dplyr::left_join(latlon2county.nodup, by=c("latitude", "longitude")) %>>%
  (?nrow(.)) %>>%
  dplyr::filter(State != `state_code`) %>%
  {.}

building.conflicting.state %>%
  readr::write_csv("conflicting_state_and_latlon.csv")

allDataGreen <- allDataGreen %>>%
  (?nrow(.)) %>>%
  dplyr::left_join(latlon2county.nodup, by=c("latitude", "longitude")) %>>%
  (?nrow(.)) %>>%
  {.}

allDataGreen %>%
  distinct(Name, Organization) %>%
  dplyr::group_by(Organization) %>%
  dplyr::summarise(n())

allDataGreen <- allDataGreen %>>%
  dplyr::filter(!is.na(`county_fips`)) %>%
  {.}

allDataGreen %>%
  distinct(Name, Organization) %>%
  dplyr::group_by(Organization) %>%
  dplyr::summarise(n())

allDataGreen <- allDataGreen %>>%
  dplyr::filter(State == `state_code`) %>%
  {.}

allDataGreen %>%
  distinct(Name, Organization) %>%
  dplyr::group_by(Organization) %>%
  dplyr::summarise(n())

allDataGreen <- allDataGreen %>%
  dplyr::rename(`County_FIPS`=`county_fips`) %>%
  dplyr::select(-`state_code`, -`county_name`) %>%
  dplyr::left_join(df_green_howe_county, by=c("County_FIPS")) %>%
  {.}

summary(allDataGreen)

allDataGreen %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data/allDataGreen.csv")

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", allDataGreen, overwrite = TRUE)
