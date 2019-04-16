library("dplyr")
library("DBI")
library("readr")
library("rlang")
library("pipeR")
library("leaflet")
library("xtable")

## #### ## ## ## ## ## ## ## ## ##
## This file prepares for the event study
## #### ## ## ## ## ## ## ## ## ##

## reload all data
devtools::load_all("~/Dropbox/gsa_2017/db.interface")
gsa_energy =
  db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly",
                                   cols=c("Building_Number", "state_abbr", "Gross_Sq.Ft", "year", "month",
                                          "Electric_(kBtu)", "Gas_(kBtu)"
                                          ##, "Occupancy", "datacenter_sqft", "lab_sqft"
                                          )) %>%
  dplyr::rename(`Name`=`Building_Number`) %>%
  na.omit() %>%
  {.}

## 1010 buildings has information about occupancy, area for data center, and area for lab

## checked for duplicates, no duplicates
## gsa_energy %>%
##   dplyr::group_by(`Name`, `year`, `month`) %>%
##   dplyr::filter(n() > 1) %>%
##   print()

## get a dataframe with retrofit year month

## get the set of buildings with retrofit and date
gsa_retrofit =
  db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_ecm", cols=c("Building_Number", "Substantial_Completion_Date", "high_level_ECM")) %>%
  na.omit(`Substantial_Completion_Date`) %>%
  dplyr::group_by(`Building_Number`, `Substantial_Completion_Date`,
                  `high_level_ECM`) %>%
  slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(`Building_Number`, `high_level_ECM`) %>%
  dplyr::mutate(`retrofit_year` = as.integer(substr(`Substantial_Completion_Date`, start=1, stop=4)),
                `retrofit_month` = as.integer(substr(`Substantial_Completion_Date`, start=6, stop=7))) %>%
  dplyr::rename(`Name`=`Building_Number`, `retrofit_type`=`high_level_ECM`) %>%
  dplyr::select(-`Substantial_Completion_Date`) %>%
  {.}

gsa_retrofit %>%
  dplyr::group_by(`retrofit_type`) %>%
  dplyr::summarise(n()) %>%
  head()

## get the time range of retrofits: 2011 to 2015
## gsa_retrofit %>%
##   dplyr::distinct(`retrofit_year`)

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_ecm", cols=c("Building_Number", "Substantial_Completion_Date", "high_level_ECM", "detail_level_ECM")) %>%
  dplyr::group_by(`high_level_ECM`, `detail_level_ECM`) %>%
    dplyr::filter(!is.na(`Substantial_Completion_Date`)) %>%
    dplyr::summarise(`Building Count`=n_distinct(`Building_Number`)) %>%
    readr::write_csv("nonsingle_retrofit_type_cnt.csv")

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

climateRegionLookup = readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/climateRegionLookup.csv") %>%
  as.data.frame() %>%
  {.}

## state full name to abbreviation lookup
df_state_abbr =
  readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/state_abbr.csv") %>%
  {.}

## read state average temperature to data frame
state_average_temperature_2008to2012 =
  readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/stateAverageTemperature2008to2012.csv") %>%
  tibble::as_data_frame() %>%
  {.}

state_average_temperature_2013to2017 =
  readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/stateAverageTemperature2013to2017.csv") %>%
  tibble::as_data_frame() %>%
  {.}

state_average_temperature =
  state_average_temperature_2008to2012 %>%
  dplyr::bind_rows(state_average_temperature_2013to2017) %>%
  dplyr::select(`Location`, `Value`) %>%
  dplyr::arrange(`Location`) %>%
  dplyr::group_by(`Location`) %>%
  dplyr::summarise(`average_temperature`=mean(`Value`)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(`state_full_name`=`Location`) %>%
  dplyr::left_join(df_state_abbr, by="state_full_name") %>%
  dplyr::rename(`State`=`state_abbr`) %>%
  dplyr::select(-`state_full_name`) %>%
  {.}

head(state_average_temperature)
nrow(state_average_temperature)

## source: https://www.ncdc.noaa.gov/cag/city/mapping/49-tavg-201212-60.csv
## USW00013743,"Washington (Reagan National), Washington, D.C.",59.5,58.2,1.3
## source: https://www.ncdc.noaa.gov/cag/city/mapping/49-tavg-201712-60.csv
## USW00013743,"Washington (Reagan National), Washington, D.C.",59.7,58.2,1.5
dc_average_temperature = data.frame(`State`="DC", `average_temperature`=59.6)

head(dc_average_temperature )

state_average_temperature =
  state_average_temperature %>%
  dplyr::bind_rows(dc_average_temperature) %>%
  {.}

## classify warmth with state average temperature
warmthClass =
  state_average_temperature %>%
  dplyr::arrange(`average_temperature`) %>%
  dplyr::mutate(`row_number`=row_number()) %>%
  dplyr::mutate(`warmth`=ifelse(row_number < nrow(.)/3,
                                "cold", ifelse(row_number < nrow(.) * 2 / 3, "mild", "hot"))) %>%
  dplyr::select(-`row_number`) %>%
  {.}

head(warmthClass)
tail(warmthClass)

## classify warmth with state centroid latitude
boundary_low = 36.9
boundary_high = 42
warmthClass_lat =
  readr::read_csv("state_lat_lng.csv") %>%
  tibble::as_data_frame() %>%
  dplyr::rename(`state_full_name`=`State`) %>%
  dplyr::left_join(df_state_abbr, by="state_full_name") %>%
  dplyr::rename(`State`=`state_abbr`) %>%
  dplyr::select(-`state_full_name`) %>%
  dplyr::mutate(`warmth`=ifelse(`Latitude`>boundary_high, "cold",
                                ifelse(`Latitude`<boundary_low, "hot", "mild"))) %>%
  {.}
devtools::use_data(warmthClass_lat, pkg="~/Dropbox/thesis/code/pubPriCmp", overwrite = TRUE)

## show average temperature of each warmth class
warmthClass %>%
  dplyr::group_by(`warmth`) %>%
  summarise(`average temperature` = mean(`average_temperature`), `states`=paste(`State`, collapse = ",")) %>%
  readr::write_csv("average_temperature_by_warmth.csv")

devtools::use_data(warmthClass, pkg="~/Dropbox/thesis/code/pubPriCmp", overwrite = TRUE)

gsa_building_retrofit =
  gsa_energy %>%
  dplyr::left_join(gsa_retrofit, by="Name") %>%
  na.omit() %>%
  dplyr::left_join(gsa_lat_lon, by="Name") %>%
  dplyr::left_join(gsa_city, by="Name") %>%
  dplyr::rename(`State`=`state_abbr`) %>%
  dplyr::left_join(climateRegionLookup, by="State") %>%
  dplyr::left_join(warmthClass, by="State") %>%
  dplyr::mutate(`Organization`="GSA") %>%
  dplyr::rename(`GSF`=`Gross_Sq.Ft`) %>%
  ## restrict to continental US
  dplyr::filter(!State %in% c("AK", "HI", "VI", "GU", "PR", "VI")) %>%
  {.}

## there are 301 buildings with retrofits
length(unique(gsa_building_retrofit$Name))

## Following checks which state misses average temperature after joining with warmthClass: DC
gsa_building_retrofit %>%
  dplyr::filter(is.na(`average_temperature`)) %>%
  dplyr::select(`State`) %>%
  distinct(`State`)

buildingLatlng = gsa_building_retrofit %>%
  dplyr::select(-`Electric_(kBtu)`, -`Gas_(kBtu)`) %>%
  dplyr::group_by(`Name`) %>%
  slice(1) %>%
  ungroup() %>%
  {.}

pal <- colorFactor(palette = 'Dark2', domain = buildingLatlng$retrofit_type)

## do map of buildings with retrofits
buildingLatlng %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lat=~latitude, lng=~longitude, color=~ pal(retrofit_type),
             label=~sprintf("%s,(%s,%s)", Name, retrofit_type, latitude, longitude)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLegend("bottomright", pal = pal, values = ~retrofit_type,
            title = "Retrofit type",
            opacity = 1)

## check on retrofit time and decide what range of weather to get
## 2010-03 to 2015-10
gsa_retrofit %>%
  dplyr::arrange(`retrofit_year`, `retrofit_month`) %>%
  head()
gsa_retrofit %>%
  dplyr::arrange(`retrofit_year`, `retrofit_month`) %>%
  tail()

## the earliest is 2010-03, latest is 2015-10, download 5 years before and 5 years after if there are 5 years
## but maybe just use the 2 years before and after?

## get weather
load("~/Dropbox/thesis/code/pubPriCmp/data/ghcnd_data_full.rda")

buildings = unique(gsa_building_retrofit$Name)

## buildings = readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_without_tmin.csv") %>%
##   .$Name

duration = "years"
## all_times = seq(as.Date("2011-01-01"), as.Date("2014-01-01"), duration)
## start_times = all_times[1:(length(all_times) - 1)]
## end_times = all_times[2:length(all_times)]
start_times = seq(as.Date("2005-01-01"), as.Date("2017-01-01"), duration)
end_times = seq(as.Date("2005-12-31"), as.Date("2017-12-31"), duration)

download_rounds = length(start_times)

v = "TMIN"
## only search regarding the variables interested
ghcnd_data_var = ghcnd_data_full %>%
  dplyr::filter(`element` == v) %>%
  {.}

devtools::load_all("~/Dropbox/gsa_2017/get.noaa.weather")

## fixme: add error handling for
## Error in curl::curl_fetch_memory(url, handle = handle) :
##            Recv failure: Operation timed out
for (j in 1:download_rounds) {
  ## for (j in 1:length(start_times)) {
  date_min = start_times[j]
  date_max = end_times[j]
  bad = NULL
  if (file.exists(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/noaa/bad_%s_%s.csv", v, date_min))) {
    bad_acc = readr::read_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/noaa/bad_%s_%s.csv", v, date_min))$bad
  } else {
    bad_acc = NULL
  }
  good_ghcnd <- ghcnd_data_var %>%
    dplyr::filter(!(`id` %in% bad_acc))
  start = 1
  counter = start
  for (b in buildings) {
  ## for (b in buildings[start:length(buildings)]) {
    ## this part only downloads data
    if (!file.exists(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_station_distance_%s_%s.csv",
                            v, b, v, date_min))) {
      print(sprintf("%s --------------%s -----------", counter, b))
      b_loc = buildingLatlng %>%
        dplyr::filter(`Name`==b) %>%
        {.}
      print(b_loc)
      acc <- NULL
      good_ghcnd <- good_ghcnd %>%
        dplyr::filter(!(`id` %in% bad)) %>%
        {.}
      print(sprintf("size of search space: %s", nrow(good_ghcnd)))
      result = get_nearby_ghcnd_stations_one_loc(lat_lon_df=b_loc, id_col_name="Name", ghcnd_data=good_ghcnd, v=v,
                                        radius=100, limit=3,
                                        date_min=date_min, date_max=date_max,
                                        year=NULL)
      print("final result---------")
      print(result$df)
      bad <- result$bad
      bad_acc <- c(bad_acc, bad)
      data.frame(bad=bad_acc) %>%
        readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/noaa/bad_%s_%s.csv", v, date_min))
      result$df %>%
        readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_station_distance_%s_%s.csv",
                                v, b, v, date_min))
    ## when weather file is already downloaded
    } else if (!file.exists(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_%s_%s.csv",
                                    v, b, v, date_min))){
      print("compile weather for building")
      print(sprintf("%s --------------%s -----------", counter, b))
      df <- readr::read_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_station_distance_%s_%s.csv", v, b, v, date_min))
      weatheri = compile_weather_ghcnd_main(building=b, station_df=df,
                                date_min=date_min, date_max=date_max,
                                var=v,
                                format_fun=get.noaa.weather::format_noaa_temperature)
      print(weatheri)
      weatheri %>%
        readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_%s_%s.csv", v, b, v, date_min))
    ## when weather output is compiled
    } else {
      print(sprintf("%s file exists for %s", v, b))
    }
    counter = counter + 1
  }
  ## acc %>%
  ##   readr::write_csv(sprintf("%s_%s.csv", b, v))
}
## #### ## ## ## ## ## ## ## ## ##
## a slow version of downloading weather data end
## #### ## ## ## ## ## ## ## ## ##

length(buildings)

## compile individual building's weather data to one data frame
for (b in buildings) {
  ## for (b in buildings[start:length(buildings)]) {
  print(b)
  acc = NULL
  for (v in c("TMIN", "TMAX")) {
    for (j in 1:download_rounds) {
      date_min = start_times[j]
      filename = sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_%s/%s_%s_%s.csv", v, b, v,
                         date_min)
      if (file.exists(filename)) {
        dfvar <- readr::read_csv(filename, col_types=cols()) %>%
          dplyr::mutate(`varname`=v) %>%
          tibble::as_data_frame() %>%
          {.}
        acc <- rbind(acc, dfvar)
      } else {
        print(sprintf("file not exist %s_%s_%s.csv", b, v, date_min))
      }
    }
  }
  acc %>%
    dplyr::arrange(`date`, `varname`) %>%
    dplyr::group_by(`date`, `varname`) %>%
    slice(n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(varname, weighted) %>%
    dplyr::mutate(`AVGMINMAX` = (`TMIN` + `TMAX`) / 2) %>%
    dplyr::mutate(`HDD` = ifelse(`AVGMINMAX` <= 65, 65 - `AVGMINMAX`, 0)) %>%
    dplyr::mutate(`CDD` = ifelse(`AVGMINMAX` > 65, `AVGMINMAX` - 65, 0)) %>%
    feather::write_feather(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_AVGMINMAX_05_to_17/%s.feather", b))
  ## readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_AVGMINMAX/%s.csv", b))
}

## combine all buildings' weather data into one file
acc = NULL
for (b in buildings) {
  print(b)
  df <- feather::read_feather(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/building_AVGMINMAX_05_to_17/%s.feather", b)) %>%
    dplyr::mutate(`Name`=b) %>%
    {.}
  acc <- rbind(acc, df)
}
acc %>% feather::write_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/retrofit_all_weather.feather")

acc = feather::read_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/retrofit_all_weather.feather")

## produce table with columns of AVGMINMAX bins
lowerbound = min(acc$AVGMINMAX)
upperbound = max(acc$AVGMINMAX)

## check distribution of weather data
acc %>%
  ggplot2::ggplot(ggplot2::aes(x=`AVGMINMAX`)) +
  ggplot2::geom_histogram()

breaks = c(lowerbound, seq(10, 90, by=10), upperbound)
break_labels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90")

## check distribution of average temperature
acc %>%
  dplyr::select(`date`, `Name`, `AVGMINMAX`) %>%
  dplyr::mutate(`value_label`=dplyr::case_when(`AVGMINMAX` < 10 ~ "<10",
                                               `AVGMINMAX` < 20 ~ break_labels[2],
                                               `AVGMINMAX` < 30 ~ break_labels[3],
                                               `AVGMINMAX` < 40 ~ break_labels[4],
                                               `AVGMINMAX` < 50 ~ break_labels[5],
                                               `AVGMINMAX` < 60 ~ break_labels[6],
                                               `AVGMINMAX` < 70 ~ break_labels[7],
                                               `AVGMINMAX` < 80 ~ break_labels[8],
                                               `AVGMINMAX` < 90 ~ break_labels[9],
                                               TRUE ~ break_labels[10]
                                               )) %>%
  dplyr::mutate(`value_label`=factor(`value_label`, levels=break_labels)) %>%
  ggplot2::ggplot(ggplot2::aes(x=value_label)) +
  ggplot2::ggtitle("Daily mean temperature distribution for buildings with the 6 retrofit types") +
  ggplot2::xlab("Mean Temperature Bin") +
  ggplot2::ylab("Number of days from 2005 to 2017") +
  ggplot2::geom_bar()
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/nonsingle_retrofit_building_temp_distribution.png", width=8, height=5, units="in")
## looks alright

avgminmax_bin = acc %>%
  dplyr::select(`date`, `Name`, `AVGMINMAX`) %>%
  dplyr::mutate(`value_label`=dplyr::case_when(`AVGMINMAX` < 10 ~ "<10",
                                               `AVGMINMAX` < 20 ~ break_labels[2],
                                               `AVGMINMAX` < 30 ~ break_labels[3],
                                               `AVGMINMAX` < 40 ~ break_labels[4],
                                               `AVGMINMAX` < 50 ~ break_labels[5],
                                               `AVGMINMAX` < 60 ~ break_labels[6],
                                               `AVGMINMAX` < 70 ~ break_labels[7],
                                               `AVGMINMAX` < 80 ~ break_labels[8],
                                               `AVGMINMAX` < 90 ~ break_labels[9],
                                               TRUE ~ break_labels[10]
                                               )) %>%
  dplyr::mutate(`value_label`=factor(`value_label`, levels=break_labels)) %>%
  dplyr::mutate(`year`=format(date, "%Y")) %>%
  dplyr::mutate(`month`=format(date, "%m")) %>%
  dplyr::select(-`date`) %>%
  dplyr::group_by(`Name`, `year`, `month`, `value_label`) %>%
  dplyr::summarise(`value_count`=n()) %>%
  dplyr::ungroup() %>%
  tidyr::spread(key=`value_label`, value=`value_count`) %>%
  dplyr::mutate_all(funs(ifelse(is.na(.), 0L, .))) %>%
  {.}

avgminmax_bin %>%
  feather::write_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/retrofit_avgminmax_bin.feather")

## compile monthly degree day
hddcdd = acc %>%
  dplyr::select(`date`, `Name`, `HDD`, `CDD`) %>%
  dplyr::mutate(`year`=format(date, "%Y")) %>%
  dplyr::mutate(`month`=format(date, "%m")) %>%
  dplyr::select(-`date`) %>%
  dplyr::group_by(`Name`, `year`, `month`) %>%
  dplyr::summarise(`HDD`=sum(`HDD`), `CDD`=sum(`CDD`)) %>%
  dplyr::ungroup() %>%
  {.}

hddcdd %>%
  feather::write_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/retrofit_hddcdd.feather")

building_weather = avgminmax_bin %>%
  dplyr::left_join(hddcdd, by=c("Name", "year", "month")) %>%
  {.}

## get all temperature bin and building weather variables
building_weather %>%
  feather::write_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/retrofit_building_weather.feather")

## PA0060ZZ has 0 sqft
## PA0064ZZ's energy data is before 2004, dropped from the analysis
nonsingle_retrofit_allData =
  gsa_building_retrofit %>%
  dplyr::mutate(year = as.character(year), month=sprintf("%02d", month)) %>%
  dplyr::left_join(hddcdd, by=c("Name", "year", "month")) %>%
  dplyr::left_join(avgminmax_bin, by=c("Name", "year", "month")) %>%
  dplyr::mutate(`eui_elec` = `Electric_(kBtu)` / `GSF`,
                `eui_gas` = `Gas_(kBtu)` / `GSF`) %>%
  dplyr::mutate(`eui_elec_ln` = log(`eui_elec` + 1),
                `eui_gas_ln` = log(`eui_gas` + 1)) %>%
  dplyr::mutate(`HDD2`=`HDD`^2, `CDD2`=`CDD`^2) %>%
  dplyr::mutate(`Date`=zoo::as.yearmon(paste(`year`, `month`), "%Y %m")) %>%
  dplyr::mutate(`Date`=as.character(`Date`)) %>%
  dplyr::mutate(`Retrofit_Date`=zoo::as.yearmon(paste(`retrofit_year`, `retrofit_month`), "%Y %m")) %>%
  dplyr::mutate(`Retrofit_Date`=as.character(`Retrofit_Date`)) %>%
  na.omit() %>%
  {.}

length(unique(nonsingle_retrofit_allData$Name))

warmthClass_to_join = warmthClass_lat %>%
  dplyr::select(-`Latitude`, -`Longitude`) %>%
  dplyr::rename(`warmth_stateLat`=`warmth`) %>%
  {.}
names(warmthClass_to_join)

nonsingle_retrofit_allData <-
  nonsingle_retrofit_allData %>%
  dplyr::left_join(warmthClass_to_join, by="State") %>%
  {.}

nonsingle_retrofit_allData <-
  nonsingle_retrofit_allData %>%
  dplyr::mutate(`warmth_stateLat`=ifelse((`State`=="CA") & (`latitude` > boundary_low), "mild", `warmth_stateLat`)) %>%
  {.}

acc = feather::read_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/retrofit_all_weather.feather")

df_mean_avgminmax <-
  acc %>%
  dplyr::select(`date`, `AVGMINMAX`, `Name`) %>%
  dplyr::mutate(`year`=format(`date`, "%Y"),
                `month`=format(`date`, "%m")) %>%
  dplyr::group_by(`year`, `month`, `Name`) %>%
  dplyr::summarise(`mean_avgminmax`=mean(`AVGMINMAX`)) %>%
  dplyr::ungroup() %>%
  {.}

head(df_mean_avgminmax)

building_mean_temp <- df_mean_avgminmax %>%
  dplyr::group_by(`Name`) %>%
  dplyr::summarise(`mean_avgminmax`=mean(`mean_avgminmax`)) %>%
  dplyr::ungroup() %>%
  {.}

building_mean_temp %>%
  readr::write_csv("building_mean_temperature_05to17.csv")

cutoffs = quantile(building_mean_temp$mean_avgminmax, probs=c(1/3, 2/3))

warmthClass_building_monthTemp <-
  building_mean_temp %>%
  dplyr::mutate(`warmth_buildingTemp`=ifelse(`mean_avgminmax`<cutoffs[[1]], "cold",
                                      ifelse(`mean_avgminmax`<cutoffs[[2]], "mild", "hot"))) %>%
  dplyr::select(-`mean_avgminmax`) %>%
  {.}

head(warmthClass_building_monthTemp)

nonsingle_retrofit_allData <-
  nonsingle_retrofit_allData %>%
  ## dplyr::left_join(df_mean_avgminmax, by=c("year", "month", "Name")) %>%
  dplyr::left_join(warmthClass_building_monthTemp, by="Name") %>%
  {.}

names(nonsingle_retrofit_allData)

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", nonsingle_retrofit_allData, overwrite = TRUE)

nonsingle_retrofit_allData %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data/nonsingle_retrofit_allData.csv")

## make retrofit wide format
nonsingle_retrofit_allData %>%
  ## dplyr::select(`Name`, `retrofit_type`, `Retrofit_Date`, `Date`) %>%
  dplyr::mutate(`Retrofit_Date`=zoo::as.yearmon(`Retrofit_Date`),
                `Date`=zoo::as.yearmon(`Date`)) %>%
  dplyr::mutate(`post_retrofit`=as.integer(`Date`>`Retrofit_Date`)) %>%
  tidyr::spread(`retrofit_type`, `post_retrofit`, fill=0) %>%
  readr::write_csv("../data/additiveRetrofit.csv")

## print how many buildings with one retrofit in each warmth class
warmth_col = "warmth"
warmth_method = "state temperature"
## warmth_col = "warmth_stateLat"
## warmth_method = "state latitude"
## warmth_col = "warmth_buildingTemp"
## warmth_method = "building temperature"
dfcnt =
  nonsingle_retrofit_allData %>%
  dplyr::select(`Name`, `retrofit_type`, !!(rlang::sym(warmth_col))) %>%
  dplyr::group_by(`retrofit_type`, !!(rlang::sym(warmth_col)), `Name`) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(`retrofit_type`, !!(rlang::sym(warmth_col))) %>%
  dplyr::summarise(building_count = n()) %>%
  dplyr::ungroup() %>%
  tidyr::spread(!!(rlang::sym(warmth_col)) ,`building_count`) %>%
  dplyr::rename(`Retrofit Type`=`retrofit_type`) %>%
  {.}
dfcnt %>%
  readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/cnt_nonsingle_retrofit_by_%s.csv", warmth_col))

result_table_tex <- xtable(dfcnt, caption=sprintf("Building count by retrofit and warmth class using %s", warmth_method))
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/cnt_by_nonsingle_retrofit_%s.tex", warmth_col), size="\\footnotesize")

## year  month
## <chr> <chr>
## 1 2005  01   
## 2 2017  09   
## get time range of the study set
nonsingle_retrofit_allData %>%
  dplyr::select(`year`, `month`) %>%
  dplyr::arrange(`year`, `month`) %>%
  dplyr::group_by_all() %>%
  slice(1) %>%
  dplyr::ungroup() %>%
  slice(c(1, n()))

## retrofit_year retrofit_month
## <int>          <int>
## 1          2010              7
## 2          2015              9
## get time range of retrofit
nonsingle_retrofit_allData %>%
  dplyr::select(`retrofit_year`, `retrofit_month`) %>%
  dplyr::arrange(`retrofit_year`, `retrofit_month`) %>%
  dplyr::group_by_all() %>%
  slice(1) %>%
  dplyr::ungroup() %>%
  slice(c(1, n()))

## get the distance to the retrofit
retrofit_allData %>%
  dplyr::select(`year`, `month`, `retrofit_year`, `retrofit_month`) %>%
  dplyr::mutate(`date`=zoo::as.yearmon(sprintf("%s-%s", year, month))) %>%
  dplyr::mutate(`retrofit_date`=zoo::as.yearmon(sprintf("%s-%s", `retrofit_year`, `retrofit_month`))) %>%
  dplyr::mutate(`diff`=(`date` - `retrofit_date`) * 12) %>%
  ggplot2::ggplot(ggplot2::aes(x=diff)) +
  ggplot2::ylab(NULL) +
  ggplot2::xlab("Distance to retrofit time") +
  ggplot2::geom_histogram(binwidth = 10) +
  ggplot2::theme_bw()
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/eventstudy_tau_distribution.png", width=8, height=5, units="in")

head(retrofit_allData)

load("~/Dropbox/thesis/code/pubPriCmp/data/retrofit_allData.rda")
## get event study input with monthly level data
endbound = 60
monthly_retrofit_allData =
  retrofit_allData %>%
  dplyr::mutate(`tau`=(zoo::as.yearmon(`Date`) - zoo::as.yearmon(`Retrofit_Date`))*12) %>%
  ## end-point restriction
  dplyr::mutate(`tau_bounded`=ifelse(`tau` < (-1)*endbound, (-1)*endbound,
                              ifelse(`tau` > endbound, endbound, `tau`))) %>%
  dplyr::mutate(`tau_bounded_post`=ifelse(`tau_bounded` > 0, `tau_bounded`, 0)) %>%
  ## assume (-1)*endbound >= min(`tau_bounded`)
  dplyr::mutate(`tau_bounded_shift` = ifelse(`tau_bounded` < 0, `tau_bounded` + 2 * endbound + 1, `tau_bounded`)) %>%
  {.}

names(monthly_retrofit_allData)

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", monthly_retrofit_allData, overwrite = TRUE)

monthly_retrofit_allData %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data/monthly_retrofit_allData.csv")

load("~/Dropbox/thesis/code/pubPriCmp/data/retrofit_allData.rda")

## get event study study quarterly level data
endbound = 20
quarterly_nonsingle_retrofit_allData =
  nonsingle_retrofit_allData %>%
  dplyr::mutate(`tau`=as.integer(((zoo::as.yearmon(`Date`) - zoo::as.yearmon(`Retrofit_Date`))*12) / 3)) %>%
  ## end-point restriction
  dplyr::mutate(`tau_bounded`=ifelse(`tau` < (-1)*endbound, (-1)*endbound,
                              ifelse(`tau` > endbound, endbound, `tau`))) %>%
  dplyr::mutate(`tau_bounded_post`=ifelse(`tau_bounded` > 0, `tau_bounded`, 0)) %>%
  ## assume (-1)*endbound >= min(`tau_bounded`)
  dplyr::mutate(`tau_bounded_shift` = ifelse(`tau_bounded` < 0, `tau_bounded` + 2 * endbound + 1, `tau_bounded`)) %>%
  {.}

head(quarterly_nonsingle_retrofit_allData)
devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", quarterly_nonsingle_retrofit_allData, overwrite = TRUE)

quarterly_nonsingle_retrofit_allData %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data/quarterly_nonsingle_retrofit_allData.csv")

## get annual data of retrofit_allData
fullyear_retrofit_allData = retrofit_allData %>%
  dplyr::group_by(`Name`, `year`) %>%
  ## only take full year data, in case total consumption and degree day favors some season
  dplyr::filter(n() == 12) %>%
  dplyr::ungroup() %>%
  {.}

fullyear_retrofit_allData_static =
  fullyear_retrofit_allData %>%
  dplyr::select(`Name`, `year`, `State`, `GSF`, `retrofit_type`, `retrofit_year`, `latitude`, `longitude`, `City`,
                `US Climate Region`, `average_temperature`, `warmth`, `Organization`) %>%
  dplyr::group_by(`Name`, `year`) %>%
  dplyr::summarise_all(first) %>%
  {.}

fullyear_retrofit_allData_cum =
  fullyear_retrofit_allData %>%
  dplyr::select(`Name`, `year`, `Electric_(kBtu)`, `Gas_(kBtu)`, `HDD`, `CDD`, `<10`, `[10-20)`, `[20-30)`,
                `[30-40)`, `[40-50)`, `[50-60)`, `[60-70)`, `[70-80)`, `[80-90)`, `>90`, `eui_elec`, `eui_gas`) %>%
  dplyr::group_by(`Name`, `year`) %>%
  dplyr::summarise_all(sum) %>%
  {.}

yearly_total_retrofit_allData = fullyear_retrofit_allData_static %>%
  dplyr::inner_join(fullyear_retrofit_allData_cum, by=c("Name", "year")) %>%
  dplyr::mutate(`eui_elec_ln` = log(`eui_elec` + 1),
                `eui_gas_ln` = log(`eui_gas` + 1),
                `HDD2` = `HDD`^2,
                `CDD2` = `CDD`^2) %>%
  {.}

## drop the columns not used
yearly_total_retrofit_allData = yearly_total_retrofit_allData %>%
  dplyr::select(-`latitude`, -`longitude`, -`US Climate Region`, -`average_temperature`) %>%
  {.}

yearly_total_retrofit_allData %>%
  dplyr::mutate(`tau`=as.numeric(year) - as.numeric(`retrofit_year`)) %>%
  dplyr::select(`tau`, `year`, `retrofit_year`) %>%
  ggplot2::ggplot(ggplot2::aes(x=tau)) +
  ggplot2::ylab(NULL) +
  ggplot2::xlab("Distance to retrofit time") +
  ggplot2::geom_histogram(binwidth = 2) +
  ggplot2::theme_bw()
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/eventstudy_tau_yearly_distribution.png", width=8, height=5, units="in")

endbound = 5
yearly_total_retrofit_allData =
  yearly_total_retrofit_allData %>%
  dplyr::mutate(`tau`=as.numeric(year) - as.numeric(`retrofit_year`)) %>%
  ## end-point restriction
  dplyr::mutate(`tau_bounded`=ifelse(`tau` < (-1)*endbound, (-1)*endbound,
                             ifelse(`tau` > endbound, endbound, `tau`))) %>%
  dplyr::mutate(`tau_bounded_post`=ifelse(`tau_bounded` > 0, `tau_bounded`, 0)) %>%
  ## assume (-1)*endbound >= min(`tau_bounded`)
  dplyr::mutate(`tau_bounded_shift` = ifelse(`tau_bounded` < 0, `tau_bounded` + 2 * endbound + 1, `tau_bounded`)) %>%
  {.}

yearly_total_retrofit_allData %>%
  dplyr::filter(retrofit_type == "Advanced Metering",
                warmth == "mild") %>%
  .$`tau` %>%
  summary()


yearly_total_retrofit_allData %>%
  dplyr::filter(`tau_bounded_shift` == 6) %>%
  head()

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", yearly_total_retrofit_allData, overwrite = TRUE)

yearly_total_retrofit_allData %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data/yearly_total_retrofit_allData.csv")

load("../data/retrofit_allData.rda")

## get table of retrofit sub category count
study_set = retrofit_allData %>%
  dplyr::filter(`retrofit_type`!="Building Tuneup or Utility Improvements") %>%
  distinct(Name) %>%
  .$Name

devtools::load_all("~/Dropbox/gsa_2017/db.interface")

db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_ecm", cols=c("Building_Number", "Substantial_Completion_Date", "high_level_ECM", "detail_level_ECM")) %>%
  na.omit(`Substantial_Completion_Date`) %>%
  dplyr::filter(`Building_Number` %in% study_set) %>%
    dplyr::distinct(`Building_Number`, `high_level_ECM`, `detail_level_ECM`) %>%
    dplyr::group_by(`high_level_ECM`, `detail_level_ECM`) %>%
    dplyr::summarise(`building count of sub category`=n()) %>%
    dplyr::ungroup() %>%
    readr::write_csv("retrofit_sub_category_count.csv")

## following compiles a retrofit anticipated savings for the study set
anticipated_lightTouch = readxl::read_excel("March_Updated_Light-Touch M_V - ARRA Targets to Actuals and Commissioning Details.xlsx", sheet=1, skip=3) %>%
  dplyr::filter(`Building ID` %in% study_set) %>%
  dplyr::filter(!is.na(`gBUILD Target EUI\n(kBTU/GSF)`),
                !is.na(`gBUILD  Baseline EUI\n(kBTU/GSF)`)) %>%
  dplyr::group_by(`Building ID`) %>%
  dplyr::filter(`Project Name`!="WA, Spokane Fed Bldg U S Post Office") %>%
  dplyr::ungroup() %>%
  dplyr::rename(`Name`=`Building ID`) %>%
  {.}

anticipated_hpgb =
  readr::read_csv("Portfolio HPGB Dashboard 12-18-2015_TargetPerformance.csv", skip=3) %>%
  dplyr::filter(`Building ID` %in% study_set) %>%
  dplyr::rename(`Name`=`Building ID`) %>%
  dplyr::select(`Name`, `Project Name`, `Baseline Electricity (mmBTU)`, `Baseline Gas (mmBTU)`,
                `Target Electricity (mmBTU)`, `Target Gas (mmBTU)`) %>%
  dplyr::group_by(`Name`) %>%
  dplyr::filter(`Project Name`!="WA, Spokane Fed Bldg U S Post Office") %>%
  dplyr::ungroup() %>%
  dplyr::select(-`Project Name`) %>%
  na.omit() %>%
  dplyr::mutate(`Target % Electricity Reduction`=sprintf("%.2f%%", (`Target Electricity (mmBTU)` - `Baseline Electricity (mmBTU)`)/`Baseline Electricity (mmBTU)` * (-100)),
                `Target % Gas Reduction`=sprintf("%.2f%%", (`Target Gas (mmBTU)` - `Baseline Gas (mmBTU)`)/`Baseline Gas (mmBTU)` * (-100))) %>%
  {.}

anticipated_hpgb <- anticipated_hpgb %>%
  {.}

anticipated_lightTouch <- anticipated_lightTouch %>%
  dplyr::select(`Name`, `gBUILD  Baseline EUI\n(kBTU/GSF)`, `gBUILD Target EUI\n(kBTU/GSF)`,
                `Target Reduction: \ngBUILD Baseline to gBUILD Target\n(Columns J & K)`) %>%
  {.}

retrofit_allData %>%
  dplyr::distinct(`Name`, `retrofit_year`, `retrofit_month`, `retrofit_type`) %>%
  dplyr::left_join(anticipated_lightTouch, by="Name") %>%
  dplyr::left_join(anticipated_hpgb, by="Name") %>%
  dplyr::arrange(`retrofit_type`, `Name`, `retrofit_year`, `retrofit_month`) %>%
  readr::write_csv("anticipated_saving_study_set.csv")
