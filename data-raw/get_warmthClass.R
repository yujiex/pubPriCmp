library("dplyr")
library("readr")

## state full name to abbreviation lookup
df_state_abbr =
  readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/state_abbr.csv") %>%
  {.}

## move to another file start
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
