library("dplyr")
library("readr")
library("pipeR")
library("feather")
library("ggplot2")
library("readxl")
library("xtable")
library("stringi")
library("zoo")
library("MASS")

load("~/Dropbox/thesis/code/pubPriCmp/data/allData.rda")

load("~/Dropbox/thesis/code/pubPriCmp/data/allDataGreen.rda")

## EDA on buildings in East North Central region GSA portfolio
allDataGreen %>%
  dplyr::select(`Name`, `Organization`, `eui_total`, `year`) %>%
  dplyr::group_by(`Organization`, `Name`, `year`) %>%
  dplyr::summarise(`eui_total`=sum(`eui_total`)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(`Organization`, `Name`) %>%
  dplyr::summarise(`mean_annual_eui_total`=mean(`eui_total`)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot() +
  ## ggplot2::geom_boxplot(ggplot2::aes(y=`mean_annual_eui_total`, x=`Organization`)) +
  ## ggplot2::ylim(c(0, 500)) +
  ggplot2::geom_density(ggplot2::aes(x=`mean_annual_eui_total`, fill=`Organization`)) +
  ggplot2::facet_wrap(.~`Organization`, ncol=1) +
  ggplot2::theme()

allDataGreen %>%
  dplyr::select(`Name`, `Organization`, `eui_total`, `year`, `month`) %>%
  dplyr::group_by(`Organization`, `Name`, `year`) %>%
  dplyr::summarise(`eui_total`=sum(`eui_total`)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(`Organization`, `Name`) %>%
  dplyr::summarise(`mean_annual_eui_total`=mean(`eui_total`)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(`mean_annual_eui_total` > 1000)

allDataGreen %>%
  dplyr::select(`Name`, `Organization`, `eui_elec`, `eui_gas`, `year`, `month`, `GSF`) %>%
  dplyr::filter(Name == "DC0001ZZ")

allDataGreen %>%
  dplyr::distinct(`Name`, `GSF`) %>%
  summary()

eastNorthCentral = allData %>%
  dplyr::filter(`US Climate Region`=="East North Central") %>%
  {.}

## plot energy trend
eastNorthCentral %>%
  dplyr::mutate(`Date`=as.Date(sprintf("%s-%s-01", year, month), "%Y-%m-%d")) %>%
  dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
  dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=`Date`, y=`eui_elec_ln`, color=`Organization`, group=`Date`)) +
  ggplot2::geom_violin() +
  ggplot2::facet_wrap(.~`Organization`, ncol=1) +
  ggplot2::ylab("Electricity in kBtu / gross sqft") +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
                 legend.position = "bottom")
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/eastNorthCentral_distribution_by_time.png", width=6, height=4, units="in")

df_mean_avgminmax = feather::read_feather("building_mean_avgminmax.feather")

## plot public vs private portfolio average consumption by month
allData %>%
  dplyr::filter(`Organization`=="GSA") %>%
  dplyr::group_by(`Name`) %>%
  dplyr::filter(sum(`>90`) > 0) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(`Organization`, `Name`, `month`) %>%
  dplyr::summarise(`eui_elec_ln`=mean(`eui_elec_ln`)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x=`month`, y=`eui_elec_ln`, color=`Organization`, group=`month`)) +
  ## ggplot2::geom_line() +
  ## ggplot2::geom_line() +
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(.~`Organization`) +
  ggplot2::theme_bw()

## plot individual building kernel smooth energy given mean monthly temperature
set.seed(0)
ratio = 0.2
number = 40
region = "East North Central"
## region = "Central"
## region = "Northeast"
## region = "Southeast"
samples =
  allData %>%
  dplyr::filter(`US Climate Region`==region) %>%
  dplyr::select(`Name`, `Organization`) %>%
  distinct(`Organization`, `Name`) %>%
  dplyr::group_by(`Organization`) %>%
  dplyr::slice(sample(1:n(), 30, replace=FALSE)) %>%
  ## dplyr::slice(sample(1:n(), as.integer(n()*ratio))) %>%
  dplyr::ungroup() %>%
  .$Name

## plot for all building
allData %>%
  dplyr::left_join(df_mean_avgminmax, by=c("Name", "year", "month")) %>%
  dplyr::filter(`US Climate Region`==region) %>%
  dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
  dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
  dplyr::group_by(`Name`) %>%
  dplyr::mutate(`total_elec_eui`=sum(`eui_elec_ln`)) %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(ggplot2::aes(x=`mean_avgminmax`, y=`eui_elec_ln`, group=`Name`, color=`total_elec_eui`)) +
  ## ggplot2::ggplot(ggplot2::aes(x=`mean_avgminmax`, y=`eui_elec_ln`, group=`Name`)) +
  ggplot2::geom_point(size=0.25) +
  ggplot2::geom_smooth(span=1.0, se=FALSE, size=0.5) +
  ggplot2::facet_wrap(~`Organization`) +
  ggplot2::xlab("monthly average of daily mean temperature") +
  ggplot2::ylab("Electricity in kBtu / gross sqft") +
  ggplot2::ggtitle(sprintf("kernel smoothing of building electricity (kBtu)/sqft on monthly mean temperature (%s)",
                           region)) +
  ggplot2::theme_bw()
ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/%s_smooth_energy_avgtemp_all.png", gsub(" ", "", region)), width=6, height=4, units="in")

## plot a random sample of buildings
allData %>%
  dplyr::left_join(df_mean_avgminmax, by=c("Name", "year", "month")) %>%
  dplyr::filter(`Name` %in% samples) %>%
  dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
  dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
  dplyr::group_by(`Name`) %>%
  dplyr::mutate(`total electricity per sqft`=sum(`eui_elec_ln`)) %>%
  dplyr::ungroup() %>%
  ## ggplot2::ggplot(ggplot2::aes(x=`mean_avgminmax`, y=`eui_elec_ln`, group=`Name`)) +
  ggplot2::ggplot(ggplot2::aes(x=`mean_avgminmax`, y=`eui_elec_ln`, group=`Name`, color=`total_elec_eui`)) +
  ggplot2::geom_point(size=0.25) +
  ggplot2::geom_smooth(span=1.0, se=FALSE, size=0.5) +
  ggplot2::facet_wrap(~`Organization`) +
  ggplot2::xlab("monthly average of daily mean temperature") +
  ggplot2::ylab("Electricity in kBtu / gross sqft") +
  ggplot2::ggtitle(sprintf("kernel smoothing of building electricity (kBtu)/sqft on monthly mean temperature (%s)",
                           region)) +
  ggplot2::theme_bw()
ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/%s_smooth_energy_avgtemp_sample.png", gsub(" ", "", region)), width=6, height=4, units="in")

## find buildings in the sample with a negative slope
downslope_buildings =
  allData %>%
  dplyr::left_join(df_mean_avgminmax, by=c("Name", "year", "month")) %>%
  dplyr::group_by(`Organization`, `Name`) %>%
  dplyr::summarise(`slope`=lm(`eui_elec_ln`~`mean_avgminmax`)$coefficients[[2]]) %>%
  dplyr::ungroup() %>%
  dplyr::filter(`slope` < -0.005) %>%
  .$Name

length(downslope_buildings)

## plot smoothed results for down slope buildings only
region = "East North Central"
allData %>%
  dplyr::left_join(df_mean_avgminmax, by=c("Name", "year", "month")) %>%
  dplyr::filter(`Name` %in% downslope_buildings) %>%
  dplyr::filter(`US Climate Region`==region) %>%
  dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
  dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=`mean_avgminmax`, y=`eui_elec_ln`, group=`Name`)) +
  ggplot2::geom_point(size=0.25) +
  ggplot2::geom_smooth(span=1.0, se=FALSE, size=0.5) +
  ggplot2::facet_wrap(~`Organization`) +
  ggplot2::xlab("monthly average of daily mean temperature") +
  ggplot2::ylab("Electricity in kBtu / gross sqft") +
  ggplot2::ggtitle(sprintf("kernel smoothing of building electricity (kBtu)/sqft on monthly mean temperature (%s)\nwith negative slope", region)) +
  ggplot2::theme_bw()
ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/%s_smooth_energy_avgtemp_downslope.png", gsub(" ", "", region)), width=6, height=4, units="in")

## find buildings in the sample with a negative slope
up_buildings =
  allData %>%
  dplyr::left_join(df_mean_avgminmax, by=c("Name", "year", "month")) %>%
  dplyr::group_by(`Organization`, `Name`) %>%
  dplyr::summarise(`slope`=lm(`eui_elec_ln`~`mean_avgminmax`)$coefficients[[2]]) %>%
  dplyr::ungroup() %>%
  dplyr::filter(`slope` > 0.005) %>%
  .$Name

length(up_buildings)

region = "East North Central"
allData %>%
  dplyr::left_join(df_mean_avgminmax, by=c("Name", "year", "month")) %>%
  dplyr::filter(`Name` %in% up_buildings) %>%
  dplyr::filter(`US Climate Region`==region) %>%
  dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
  dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=`mean_avgminmax`, y=`eui_elec_ln`, group=`Name`)) +
  ggplot2::geom_point(size=0.25) +
  ggplot2::geom_smooth(span=1.0, se=FALSE, size=0.5) +
  ggplot2::facet_wrap(~`Organization`) +
  ggplot2::xlab("monthly average of daily mean temperature") +
  ggplot2::ylab("Electricity in kBtu / gross sqft") +
  ggplot2::ggtitle(sprintf("kernel smoothing of building electricity (kBtu)/sqft on monthly mean temperature (%s)\nwith negative slope", region)) +
  ggplot2::theme_bw()
ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/%s_smooth_energy_avgtemp_up.png", gsub(" ", "", region)), width=6, height=4, units="in")

overall =
  allData %>%
  dplyr::left_join(df_mean_avgminmax, by=c("Name", "year", "month")) %>%
  dplyr::select(`Name`, `Organization`, `eui_elec_ln`, `<10`, starts_with("["), `>90`) %>%
  {.}

buildings = unique(overall$Name)

coef_labels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90")

coef_labels <- coef_labels[which(coef_labels != "[60-70)")]

## transpose a data frame using the first column as the new header
transpose_df_header_1st_col <- function(df, colname) {
  transposed = setNames(data.frame(t(df[,-1])), df[,1][, colname]) %>%
    {.}
  return(transposed)
}

acc = NULL
for (b in buildings) {
  print(b)
  df <- overall %>%
    dplyr::filter(Name == b) %>%
    ## dplyr::mutate(`eui_elec_ln_scaled` = `eui_elec_ln` * 1e3) %>%
    {.}
  out <- lm(`eui_elec_ln`~`<10` + `[10-20)` + `[20-30)` + `[30-40)` +
              ## out <- lm(`eui_elec_ln_scaled`~`<10` + `[10-20)` + `[20-30)` + `[30-40)` +
            `[40-50)` + `[50-60)` + `[70-80)` + `[80-90)` + `>90`, data=df)
  coefs = as.numeric(out$coefficients)
  coefs = coefs[2:length(coefs)]
  names(coefs) = coef_labels
  dfcoef = data.frame(coefs) %>%
    tibble::rownames_to_column() %>%
    dplyr::bind_rows(data.frame(rowname="[60-70)", coefs=NA)) %>%
    dplyr::mutate(`Name`=b) %>%
    {.}
  acc <- rbind(acc, dfcoef)
}

acc %>%
  readr::write_csv("bin_coef_by_building_scaleUp1e3.csv")

org_lookup = allData %>%
  distinct(`Organization`, `US Climate Region`, `Name`)

total_eui_elec_lookup = allData %>%
  dplyr::group_by(`Name`) %>%
  dplyr::summarise(`total electricity per sqft`=sum(`eui_elec`)) %>%
  dplyr::ungroup()

acc %>%
  dplyr::filter(`rowname`=="<10", coefs < -2) %>%
  ## dplyr::filter(`rowname`=="<10", coefs < -2000) %>%
  head()

## plot coefficient estimate for individual buildings for each region
acc %>%
  dplyr::mutate(`ordering`=match(`rowname`, breaklabels)) %>%
  dplyr::left_join(org_lookup) %>%
  dplyr::left_join(total_eui_elec_lookup) %>%
  dplyr::mutate(`US Climate Region`=factor(`US Climate Region`, levels=c("Central", "East North Central", "Northeast", "Southeast", "South"))) %>%
  dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
  dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
  ## dplyr::filter(`US Climate Region`=="East North Central") %>%
  ## dplyr::filter(`Name` != "MI1879ZZ") %>%
  ggplot2::ggplot(ggplot2::aes(x=ordering, y=coefs, group=Name, color=`total electricity per sqft`)) +
  ggplot2::geom_point(size=0.5) +
  ggplot2::geom_line(size=0.5) +
  ggplot2::ylab("Electricity in kBtu / gross sqft") +
  ggplot2::xlab("Temperature bin") +
  ggplot2::scale_x_continuous(breaks=1:10, labels=breaklabels) +
  ggplot2::facet_wrap(`Organization`~`US Climate Region`, nrow=2) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust=1),
                 legend.position = "bottom")

ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/facet_building_bin_coef.png", width=8, height=6, units="in")

## restrict the plot range to -5000 to 5000
acc %>%
  dplyr::mutate(`ordering`=match(`rowname`, breaklabels)) %>%
  dplyr::left_join(org_lookup) %>%
  dplyr::left_join(total_eui_elec_lookup) %>%
  dplyr::mutate(`US Climate Region`=factor(`US Climate Region`, levels=c("Central", "East North Central", "Northeast", "Southeast", "South"))) %>%
  dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
  dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
  dplyr::filter(coefs < 5, coefs > -5) %>%
  ## dplyr::filter(`total electricity per sqft`>100) %>%
  ggplot2::ggplot(ggplot2::aes(x=ordering, y=coefs, group=Name, color=`total electricity per sqft`)) +
  ggplot2::geom_point(size=0.5) +
  ggplot2::geom_line(size=0.5) +
  ggplot2::ylab("Electricity in kBtu / gross sqft") +
  ggplot2::xlab("Temperature bin") +
  ggplot2::scale_x_continuous(breaks=1:10, labels=breaklabels) +
  ggplot2::facet_wrap(`Organization`~`US Climate Region`, nrow=2) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust=1),
                 legend.position = "bottom")
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/facet_building_bin_coef_restrictYrange.png", width=8, height=6, units="in")

## count of days in 90 bin
num_days_in_bin_90 <- allData %>%
  dplyr::select(`US Climate Region`, Name, `Organization`, `eui_elec_ln`, `<10`, starts_with("["), `>90`) %>%
  tidyr::gather(`temperature bin`, `count`, `<10`:`>90`) %>%
  dplyr::filter(`temperature bin` %in% c(">90")) %>%
  dplyr::filter(count > 0) %>%
  dplyr::group_by(`US Climate Region`, `Organization`, `temperature bin`) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::ungroup() %>%
  {.}
result_table_tex <- xtable(num_days_in_bin_90, caption="Count of days with daily temperature greater than 90")
print(result_table_tex, tabular.environment = "longtable", include.rownames=FALSE,
      file="~/Dropbox/thesis/writeups/policy_cmp/tables/num_days_in_bin_90.tex")

head(num_days_in_bin_90)

## plot energy against weather for each region
## region = "East North Central"
## region = "Central"
## region = "Northeast"
## region = "Southeast"
region=""
to_plot = allData
if (nchar(region) > 0) {
  to_plot = allData %>%
    dplyr::filter(`US Climate Region`==region) %>%
    {.}
}
to_plot %>%
  dplyr::select(Name, `Organization`, `eui_elec_ln`, `<10`, starts_with("["), `>90`) %>%
  tidyr::gather(`temperature bin`, `count`, `<10`:`>90`) %>%
  dplyr::filter(`temperature bin` %in% c("[80-90)", ">90")) %>%
  ggplot2::ggplot(ggplot2::aes(x=`count`, y=`eui_elec_ln`, color=`Organization`)) +
  ggplot2::geom_jitter(size=0.5, width=0.25) +
  ggplot2::facet_wrap(`Organization`~`temperature bin`) +
  ggplot2::ylab("Electricity in kBtu / gross sqft") +
  ggplot2::xlab("number of times per month daily mean temperature fall into this bin") +
  ggplot2::ggtitle("Distribution of electricity use intensity by days in high temperature bins") +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")
ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/%s_jitter_by_bin_cnt.png", gsub(" ", "", region)), width=6, height=4, units="in")

## plot energy distribution for bin 90
allData %>%
  dplyr::select(`Name`, `US Climate Region`, `Organization`, `eui_elec_ln`, `>90`) %>%
  dplyr::mutate(`US Climate Region`=factor(`US Climate Region`, levels=c("Central", "East North Central", "Northeast", "Southeast", "South"))) %>%
  dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
  dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
  ggplot2::ggplot(ggplot2::aes(x=`>90`, y=`eui_elec_ln`, color=`Organization`)) +
  ggplot2::geom_jitter(size=0.5, width=0.25) +
  ggplot2::ylab("Electricity in kBtu / gross sqft") +
  ggplot2::xlab("number of times per month daily mean temperature > 90F") +
  ggplot2::facet_wrap(`Organization`~`US Climate Region`, nrow=2) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/facet_energy_distribution_by_90bin.png",
                width=6, height=4, units="in")

eastNorthCentral %>%
  dplyr::group_by(`Organization`, `Name`) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  dplyr::group_by(`Organization`) %>%
  dplyr::summarise(n())

## plot histogram using full data
acc = feather::read_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/all_weather.feather")

## produce histogram of daily average temperature
lowerbound = min(acc$AVGMINMAX)
upperbound = max(acc$AVGMINMAX)
breaks = c(lowerbound, seq(10, 90, by=10), upperbound)
break_labels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90")

## produce bin count plot
acc %>%
  ggplot2::ggplot(ggplot2::aes(x=`AVGMINMAX`)) +
  ggplot2::geom_histogram(breaks = breaks) +
  ggplot2::stat_bin(breaks = breaks, geom="text", ggplot2::aes(label = ..count..), size=2, vjust=-1) +
  ggtitle("Distribution of daily mean (average of min and max) temperature") +
  xlab("daily mean temperature (average of min and max)") +
  scale_x_continuous(breaks=seq(-30, 100, 10)) +
  theme_bw()
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/avgminmax_bin.png", width=6, height=4, units="in")

## produce bar style count of daily average temp, as in Barreca et al. 2016
acc %>%
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
  ggplot2::ggplot(ggplot2::aes(x=`value_label`)) +
  ggplot2::geom_bar() +
  ggtitle("Distribution of daily mean (average of min and max) temperature,\nof the three years") +
  xlab("daily mean temperature (average of min and max)") +
  ggplot2::theme_bw()
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/avgminmax_bin_barstyle.png", width=6, height=4, units="in")

## summary stats of weather data
devtools::load_all("~/Dropbox/thesis/code/summaryUtil")
summaryUtil::summary_table(acc) %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_weather.csv")

acc_distance = feather::read_feather("~/Dropbox/thesis/code/pubPriCmp/data-raw/station_distance.feather")

## print number of distinct weather stations used in each year
acc_distance %>%
  dplyr::group_by(`start_time`) %>%
  dplyr::summarise(cnt = length(unique(`id`))) %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/distinct_station_cnt_byyear.csv")

## print number of distinct weather stations used
acc_distance %>%
  distinct(`id`) %>%
  nrow()

distance_summary = acc_distance %>%
  dplyr::group_by(`Name`, `varname`, `start_time`) %>%
  dplyr::summarise(`minimum distance` = min(`distance`), `maximum distance`=max(`distance`), `average distance`=mean(`distance`), `number of stations`= n()) %>%
  {.}

distance_summary %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_station_distance.csv")

distance_summary %>%
  dplyr::filter(cnt < 3) %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/fewerThan3Stations.csv")

## distribution of max distance of weather stations available
distance_summary %>%
  dplyr::group_by(`Name`, `varname`) %>%
  dplyr::summarise(`max_distance_overall`=max(`maximum distance`)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x=max_distance_overall)) +
  geom_histogram() +
  ggtitle("Distribution of maximum distance between weather station and building") +
  xlab("maximum distance") +
  facet_wrap(~ varname) +
  theme(plot.title = element_text(size=12))
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/station_max_distance.png", width=6, height=4, units="in")

load("~/Dropbox/thesis/code/pubPriCmp/data/allData.rda")
load("~/Dropbox/thesis/code/pubPriCmp/data/allDataGreen.rda")

## generate appendix summary table for all variables
devtools::load_all("~/Dropbox/thesis/code/summaryUtil")
summaryUtil::summary_table(allData) %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_allData.csv")

## generate appendix summary table for all variables with Greeness rating included
devtools::load_all("~/Dropbox/thesis/code/summaryUtil")
encodedGreen <- allDataGreen %>%
  dplyr::mutate(`eGreeness`=ifelse(`Greeness` == "Least Green", 1, ifelse(`Greeness` == "Moderate Green", 2, 3))) %>%
  {.}

unique(encodedGreen$eGreeness)

summaryUtil::summary_table(encodedGreen) %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_allDataGreen.csv")

names(allDataGreen)

## generate in-paper summary statistics
yearlyDataStatic <-
  allDataGreen %>%
  dplyr::select(-`latitude`, -`longitude`, -`month`, -`City`, -`hasRetrofitBeforeThisMonth`, -`Date`, -`eui_elec_ln`,
                -`eui_gas_ln`) %>%
  dplyr::group_by(`Name`, `year`) %>%
  dplyr::summarise_at(vars(`State`, `GSF`, `Organization`, `region_wiki`, `private`, `Greeness`), funs(first)) %>%
  ## dplyr::summarise_at(vars(`State`, `GSF`, `Organization`, `US Climate Region`, `private`, `Greeness`), funs(first)) %>%
  {.}

names(yearlyDataStatic)

yearlyDataOther <-
  allDataGreen %>%
  dplyr::select(-`latitude`, -`longitude`, -`month`, -`City`, -`hasRetrofitBeforeThisMonth`, -`Date`, -`eui_elec_ln`,
                -`eui_gas_ln`, -`US Climate Region`, -`USRegion`) %>%
                ## -`eui_gas_ln`) %>%
  dplyr::group_by(`Name`, `year`) %>%
  ## dplyr::summarise_at(vars(-one_of("State", "GSF", "Organization", "US Climate Region", "private", "Greeness")),
  dplyr::summarise_at(vars(-one_of("State", "GSF", "Organization", "region_wiki", "private", "Greeness")),
                      funs(sum)) %>%
  {.}

names(yearlyDataOther)

yearlyData <- yearlyDataStatic %>%
  dplyr::left_join(yearlyDataOther, by=c("Name", "year")) %>%
  {.}

yearlyAvgStatic <- yearlyDataStatic %>%
  dplyr::select(-`year`) %>%
  dplyr::group_by(`Name`) %>%
  dplyr::summarise_all(funs(first)) %>%
  {.}

yearlyAvgOther <- yearlyDataOther %>%
  dplyr::select(-`year`) %>%
  dplyr::group_by(`Name`) %>%
  dplyr::summarise_all(funs(mean)) %>%
  {.}

yearlyAvgData <- yearlyAvgStatic %>%
  dplyr::left_join(yearlyAvgOther, by="Name") %>%
  {.}

head(yearlyAvgData)

## ## check whether additional rows are added as a result of the join
## nrow(yearlyAvgStatic)
## nrow(yearlyAvgData)

## bar plot of temperature bins, fill by ownership
break_labels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90")
yearlyAvgData %>%
  dplyr::select(-one_of("Name", "State", "Organization", "GSF", "HDD2", "CDD2", "US Climate Region", "USRegion",
                        "region_wiki", "eui_gas",
                        ## dplyr::select(-one_of("Name", "State", "Organization", "GSF", "HDD2", "CDD2", "US Climate Region", "eui_gas",
                        "eui_elec", "HDD", "CDD", "Electric_(kBtu)", "Gas_(kBtu)")) %>%
  tidyr::gather(`Mean Temperature Bin`, `Average Number of Days`, `<10`:`>90`) %>%
  dplyr::mutate(`Mean Temperature Bin`=factor(`Mean Temperature Bin`, levels=break_labels)) %>%
  dplyr::group_by(`private`, `Mean Temperature Bin`) %>%
  dplyr::summarise(`Average Number of Days` = mean(`Average Number of Days`)) %>%
  dplyr::mutate(`Ownership` = ifelse(`private`, "private", "public")) %>%
  ggplot2::ggplot(ggplot2::aes(x=`Mean Temperature Bin`, y=`Average Number of Days`, fill=`Ownership`)) +
  ggplot2::geom_bar(position = "dodge", stat="identity") +
  ggplot2::scale_fill_grey() +
  ggplot2::ggtitle("Distribution of average annual daily mean temperature") +
  ggplot2::ylab("Number of days per year") +
  ggplot2::theme_bw()
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/dailyAvgDistribution.png", width=6, height=4, units="in")

## bar plot of temperature bins, fill by Greeness, facet by public and private
break_labels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90")
yearlyAvgData %>%
  dplyr::select(-one_of("Name", "State", "Organization", "GSF", "HDD2", "CDD2", "US Climate Region", "USRegion",
                        "region_wiki", "eui_gas",
                        ## dplyr::select(-one_of("Name", "State", "Organization", "GSF", "HDD2", "CDD2", "US Climate Region", "eui_gas",
                        "eui_elec", "HDD", "CDD", "Electric_(kBtu)", "Gas_(kBtu)")) %>%
  tidyr::gather(`Mean Temperature Bin`, `Average Number of Days`, `<10`:`>90`) %>%
  dplyr::mutate(`Mean Temperature Bin`=factor(`Mean Temperature Bin`, levels=break_labels),
                `Greeness`=factor(`Greeness`, levels=c("Most Green", "Moderate Green", "Least Green"))) %>%
  dplyr::group_by(`private`, `Greeness`, `Mean Temperature Bin`) %>%
  dplyr::summarise(`Average Number of Days` = mean(`Average Number of Days`)) %>%
  dplyr::mutate(`Ownership` = ifelse(`private`, "private", "public")) %>%
  ggplot2::ggplot(ggplot2::aes(x=`Mean Temperature Bin`, y=`Average Number of Days`, fill=`Greeness`)) +
  ggplot2::geom_bar(position = "dodge", stat="identity") +
  ggplot2::scale_fill_grey() +
  ggplot2::ggtitle("Distribution of daily average temperature") +
  ggplot2::ylab("Number of days per year") +
  ggplot2::facet_wrap(~`Ownership`, nrow=2) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/dailyAvgDistribution_greeness.png", width=6, height=6, units="in")

## bar plot of temperature bins, fill by region, facet by public and private
break_labels = c("<10", sprintf("[%s-%s)", seq(10, 80, by=10), seq(20, 90, by=10)), ">90")
yearlyAvgData %>%
  dplyr::select(-one_of("Name", "State", "Organization", "GSF", "HDD2", "CDD2",
                        "US Climate Region", "USRegion", "Greeness",
                        "eui_gas",
                        "eui_elec", "HDD", "CDD", "Electric_(kBtu)",
                        "Gas_(kBtu)")) %>%
  tidyr::gather(`Mean Temperature Bin`, `Average Number of Days`, `<10`:`>90`) %>%
  dplyr::mutate(`Mean Temperature Bin`=factor(`Mean Temperature Bin`, levels=break_labels)) %>%
  dplyr::group_by(`private`, `region_wiki`, `Mean Temperature Bin`) %>%
  dplyr::summarise(`Average Number of Days` = mean(`Average Number of Days`)) %>%
  dplyr::mutate(`Ownership` = ifelse(`private`, "private", "public")) %>%
  ggplot2::ggplot(ggplot2::aes(x=`Mean Temperature Bin`, y=`Average Number of Days`, fill=`region_wiki`)) +
  ggplot2::geom_bar(position = "dodge", stat="identity") +
  ggplot2::scale_fill_grey() +
  ggplot2::ggtitle("Distribution of daily average temperature") +
  ggplot2::ylab("Number of days per year") +
  ggplot2::facet_wrap(~`Ownership`, nrow=2) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/dailyAvgDistribution_region_wiki.png", width=6, height=6, units="in")

yearlyAvgData_cnt =
  yearlyAvgData %>%
  dplyr::group_by(`private`, `US Climate Region`) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`private`=as.character(`private`)) %>%
  {.}

## summary by portfolio and climate region
yearlyAvgData %>%
  dplyr::select(-`Greeness`) %>%
  dplyr::mutate(`<40` = `<10` + `[10-20)` + `[20-30)` + `[30-40)`) %>%
  dplyr::select(-one_of("Name", "State", "Organization", "<10", "[10-20)", "[20-30)", "[30-40)", "[40-50)",
                        "[50-60)", "[60-70)", "[70-80)")) %>%
  dplyr::group_by(`private`, `US Climate Region`) %>%
  dplyr::summarise_all(funs(mean)) %>%
  dplyr::ungroup() %>%
  dplyr::select(`private`, `US Climate Region`, `Electric_(kBtu)`, `Gas_(kBtu)`, `eui_elec`, `eui_gas`, `HDD`,
                `CDD`, `<40`, `[80-90)`, `>90`) %>%
  dplyr::mutate(`private`=as.character(`private`)) %>%
  dplyr::mutate_if(is.numeric, function(x) {sprintf("%.1f", x)}) %>%
  dplyr::left_join(yearlyAvgData_cnt, by=c("private", "US Climate Region")) %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_annual_avg.csv")

## summary overall
yearlyAvgData %>%
  dplyr::mutate(`<40` = `<10` + `[10-20)` + `[20-30)` + `[30-40)`) %>%
  dplyr::select(-one_of("Name", "State", "Organization", "<10", "[10-20)", "[20-30)", "[30-40)", "[40-50)",
                        "[50-60)", "[60-70)", "[70-80)", "GSF", "US Climate Region", "private", "Greeness")) %>%
  dplyr::summarise_all(mean) %>%
  dplyr::select(`Electric_(kBtu)`, `Gas_(kBtu)`, `eui_elec`, `eui_gas`, `HDD`,
                `CDD`, `<40`, `[80-90)`, `>90`) %>%
  dplyr::mutate_if(is.numeric, function(x) {sprintf("%.1f", x)}) %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_annual_avg_nogroup.csv")

## summary public and private
yearlyAvgData %>%
  dplyr::mutate(`<40` = `<10` + `[10-20)` + `[20-30)` + `[30-40)`) %>%
  dplyr::select(-one_of("Name", "State", "Organization", "<10", "[10-20)", "[20-30)", "[30-40)", "[40-50)",
                        "[50-60)", "[60-70)", "[70-80)", "GSF", "US Climate Region", "Greeness")) %>%
  dplyr::group_by(`private`) %>%
  dplyr::summarise_all(funs(mean)) %>%
  dplyr::ungroup() %>%
  dplyr::select(`private`, `Electric_(kBtu)`, `Gas_(kBtu)`, `eui_elec`, `eui_gas`, `HDD`,
                `CDD`, `<40`, `[80-90)`, `>90`) %>%
  dplyr::mutate(`private`=as.character(`private`)) %>%
  dplyr::mutate_if(is.numeric, function(x) {sprintf("%.1f", x)}) %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_annual_avg_pubpri.csv")

## summary by portfolio and greeness level
yearlyAvgData_cnt =
  yearlyAvgData %>%
  dplyr::group_by(`private`, `Greeness`) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::ungroup()

yearlyAvgData %>%
  dplyr::mutate(`<40` = `<10` + `[10-20)` + `[20-30)` + `[30-40)`) %>%
  dplyr::select(-`US Climate Region`) %>%
  dplyr::select(-one_of("Name", "State", "Organization", "<10", "[10-20)", "[20-30)", "[30-40)", "[40-50)",
                        "[50-60)", "[60-70)", "[70-80)")) %>%
  dplyr::group_by(`private`, `Greeness`) %>%
  dplyr::summarise_all(funs(mean)) %>%
  dplyr::ungroup() %>%
  dplyr::select(`private`, `Greeness`, `Electric_(kBtu)`, `Gas_(kBtu)`, `eui_elec`, `eui_gas`, `HDD`,
                `CDD`, `<40`, `[80-90)`, `>90`) %>%
  dplyr::mutate_if(is.numeric, function(x) {sprintf("%.1f", x)}) %>%
  dplyr::mutate(`private`=as.numeric(`private`)) %>%
  dplyr::left_join(yearlyAvgData_cnt, by=c("private", "Greeness")) %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_annual_avg_byGreeness.csv")

## produce plots of coefficients for temperature bin, with confidence interval
breaklevels = c("avgTempBelow10", "avgTemp10to20", "avgTemp20to30", "avgTemp30to40",
                "avgTemp40to50", "avgTemp50to60", "avgTemp60to70", "avgTemp70to80", "avgTemp80to90",
    "avgTempAbove90")
breaklabels = c("<10", "[10-20)", "[20-30)", "[30-40)",
                "[40-50)", "[50-60)", "[60-70)", "[70-80)", "[80-90)",
                ">90")

load("../data/allData.rda")

## portfolio = ""
## ## "_gas" is gas, "" is electricity
## energy_type = "_gas"
## status_code = ""
## df_zero = data.frame(X1="avgTemp60to70", variable=rep(c("coef", "ci_low", "ci_high"), 6),
##                      model=rep(c(rep("X2", 3), rep("X3", 3), rep("X4", 3)), 2),
##                      value = 0, status=c(rep("public", 9), rep("private relative to public", 9)))
## portfolio = "_GSA"
## ## energy_type = "_total"
## ## energy_type = "_gas"
## energy_type = ""
## status_code = "public"
## df_zero = data.frame(X1="avgTemp60to70", variable=rep(c("coef", "ci_low", "ci_high"), 2),
##                      model=rep(c(rep("X2", 3), rep("X3", 3)), 2),
##                      value = 0, status=c(rep("public", 6)))
portfolio = "_PNC"
## energy_type = "_total"
## energy_type = "_gas"
energy_type = ""
status_code = "private"
df_zero = data.frame(X1="avgTemp60to70", variable=rep(c("coef", "ci_low", "ci_high"), 2),
                     model=rep(c(rep("X2", 3), rep("X3", 3)), 2),
                     value = 0, status=c(rep("private", 6)))

## produce the bin coefficient and CI plot for the model with both portfolio
df =
  readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_results_bin_0720%s%s.txt", tolower(portfolio), energy_type), skip=7, col_names=FALSE) %>%
## remove bottom non-conformative rows
  head(-5) %>%
  dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
  tidyr::fill(`X1`) %>%
  tidyr::unite(`temp`, `X1`, `variable`, sep="-") %>%
  tidyr::gather(`model`, `value`, starts_with("X")) %>%
  tidyr::separate(`temp`, c("X1", "variable"), sep="-") %>%
  dplyr::filter(`variable` != "se",
                !grepl("0b.", `X1`, fixed = TRUE),
                !grepl("eDate", `X1`, fixed = TRUE),
                !grepl("eState", `X1`, fixed = TRUE),
                !grepl("hasretrofit", `X1`, fixed = TRUE),
                !grepl("Constant", `X1`, fixed = TRUE)
                ) %>%
  {.}
if (status_code == "") {
  df <- df %>%
    dplyr::mutate(`status`=ifelse(grepl("1.private", X1, fixed=TRUE), "private relative to public", "public")) %>%
    dplyr::mutate(`status`=factor(`status`, levels=c("public", "private relative to public"))) %>%
    {.}
} else {
  df <- df %>%
    dplyr::mutate(`status`=status_code) %>%
    {.}
}
df <- df %>%
  dplyr::mutate(`X1`=gsub("1.private#c.", "", `X1`)) %>%
  dplyr::mutate(`value`=gsub("\\(", "", `value`)) %>%
  dplyr::mutate(`value`=gsub("\\)", "", `value`)) %>%
  dplyr::mutate(`value`=gsub("\\*", "", `value`)) %>%
  dplyr::mutate(`value`=as.numeric(`value`)) %>%
  ## readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/ci_bin.csv")
  dplyr::bind_rows(df_zero) %>%
  dplyr::mutate(`X1`=factor(X1, levels=breaklevels)) %>%
  dplyr::mutate(`ordering`=match(`X1`, breaklevels)) %>%
  dplyr::mutate_at(vars(model), recode, "X2"="base line", "X3"="state specific trend", "X4"="state trend x private") %>%
  {.}
if (nchar(portfolio) == 1) {
  df <- df %>%
    dplyr::filter(`model`=="state trend x private") %>%
    {.}
} else {
  df <- df %>%
    dplyr::filter(`model`=="state specific trend") %>%
    {.}
}

df <- df %>%
  dplyr::filter(!(`X1` %in% c("avgTempBelow10", "avgTempAbove90"))) %>%
  {.}

head(df)
min(df$value)
max(df$value)

if (energy_type == "") {
  shifty = -10
  yupperlimit = 20
  ## shifty = -25
  ## yupperlimit = 25
  scaling = 1 / 1e5 * 1.5
  ylabel = "Electricity"
} else if (energy_type == "_gas") {
  ## shifty = -50
  ## yupperlimit = 75
  shifty = -20
  yupperlimit = 40
  scaling = 1 / 1e5 * 5
  ylabel = "Gas"
} else if (energy_type == "_total") {
  shifty = -15
  yupperlimit = 35
  scaling = 1 / 1e5 * 2
  ylabel = "Electricity + Gas"
}
temperatureBinData =
  allData %>%
  {.}
temperatureBinData <- temperatureBinData %>%
  dplyr::select(`Organization`, `<10`, starts_with("["), `>90`) %>%
  tidyr::gather(`tempBin`, `value`, `<10`:`>90`) %>%
  dplyr::group_by(`Organization`, `tempBin`) %>%
  dplyr::summarise(count = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`count`=`count` * scaling) %>%
  dplyr::mutate(`count_start`=shifty, `count_end`=`count` + shifty) %>%
  dplyr::mutate(`ordering`=match(`tempBin`, breaklabels)) %>%
  {.}
if (status_code == "") {
  temperatureBinData <- temperatureBinData %>%
    dplyr::mutate(`status`=ifelse(`Organization`=="GSA", "public", "private relative to public")) %>%
    dplyr::mutate(`status`=factor(`status`, levels=c("public", "private relative to public"))) %>%
    dplyr:::select(`count_start`, `count_end`, `ordering`, `status`) %>%
    {.}
} else {
  temperatureBinData <- temperatureBinData %>%
    dplyr::mutate(`status`=ifelse(`Organization`=="GSA", "public", "private")) %>%
    dplyr:::select(`count_start`, `count_end`, `ordering`, `status`) %>%
    {.}
}
head(temperatureBinData)

## dplyr::mutate_at(vars(variable), recode, "ci_low"="95% C.I. low end", "ci_high"="95% C.I. high end", "coef"="coefficient") %>%
df %>%
  dplyr::left_join(temperatureBinData, by=c("status", "ordering")) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x=ordering, y=value)) +
  ggplot2::geom_line(ggplot2::aes(x=ordering, y=value, linetype=variable)) +
  scale_linetype_manual(values=c("dashed", "dashed", "solid"), breaks=c("ci_low", NA, "coef"), labels=c("95% C.I.", "", "Coefficient")) +
  scale_x_continuous(breaks=1:10, labels=breaklabels) +
  ggplot2::geom_segment(ggplot2::aes(x=`ordering`, xend=`ordering`, y=`count_start`, yend=`count_end`), size=10) +
  ## ggplot2::geom_bar(mapping=ggplot2::aes(x=`ordering`, y=`count`), stat="identity", data=temperatureBinData) +
  ggplot2::facet_wrap(`status`~`model`, ncol=1) +
  ggplot2::xlab("Temperature Bin") +
  ggplot2::ylab(sprintf("ln(%s in kBtu / gross sqft) * 1000", ylabel)) +
  ggplot2::coord_cartesian(ylim=c(shifty, yupperlimit)) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
                legend.title=element_blank(), legend.position = "bottom")
ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci%s%s.png", portfolio, energy_type),
                width=5, height=5, units="in")

breaklevels = c("hdd", "hdd2", "cdd", "cdd2")
breaklabels = c("HDD", "HDD2", "CDD", "CDD2")
xlabel = "Monthly HDD CDD"

## produce the hdd cdd model coefficient and CI plot for the model with both portfolio
readr::read_tsv("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_results_reg_0720.txt", skip=7, col_names=FALSE) %>%
## remove bottom non-conformative rows
head(-5) %>%
dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
tidyr::fill(`X1`) %>%
tidyr::unite(`temp`, `X1`, `variable`, sep="-") %>%
## modify this to X2:X?, ? is the number of models + 1
tidyr::gather(`model`, `value`, X2:X7) %>%
tidyr::separate(`temp`, c("X1", "variable"), sep="-") %>%
dplyr::filter(`variable` != "se",
              !grepl("0b.", `X1`, fixed = TRUE),
              !grepl("eDate", `X1`, fixed = TRUE),
              !grepl("eState", `X1`, fixed = TRUE),
              !grepl("hasretrofit", `X1`, fixed = TRUE),
              !grepl("Constant", `X1`, fixed = TRUE)
              ) %>%
dplyr::mutate(`status`=ifelse(grepl("1.private", X1, fixed=TRUE), "private relative to public", "public")) %>%
dplyr::mutate(`X1`=gsub("1.private#c.", "", `X1`)) %>%
dplyr::mutate(`value`=gsub("\\(", "", `value`)) %>%
dplyr::mutate(`value`=gsub("\\)", "", `value`)) %>%
dplyr::mutate(`value`=gsub("\\*", "", `value`)) %>%
dplyr::mutate(`value`=as.numeric(`value`)) %>%
na.omit() %>%
## readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/ci_reg.csv")
dplyr::mutate(`X1`=factor(X1, levels=breaklevels)) %>%
dplyr::mutate(`ordering`=match(`X1`, breaklevels)) %>%
dplyr::mutate_at(vars(model), recode, "X2"="baseline", "X3"="baseline (poly 2)", "X4" = "state specific trend",
                 "X5" = "state specific trend (poly 2)", "X6"="state trend x private",
                 "X7"="state trend x private (poly 2)") %>%
## dplyr::mutate_at(vars(variable), recode, "ci_low"="95% C.I. low end", "ci_high"="95% C.I. high end", "coef"="coefficient") %>%
dplyr::filter(`model` %in% c("state trend x private", "state trend x private (poly 2)")) %>%
tidyr::unite(`temp`, `X1`, `model`, sep="-") %>%
tidyr::spread(variable, value) %>%
tidyr::separate(`temp`, c("X1", "model"), sep="-") %>%
ggplot2::ggplot() +
ggplot2::geom_pointrange(ggplot2::aes(x=ordering, ymin=`ci_low`, ymax=`ci_high`, y=`coef`),
                         shape=18) +
## ggplot2::geom_errorbar(ggplot2::aes(x=ordering, ymin=`ci_low`, ymax=`ci_high`)) +
scale_x_continuous(breaks=1:length(breaklabels), labels=breaklabels) +
ggplot2::facet_wrap(`status`~`model`, nrow=2) +
ggplot2::xlab(xlabel) +
ggplot2::ylab(NULL) +
ggplot2::coord_cartesian(ylim=c(-300, 750)) +
ggplot2::theme_bw() +
ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
               legend.title=element_blank())
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/reg_coef_ci.png", width=5, height=5, units="in")

## portfolio = "GSA"
## status_code = "public"
portfolio = "PNC"
status_code = "private"
## produce the hdd cdd model coefficient and CI plot for the model for just one portfolio
readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_results_reg_0720_%s.txt", portfolio), skip=7, col_names=FALSE) %>%
## remove bottom non-conformative rows
head(-5) %>%
dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
tidyr::fill(`X1`) %>%
tidyr::unite(`temp`, `X1`, `variable`, sep="-") %>%
## modify this to X2:X?, ? is the number of models + 1
tidyr::gather(`model`, `value`, X2:X5) %>%
tidyr::separate(`temp`, c("X1", "variable"), sep="-") %>%
dplyr::filter(`variable` != "se",
              !grepl("0b.", `X1`, fixed = TRUE),
              !grepl("eDate", `X1`, fixed = TRUE),
              !grepl("eState", `X1`, fixed = TRUE),
              !grepl("hasretrofit", `X1`, fixed = TRUE),
              !grepl("Constant", `X1`, fixed = TRUE)
              ) %>%
dplyr::mutate(`status`=status_code) %>%
dplyr::mutate(`value`=gsub("\\(", "", `value`)) %>%
dplyr::mutate(`value`=gsub("\\)", "", `value`)) %>%
dplyr::mutate(`value`=gsub("\\*", "", `value`)) %>%
dplyr::mutate(`value`=as.numeric(`value`)) %>%
na.omit() %>%
## readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/ci_reg_%s.csv", portfolio))
dplyr::mutate(`X1`=factor(X1, levels=breaklevels)) %>%
dplyr::mutate(`ordering`=match(`X1`, breaklevels)) %>%
dplyr::mutate_at(vars(model), recode, "X2"="baseline", "X3"="baseline (poly 2)", "X4" = "state specific trend",
                 "X5" = "state specific trend (poly 2)") %>%
dplyr::filter(`model` %in% c("state specific trend", "state specific trend (poly 2)")) %>%
tidyr::unite(`temp`, `X1`, `model`, sep="-") %>%
tidyr::spread(variable, value) %>%
tidyr::separate(`temp`, c("X1", "model"), sep="-") %>%
ggplot2::ggplot() +
ggplot2::geom_pointrange(ggplot2::aes(x=ordering, ymin=`ci_low`, ymax=`ci_high`, y=`coef`), shape=18) +
## ggplot2::geom_errorbar(ggplot2::aes(x=ordering, ymin=`ci_low`, ymax=`ci_high`)) +
## ggplot2::geom_point(ggplot2::aes(x=ordering, y=coef)) +
scale_x_continuous(breaks=1:length(breaklabels), labels=breaklabels) +
ggplot2::facet_wrap(`status`~`model`, nrow=1) +
ggplot2::xlab(xlabel) +
ggplot2::ylab(NULL) +
ggplot2::coord_cartesian(ylim=c(-300, 750)) +
ggplot2::theme_bw() +
ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
               legend.title=element_blank())
ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/reg_coef_ci_%s.png", portfolio), width=4, height=3, units="in")

breaklevels = c("avgTempBelow10", "avgTemp10to20", "avgTemp20to30", "avgTemp30to40",
                "avgTemp40to50", "avgTemp50to60", "avgTemp60to70", "avgTemp70to80", "avgTemp80to90",
                "avgTempAbove90")
breaklabels = c("<10", "[10-20)", "[20-30)", "[30-40)",
                "[40-50)", "[50-60)", "[60-70)", "[70-80)", "[80-90)",
                ">90")

## produce coefficient plot of greeness for separate portfolio using error bars
portfolio = "GSA"
status_code = "public"
## portfolio = "PNC"
## status_code = "private"
xlabel = "Temperature Bin"
## produce the bin model coefficient and CI plot for greeness analysis for the model for just one portfolio
df <- readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_results_bin_greeness_0720_%s.txt", portfolio), skip=7, col_names=FALSE) %>%
  ## remove bottom non-conformative rows
  head(-5) %>%
  dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
  tidyr::fill(`X1`) %>%
  tidyr::unite(`temp`, `X1`, `variable`, sep="-") %>%
  ## modify this to X2:X?, ? is the number of models + 1
  tidyr::gather(`model`, `value`, X2:X3) %>%
  tidyr::separate(`temp`, c("X1", "variable"), sep="-") %>%
  dplyr::filter(`variable` != "se",
                !grepl("0b.", `X1`, fixed = TRUE),
                !grepl("1b.eGreeness", `X1`, fixed = TRUE),
                !grepl("eDate", `X1`, fixed = TRUE),
                !grepl("eState", `X1`, fixed = TRUE),
                !grepl("hasretrofit", `X1`, fixed = TRUE),
                !grepl("Constant", `X1`, fixed = TRUE)
                ) %>%
  dplyr::mutate(`status`=status_code) %>%
  dplyr::mutate(`Rating`=ifelse(grepl("2.eGreeness", `X1`, fixed=TRUE), "Moderate Green \nrelative to Least Green",
                        ifelse(grepl("3.eGreeness", `X1`, fixed=TRUE), "Most Green \nrelative to Least Green", "Least Green"))) %>%
  dplyr::mutate(`X1`=gsub("2.eGreeness#c.", "", `X1`)) %>%
  dplyr::mutate(`X1`=gsub("3.eGreeness#c.", "", `X1`)) %>%
  dplyr::mutate(`value`=gsub("\\(", "", `value`)) %>%
  dplyr::mutate(`value`=gsub("\\)", "", `value`)) %>%
  dplyr::mutate(`value`=gsub("\\*", "", `value`)) %>%
  dplyr::mutate(`value`=as.numeric(`value`)) %>%
  na.omit() %>%
  dplyr::mutate(`X1`=factor(X1, levels=breaklevels)) %>%
  dplyr::mutate(`ordering`=match(`X1`, breaklevels)) %>%
  dplyr::mutate_at(vars(model), recode, "X2"="baseline", "X3" = "state specific trend") %>%
  ## readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/ci_bin_greeness_%s.csv", portfolio))
  tidyr::unite(`temp`, `X1`, `model`, `Rating`, sep="-") %>%
  tidyr::spread(variable, value) %>%
  tidyr::separate(`temp`, c("X1", "model", "Rating"), sep="-") %>%
{.}
df %>%
ggplot2::ggplot() +
ggplot2::geom_errorbar(ggplot2::aes(x=ordering, ymin=`ci_low`, ymax=`ci_high`)) +
ggplot2::geom_point(ggplot2::aes(x=ordering, y=coef)) +
scale_x_continuous(breaks=1:length(breaklabels), labels=breaklabels) +
ggplot2::facet_wrap(`Rating`~`model`, ncol=2) +
ggplot2::xlab(xlabel) +
ggplot2::ylab(NULL) +
## ggplot2::coord_cartesian(ylim=c(-300, 750)) +
ggplot2::theme_bw() +
ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
               legend.title=element_blank())
ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_greeness_%s.png", portfolio), width=8, height=5, units="in")

## produce coefficient plot of greeness for separate portfolio using connected dashed lines for conf int
df_zero = data.frame(X1="avgTemp60to70", variable=rep(c("coef", "ci_low", "ci_high"), 6),
                     model=rep(c(rep("X2", 9), rep("X3", 9)), 2),
                     Rating=rep(c(rep("Least Green", 3), rep("Moderate Green \nrelative to Least Green", 3),
                                  rep("Most Green \nrelative to Least Green", 3)), 2),
                     value = 0, status=status_code)
## portfolio = "GSA"
## status_code = "public"
## energy_type = "_gas"
portfolio = "PNC"
status_code = "private"
energy_type = "_gas"
xlabel = "Temperature Bin"
## produce the bin model coefficient and CI plot for greeness analysis for the model for just one portfolio
df <- readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_results_bin_greeness_0720_%s%s.txt", portfolio, energy_type), skip=7, col_names=FALSE) %>%
  ## remove bottom non-conformative rows
  head(-5) %>%
  dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
  tidyr::fill(`X1`) %>%
  tidyr::unite(`temp`, `X1`, `variable`, sep="-") %>%
  ## modify this to X2:X?, ? is the number of models + 1
  tidyr::gather(`model`, `value`, X2:X3) %>%
  tidyr::separate(`temp`, c("X1", "variable"), sep="-") %>%
  dplyr::filter(`variable` != "se",
                !grepl("0b.", `X1`, fixed = TRUE),
                !grepl("1b.eGreeness", `X1`, fixed = TRUE),
                !grepl("eDate", `X1`, fixed = TRUE),
                !grepl("eState", `X1`, fixed = TRUE),
                !grepl("hasretrofit", `X1`, fixed = TRUE),
                !grepl("Constant", `X1`, fixed = TRUE)
                ) %>%
  dplyr::mutate(`status`=status_code) %>%
  dplyr::mutate(`Rating`=ifelse(grepl("2.eGreeness", `X1`, fixed=TRUE), "Moderate Green \nrelative to Least Green",
                        ifelse(grepl("3.eGreeness", `X1`, fixed=TRUE), "Most Green \nrelative to Least Green", "Least Green"))) %>%
  dplyr::mutate(`X1`=gsub("2.eGreeness#c.", "", `X1`)) %>%
  dplyr::mutate(`X1`=gsub("3.eGreeness#c.", "", `X1`)) %>%
  dplyr::mutate(`value`=gsub("\\(", "", `value`)) %>%
  dplyr::mutate(`value`=gsub("\\)", "", `value`)) %>%
  dplyr::mutate(`value`=gsub("\\*", "", `value`)) %>%
  dplyr::mutate(`value`=as.numeric(`value`)) %>%
  na.omit() %>%
  dplyr::bind_rows(df_zero) %>%
  dplyr::mutate(`X1`=factor(X1, levels=breaklevels)) %>%
  dplyr::mutate(`ordering`=match(`X1`, breaklevels)) %>%
  dplyr::mutate_at(vars(model), recode, "X2"="baseline", "X3" = "state specific trend") %>%
  ## readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/ci_bin_greeness_%s.csv", portfolio))
{.}

min(df$value)
max(df$value)

if (energy_type == "") {
  shifty = -45
  yupperlimit = 25
  scaling = 1 / 1e4 * 1.5
} else if (energy_type == "_gas") {
  if (status_code == "public") {
    shifty = -260
  } else {
    shifty = -100
  }
  yupperlimit = 75
  scaling = 1 / 1e4 * 5
}
temperatureBinData =
  allDataGreen %>%
  {.}
temperatureBinData <-
  temperatureBinData %>%
  dplyr::select(`Organization`, `Greeness`, `<10`, starts_with("["), `>90`) %>%
  tidyr::gather(`tempBin`, `value`, `<10`:`>90`) %>%
  dplyr::group_by(`Organization`, `Greeness`, `tempBin`) %>%
  dplyr::summarise(count = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`count`=`count` * scaling) %>%
  dplyr::mutate(`count_start`=shifty, `count_end`=`count` + shifty) %>%
  dplyr::mutate(`ordering`=match(`tempBin`, breaklabels)) %>%
  {.}
if (status_code == "") {
  temperatureBinData <- temperatureBinData %>%
    dplyr::mutate(`status`=ifelse(`Organization`=="GSA", "public", "private relative to public")) %>%
    dplyr::mutate(`status`=factor(`status`, levels=c("public", "private relative to public"))) %>%
    dplyr:::select(`count_start`, `count_end`, `ordering`, `status`, `Greeness`) %>%
    {.}
} else {
  temperatureBinData <- temperatureBinData %>%
    dplyr::mutate(`status`=ifelse(`Organization`=="GSA", "public", "private")) %>%
    dplyr:::select(`count_start`, `count_end`, `ordering`, `status`, `Greeness`) %>%
    {.}
}
temperatureBinData <- temperatureBinData %>%
  dplyr::mutate(`Rating`=recode(Greeness, "Moderate Green"="Moderate Green \nrelative to Least Green",
                                "Most Green"="Most Green \nrelative to Least Green")) %>%
  dplyr::select(-`Greeness`) %>%
  {.}
df %>%
  dplyr::left_join(temperatureBinData, by=c("ordering", "status", "Rating")) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x=ordering, y=value)) +
  ggplot2::geom_line(ggplot2::aes(x=ordering, y=value, linetype=variable)) +
  scale_linetype_manual(values=c("dashed", "dashed", "solid"), breaks=c("ci_low", NA, "coef"), labels=c("95% C.I.", "", "Coefficient")) +
  scale_x_continuous(breaks=1:length(breaklabels), labels=breaklabels) +
  ggplot2::geom_segment(ggplot2::aes(x=`ordering`, xend=`ordering`, y=`count_start`, yend=`count_end`), size=7) +
  ## for slide
  ggplot2::facet_wrap(`model`~`Rating`, ncol=3) +
  ## for paper
  ## ggplot2::facet_wrap(`Rating`~`model`, ncol=2) +
  ggplot2::xlab(xlabel) +
  ggplot2::ylab("ln(Electricity in kBtu / gross sqft) * 1000") +
  ## ggplot2::coord_cartesian(ylim=c(-300, 750)) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
                legend.title=element_blank(), legend.position = "bottom")
ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_greeness_%s_slide%s.png", portfolio, energy_type), width=8, height=5, units="in")
## ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_greeness_%s.png", portfolio), width=6, height=7, units="in")

## produce coefficient plot of greeness for separate portfolio by greeness level
## using connected dashed lines for conf int
read_outreg2 <- function(filename, fields_to_remove, fields_to_keep, outreg_vars,
                         vars_to_keep) {
  df =
    readr::read_tsv(filename, skip=7, col_names=FALSE) %>%
    head(-5) %>%
    dplyr::mutate(`variable`=rep(outreg_vars, nrow(.)/length(outreg_vars))) %>%
    tidyr::fill(`X1`) %>%
    dplyr::filter(`variable` %in% vars_to_keep) %>%
    {.}
  for (field in fields_to_remove) {
    df <- df %>%
      dplyr::filter(!grepl(field, `X1`, fixed = TRUE)) %>%
      {.}
  }
  for (field in fields_to_keep) {
    df <- df %>%
      dplyr::filter(grepl(field, `X1`, fixed = TRUE)) %>%
      {.}
  }
  return(df)
}

breaklevels = c("avgTempBelow10", "avgTemp10to20", "avgTemp20to30", "avgTemp30to40",
                "avgTemp40to50", "avgTemp50to60", "avgTemp60to70", "avgTemp70to80", "avgTemp80to90",
                "avgTempAbove90")
breaklabels = c("<10", "[10-20)", "[20-30)", "[30-40)",
                "[40-50)", "[50-60)", "[60-70)", "[70-80)", "[80-90)",
                ">90")
## green_levels = c("Least Green", "Moderate Green", "Most Green")
green_levels = c("Least Green", "Most Green")
len_green_levels = length(green_levels)

## green_levels = c("Least Green", "Most Green")
## energy_type = "_gas"
modelsuffix = "baseline"
modelname = "baseline"
## energy_type = "_gas"
energy_type = "_total"
## modelsuffix = "statetrend"
## modelname = "state specific trend"
filename = sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_greeness_%s%s.txt", modelsuffix,
                   energy_type)
fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "0b.", "1b.eGreeness")
fields_to_keep = NULL
outreg_vars = c("coef", "se", "ci_low", "ci_high")
vars_to_keep = c("coef", "ci_low", "ci_high")
df <- read_outreg2(filename=filename, fields_to_remove= fields_to_remove, fields_to_keep= fields_to_keep,
                   outreg_vars=outreg_vars, vars_to_keep=vars_to_keep)
head(df)

model_info = data.frame(`model_header`=sprintf("X%s", 2:(2*len_green_levels + 1)),
                        status=c(rep("public", len_green_levels), rep("private", len_green_levels)),
                        Rating=rep(green_levels, 2))

df_zero = data.frame(X1="avgTemp60to70", variable=rep(c("coef", "ci_low", "ci_high"), 2 * len_green_levels),
                     model=modelname,
                     Rating=rep(sort(rep(green_levels, 3)), 2),
                     value = 0, status=c(rep("public", 3 * len_green_levels), rep("private", 3 * len_green_levels)))
nrow(df_zero)

df <-
  df %>%
  tidyr::gather(`model_header`, `value`, X2:(!!rlang::sym(sprintf("X%s", 2 * len_green_levels + 1)))) %>%
  dplyr::mutate(`value`=gsub("\\(", "", `value`)) %>%
  dplyr::mutate(`value`=gsub("\\)", "", `value`)) %>%
  dplyr::mutate(`value`=gsub("\\*", "", `value`)) %>%
  dplyr::mutate(`value`=as.numeric(`value`)) %>%
  na.omit() %>%
  dplyr::left_join(model_info, by="model_header") %>%
  dplyr::bind_rows(df_zero) %>%
  dplyr::mutate(`ordering`=match(`X1`, breaklevels)) %>%
  dplyr::mutate(`model`=modelname) %>%
  {.}

df <- df %>%
  dplyr::filter(!(`X1` %in% c("avgTempBelow10", "avgTempAbove90"))) %>%
  {.}

min(df$value)
max(df$value)

if (energy_type == "") {
  shifty = -45
  yupperlimit = 25
  scaling = 1 / 1e4 * 1.5
  ylabel = "Electricity"
} else if (energy_type == "_gas") {
  shifty = -45
  yupperlimit = 240
  scaling = 1 / 1e4 * 1.5
  ylabel = "Gas"
} else if (energy_type == "_total") {
  shifty = -5
  yupperlimit = 40
  scaling = 1 / 1e5 * 1.5
  ylabel = "Electricity + Gas"
}

temperatureBinData =
  allDataGreen %>%
  {.}
temperatureBinData <-
  temperatureBinData %>%
  dplyr::select(`Organization`, `Greeness`, `<10`, starts_with("["), `>90`) %>%
  tidyr::gather(`tempBin`, `value`, `<10`:`>90`) %>%
  dplyr::group_by(`Organization`, `Greeness`, `tempBin`) %>%
  dplyr::summarise(count = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`count`=`count` * scaling) %>%
  dplyr::mutate(`count_start`=shifty, `count_end`=`count` + shifty) %>%
  dplyr::mutate(`ordering`=match(`tempBin`, breaklabels)) %>%
  dplyr::mutate(`status`=ifelse(`Organization`=="GSA", "public", "private")) %>%
  dplyr::select(`count_start`, `count_end`, `ordering`, `status`, `Greeness`) %>%
  dplyr::rename(`Rating`=`Greeness`) %>%
  {.}
## head(temperatureBinData)
## head(df)

xlabel = "Temperature Bin"
## facet on public private and greeness level
df %>%
  dplyr::left_join(temperatureBinData, by=c("ordering", "status", "Rating")) %>%
  dplyr::mutate(`status`=factor(`status`, levels=c("public", "private"))) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x=ordering, y=value)) +
  ggplot2::geom_line(ggplot2::aes(x=ordering, y=value, linetype=variable)) +
  scale_linetype_manual(values=c("dashed", "dashed", "solid"), breaks=c("ci_low", NA, "coef"), labels=c("95% C.I.", "", "Coefficient")) +
  scale_x_continuous(breaks=1:length(breaklabels), labels=breaklabels) +
  ggplot2::geom_segment(ggplot2::aes(x=`ordering`, xend=`ordering`, y=`count_start`, yend=`count_end`), size=7) +
  ## for slide
  ggplot2::facet_wrap(`status`~`Rating`, ncol=len_green_levels) +
  ## for paper
  ## ggplot2::facet_wrap(`Rating`~`status`, ncol=2) +
  ggplot2::xlab(xlabel) +
  ggplot2::ylab(sprintf("ln(%s in kBtu / gross sqft) * 1000", ylabel)) +
  ## ggplot2::coord_cartesian(ylim=c(-300, 750)) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
                legend.title=element_blank(), legend.position = "bottom")
ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_greeness_separate_organization_greeness_%s_slide%s.png", modelsuffix, energy_type), width=8, height=5, units="in")
## ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_greeness_separate_organization_greeness_%s.png", modelsuffix), width=6, height=7, units="in")


## facet on public private and fill on greeness level
to_plot <- df %>%
  dplyr::left_join(temperatureBinData, by=c("ordering", "status", "Rating")) %>%
  dplyr::mutate(`status`=factor(`status`, levels=c("public", "private"))) %>%
  {.}
to_plot_least <- to_plot %>%
  dplyr::filter(`Rating`=="Least Green") %>%
  {.}
to_plot_most <- to_plot %>%
  dplyr::filter(`Rating`=="Most Green") %>%
  {.}

binsize = 4
jitterAmount = 0.15
xlabel = "Temperature Bin"
## facet on public private and greeness level
to_plot %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x=ordering, y=value, color=Rating)) +
  ggplot2::geom_line(ggplot2::aes(x=ordering, y=value, linetype=variable, color=Rating)) +
  scale_linetype_manual(values=c("dashed", "dashed", "solid"), breaks=c("ci_low", NA, "coef"), labels=c("95% C.I.", "", "Coefficient")) +
  scale_x_continuous(breaks=1:length(breaklabels), labels=breaklabels) +
  ## ggplot2::geom_segment(ggplot2::aes(x=`ordering`, xend=`ordering`, y=`count_start`, yend=`count_end`), size=7) +
  ggplot2::geom_segment(ggplot2::aes(x=`ordering`-jitterAmount, xend=`ordering`-jitterAmount, y=`count_start`, yend=`count_end`, group=`Rating`, color=`Rating`), data=to_plot_least, size=binsize) +
  ggplot2::geom_segment(ggplot2::aes(x=`ordering`+jitterAmount, xend=`ordering`+jitterAmount, y=`count_start`, yend=`count_end`, group=`status`, color=`Rating`), data=to_plot_most, size=binsize) +
  ## for slide
  ggplot2::facet_wrap(.~`status`, ncol=2) +
  ## for paper
  ## ggplot2::facet_wrap(`Rating`~`status`, ncol=2) +
  ggplot2::xlab(xlabel) +
  ggplot2::ylab(sprintf("ln(%s in kBtu / gross sqft) * 1000", ylabel)) +
  ## ggplot2::coord_cartesian(ylim=c(-300, 750)) +
  ggplot2::theme_bw() +
  ggplot2::scale_color_manual(values=c("#A1D99B", "#31A354")) +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
                legend.title=element_blank(), legend.position = "bottom")
ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/facet_pubpri_bin_coef_ci_greeness_separate_organization_greeness_%s_slide%s.png", modelsuffix, energy_type), width=8, height=5, units="in")
## ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_greeness_separate_organization_greeness_%s.png", modelsuffix), width=6, height=7, units="in")


allDataGreen %>%
  distinct(Organization, Greeness, Name) %>%
  dplyr::group_by(Organization, Greeness) %>%
  dplyr::summarise(n())

## produce coefficient plot by climate region for separate portfolio using connected dashed lines for conf int
breaklevels = c("avgTempBelow10", "avgTemp10to20", "avgTemp20to30", "avgTemp30to40",
                "avgTemp40to50", "avgTemp50to60", "avgTemp60to70", "avgTemp70to80", "avgTemp80to90",
                "avgTempAbove90")
breaklabels = c("<10", "[10-20)", "[20-30)", "[30-40)",
                "[40-50)", "[50-60)", "[60-70)", "[70-80)", "[80-90)",
                ">90")
modelname = "baseline"
xlabel = "Temperature Bin"
process_climate_result <- function(modelname, modellabel, energy_type, region_keyword) {
## produce the bin model coefficient and CI plot for greeness analysis for the model for just one portfolio
  df_recode_xi =
    readr::read_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/header_to_%s_owner.csv", region_keyword)) %>%
    {.}
  filename = sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_%s_%s%s.txt", region_keyword, modelname,
                     energy_type)
  print(filename)
  fields_to_remove = c("0b.", "eDate", "eState", "hasretrofit", "Constant", "o.avgTempBelow10")
  fields_to_keep = NULL
  outreg_vars = c("coef", "se", "ci_low", "ci_high")
  vars_to_keep = c("coef", "ci_low", "ci_high")
  df_model = read_outreg2(filename=filename, fields_to_remove=fields_to_remove, fields_to_keep=fields_to_keep,
                          outreg_vars=outreg_vars, vars_to_keep=vars_to_keep)
  ## df_model =
  ##   readr::read_tsv(),
  ##                   skip=7, col_names=FALSE) %>%
  ##   ## remove bottom non-conformative rows
  ##   head(-5) %>%
  ##   dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
  ##   tidyr::fill(`X1`) %>%
  ##   tidyr::unite(`temp`, `X1`, `variable`, sep="-") %>%
  ##   ## modify this to X2:X?, ? is the number of models + 1
  ##   tidyr::gather(`header`, `value`, X2:X9) %>%
  ##   tidyr::separate(`temp`, c("X1", "variable"), sep="-") %>%
  ##   dplyr::filter(`variable` != "se",
  ##                 !grepl("0b.", `X1`, fixed = TRUE),
  ##                 !grepl("eDate", `X1`, fixed = TRUE),
  ##                 !grepl("eState", `X1`, fixed = TRUE),
  ##                 !grepl("hasretrofit", `X1`, fixed = TRUE),
  ##                 !grepl("Constant", `X1`, fixed = TRUE),
  ##                 !grepl("o.avgTempBelow10", `X1`, fixed = TRUE)
  ##                 ) %>%
  df_zero = data.frame(X1="avgTemp60to70", variable=rep(c("coef", "ci_low", "ci_high"), 8),
                       header=sprintf("X%s", sort(rep(2:(length(unique(df_recode_xi$header)) + 1), 3))), value=0)
  df_model <- df_model %>%
    tidyr::unite(`temp`, `X1`, `variable`, sep="-") %>%
    tidyr::gather(`header`, `value`, starts_with("X")) %>%
    ## tidyr::gather(`header`, `value`, X2:X9) %>%
    tidyr::separate(`temp`, c("X1", "variable"), sep="-") %>%
    dplyr::mutate(`value`=gsub("\\(", "", `value`)) %>%
    dplyr::mutate(`value`=gsub("\\)", "", `value`)) %>%
    dplyr::mutate(`value`=gsub("\\*", "", `value`)) %>%
    dplyr::mutate(`value`=as.numeric(`value`)) %>%
    na.omit() %>%
    dplyr::bind_rows(df_zero) %>%
    dplyr::mutate(`X1`=factor(X1, levels=breaklevels)) %>%
    dplyr::mutate(`ordering`=match(`X1`, breaklevels)) %>%
    dplyr::left_join(df_recode_xi, by="header") %>%
    dplyr::mutate(`model`=modellabel) %>%
    {.}
  return(df_model)
}

breaklevels = c("avgTempBelow10", "avgTemp10to20", "avgTemp20to30", "avgTemp30to40",
                "avgTemp40to50", "avgTemp50to60", "avgTemp60to70", "avgTemp70to80", "avgTemp80to90",
                "avgTempAbove90")
breaklabels = c("<10", "[10-20)", "[20-30)", "[30-40)",
                "[40-50)", "[50-60)", "[60-70)", "[70-80)", "[80-90)",
                ">90")
## energy_type = "_gas" ## use "" for electricity
energy_type = "_total"
## region_keyword = "climateregion"
## region_keyword = "usregion"
region_keyword = "region_wiki"
## regioncol = "US Climate Region"
## regioncol = "USRegion"
regioncol = "region_wiki"
## modelname = "statetrend"
modelname = "baseline"
dfresult =
  process_climate_result(modelname=modelname, modellabel="baseline", energy_type=energy_type,
                         region_keyword=region_keyword) %>%
  ## dplyr::bind_rows(process_climate_result(modelname="statetrend", modellabel="state specific trend",
  ##                                         energy_type=energy_type,
  ##                        region_keyword=region_keyword)) %>%
  {.}

## remove 10F and 90F bins
dfresult <- dfresult %>%
  dplyr::filter(!(`X1` %in% c("avgTempBelow10", "avgTempAbove90"))) %>%
  {.}

head(dfresult)
min(dfresult$value)
max(dfresult$value)

(dfresult) %>%
  distinct(!!rlang::sym(regioncol), `status`, `model`)

if (energy_type == "") {
  shifty=-400
  yupperlimit = 750
  scaling = 1 / 1e3 * 1.5
} else if (energy_type == "_gas") {
  shifty=-900
  yupperlimit = 900
  scaling = 1 / 1e3 * 1.5
} else if (energy_type == "_total") {
  ## shifty=-130
  ## yupperlimit = 220
  ## scaling = 1 / 1e4 * 4
  shifty=-50
  yupperlimit = 80
  scaling = 1 / 1e4 * 1.5
}
temperatureBinData =
  allData %>%
  {.}
temperatureBinData <-
  temperatureBinData %>%
  dplyr::select(`Organization`, !!rlang::sym(regioncol), `<10`, starts_with("["), `>90`) %>%
  tidyr::gather(`tempBin`, `value`, `<10`:`>90`) %>%
  dplyr::group_by(`Organization`, !!rlang::sym(regioncol), `tempBin`) %>%
  dplyr::summarise(count = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`count`=`count` * scaling) %>%
  dplyr::mutate(`count_start`=shifty, `count_end`=`count` + shifty) %>%
  dplyr::mutate(`ordering`=match(`tempBin`, breaklabels)) %>%
  dplyr::mutate(`status`=ifelse(`Organization`=="GSA", "public", "private")) %>%
  dplyr::select(`count_start`, `count_end`, `ordering`, `status`, !!rlang::sym(regioncol)) %>%
  {.}

head(temperatureBinData)

to_plot <- dfresult %>%
  dplyr::left_join(temperatureBinData, by=c("ordering", "status", regioncol)) %>%
  dplyr::mutate(`status`=factor(`status`, levels=c("public", "private"))) %>%
  {.}
to_plot_public <- to_plot %>%
  dplyr::filter(`status`=="public") %>%
  {.}
to_plot_private <- to_plot %>%
  dplyr::filter(`status`=="private") %>%
  {.}
to_plot_north <- to_plot %>%
  dplyr::filter(!!rlang::sym(regioncol)=="North") %>%
  {.}
to_plot_south <- to_plot %>%
  dplyr::filter(!!rlang::sym(regioncol)=="South") %>%
  {.}

## for paper
## binsize = 4
## jitterAmount = 0.3
## for slides
binsize = 1.7
jitterAmount = 0.15
if (energy_type == "_total") {
  binsize = 3.4
  jitterAmount = 0.17
}

## facet by portfolio
p <- to_plot %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x=ordering, y=value, color=!!rlang::sym(regioncol)), size=0.7) +
  ggplot2::geom_line(ggplot2::aes(x=ordering, y=value, linetype=variable, color=!!rlang::sym(regioncol))) +
  scale_linetype_manual(values=c("dashed", "dashed", "solid"), breaks=c("ci_low", NA, "coef"), labels=c("95% C.I.", "", "Coefficient")) +
  scale_x_continuous(breaks=1:length(breaklabels), labels=breaklabels) +
  ggplot2::geom_segment(ggplot2::aes(x=`ordering`-jitterAmount, xend=`ordering`-jitterAmount, y=`count_start`, yend=`count_end`, group=!!rlang::sym(regioncol), color=!!rlang::sym(regioncol)), data=to_plot_north, size=binsize) +
  ggplot2::geom_segment(ggplot2::aes(x=`ordering`+jitterAmount, xend=`ordering`+jitterAmount, y=`count_start`, yend=`count_end`, group=!!rlang::sym(regioncol), color=!!rlang::sym(regioncol)), data=to_plot_south, size=binsize) +
  ## for paper
  ## ggplot2::facet_wrap(`US Climate Region`~`model`, ncol=2) +
  ## for slides
  ## use nrow=2 for climate region plots
  ggplot2::facet_wrap(as.formula(paste("model~", "status")), ncol=2) +
  ## ggplot2::facet_wrap(`model`~`USRegion`, nrow=2) +
  ggplot2::xlab("Temperature Bin") +
  ggplot2::ylab("ln(Electricity in kBtu / gross sqft) * 1000") +
  ggplot2::coord_cartesian(ylim=c(shifty, yupperlimit)) +
  ## this is to plot slide_zoom for electric
  ## ggplot2::ylim(c(-100, 100)) +
  ## this is to plot slide_zoom for gas
  ## ggplot2::ylim(c(-150, 150)) +
  ggplot2::theme_bw() +
  ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(3, "RdBu")[c(3, 1)], name="US Region") +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
                legend.title=element_blank(), legend.position = "bottom")
  print(p)
if (regioncol == "US Climate Region") {
  ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_climate_slide_zoom%s.png", energy_type), width=8, height=5, units="in")
## ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_climate_slide%s.png", energy_type), width=8, height=5, units="in")
## for paper
## ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_climate.png", width=8, height=7, units="in")
}
else {
  ## ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_%s_slide_zoom%s.png", regioncol, energy_type), width=8, height=5, units="in")
  ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_%s_slide%s_%s.png", regioncol, energy_type, modelname), width=8, height=5, units="in")
  ## for paper
  ## ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_usregion.png", width=8, height=7, units="in")
}

## facet by region
p <- to_plot %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x=ordering, y=value, color=status), size=0.7) +
  ggplot2::geom_line(ggplot2::aes(x=ordering, y=value, linetype=variable, color=status)) +
  scale_linetype_manual(values=c("dashed", "dashed", "solid"), breaks=c("ci_low", NA, "coef"), labels=c("95% C.I.", "", "Coefficient")) +
  scale_x_continuous(breaks=1:length(breaklabels), labels=breaklabels) +
  ggplot2::geom_segment(ggplot2::aes(x=`ordering`-jitterAmount, xend=`ordering`-jitterAmount, y=`count_start`, yend=`count_end`, group=!!rlang::sym("status"), color=!!rlang::sym("status")), data=to_plot_public, size=binsize) +
  ggplot2::geom_segment(ggplot2::aes(x=`ordering`+jitterAmount, xend=`ordering`+jitterAmount, y=`count_start`, yend=`count_end`, group=!!rlang::sym("status"), color=!!rlang::sym("status")), data=to_plot_private, size=binsize) +
  ## for paper
  ## ggplot2::facet_wrap(`US Climate Region`~`model`, ncol=2) +
  ## for slides
  ## use nrow=2 for climate region plots
  ggplot2::facet_wrap(as.formula(paste("model~", regioncol)), ncol=2) +
  ## ggplot2::facet_wrap(`model`~`USRegion`, nrow=2) +
  ggplot2::xlab("Temperature Bin") +
  ggplot2::ylab("ln(Electricity in kBtu / gross sqft) * 1000") +
  ggplot2::coord_cartesian(ylim=c(shifty, yupperlimit)) +
  ## this is to plot slide_zoom for electric
  ## ggplot2::ylim(c(-100, 100)) +
  ## this is to plot slide_zoom for gas
  ## ggplot2::ylim(c(-150, 150)) +
  ggplot2::theme_bw() +
  ggplot2::scale_color_grey() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1),
                legend.title=element_blank(), legend.position = "bottom")
  print(p)
if (regioncol == "US Climate Region") {
  ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_climate_slide_zoom%s_facetRegion.png", energy_type), width=8, height=5, units="in")
## ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_climate_slide%s_facetRegion.png", energy_type), width=8, height=5, units="in")
## for paper
## ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_climate_facetRegion.png", width=8, height=7, units="in")
}
else {
  ## ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_%s_slide_zoom%s_facetRegion.png", regioncol, energy_type), width=8, height=5, units="in")
  ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_%s_slide%s_%s_facetRegion.png", regioncol, energy_type, modelname), width=8, height=5, units="in")
  ## for paper
  ## ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/bin_coef_ci_usregion_facetRegion.png", width=8, height=7, units="in")
}

## generate regression result summary table for bin model with greeness
## portfolio="PNC"
## status_code = "private"
portfolio="GSA"
status_code = "public"
summary_portfolio = readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_results_bin_greeness_0720_%s.txt", portfolio), skip=7, col_names=FALSE) %>%
  head(-5) %>%
  dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
    tidyr::fill(`X1`) %>%
  dplyr::filter(!(`variable` %in% c("ci_low", "ci_high")),
                !grepl("1b.eGreeness", `X1`, fixed = TRUE),
                !grepl("eDate", `X1`, fixed = TRUE),
                !grepl("eState", `X1`, fixed = TRUE)
                ) %>%
  dplyr::mutate(`X1`=gsub("2.eGreeness#c.", "Moderate Green x ", `X1`)) %>%
  dplyr::mutate(`X1`=gsub("3.eGreeness#c.", "Most Green x ", `X1`)) %>%
  dplyr::mutate(`X1`=ifelse(as.numeric(rownames(.)) %% 2 == 1, `X1`, "")) %>%
  {.}
tail_summary_portfolio = readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_results_bin_greeness_0720_%s.txt", portfolio), skip=7, col_names=FALSE) %>%
  tail(5) %>%
  {.}
summary_export = summary_portfolio %>%
  dplyr::bind_rows(tail_summary_portfolio) %>%
  {.}
summary_export %>%
  readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_greeness_model_%s.csv", portfolio))
summary_export <-
  summary_export %>%
  dplyr::select(-`variable`) %>%
  dplyr::rename(" "=`X1`, "(1)"=`X2`, "(2)"=`X3`) %>%
  {.}
result_table_tex <- xtable(summary_export, caption=sprintf("Regression result for the %s portfolio", status_code))
print(result_table_tex, tabular.environment = "longtable", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/%s_greeness_result.tex", portfolio))

# long table of state temperature
avg_temp_state =
  readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/stateAverageTemperature2011to2013.csv") %>%
  tibble::as_data_frame() %>%
  dplyr::select(`Location`, `Value`) %>%
  dplyr::rename(`Average temperature (F)`=`Value`, `State`=`Location`) %>%
  {.}
result_table_tex <- xtable(avg_temp_state, caption="Average state temperature from 2011 to 2013")
print(result_table_tex, tabular.environment = "longtable", include.rownames=FALSE,
      file="~/Dropbox/thesis/writeups/policy_cmp/tables/avg_state_temp.tex", size="\\footnotesize")

## only plot the temperature bin model
models = "Temperature Bin"
plotHeight = 6
## plot all models
## models=c("No Control", "HDD CDD", "Polynomial degree 2 HDD and CDD", "Temperature Bin")
## warmth_class_method = "building temperature"
## warmth_class_method = "state latitude"
warmth_class_method = "state temperature"

## plot coefficients for retrofit climate event study
pd <- position_dodge(width = 0.4)
## quarterly plot setting starts -------------------------
duration = "quarterly"
energy_type = ""
## energy_type = "_gas"
endbound = 20
fatten_amount = 2.5
size = 0.2
point_size = 1.0
## quarterly plot setting ends   -------------------------
## monthly plot setting starts -------------------------
## duration = "monthly"
## energy_type = "_gas"
## endbound = 60
## fatten_amount = 2.5
## size = 0.2
## point_size = 1.0
## monthly plot setting ends -------------------------
## yearly plot setting starts -------------------------
## duration = "yearly"
## endbound = 5
## fatten_amount = 2.5
## size = 1.0
## point_size = 2.5
## yearly plot setting ends -------------------------
retrofits = c("Advanced Metering", "Building Envelope", "HVAC", "Lighting", "GSALink", "Bundle")
names(retrofits) = c("metering", "envelope", "hvac", "lighting", "GSALink", "bundle")
if (energy_type == "") {
  energy_label = "Electricity"
} else if (energy_type == "_gas") {
  energy_label = "Gas"
}
## for (fileSuffix in c("metering")) {
for (fileSuffix in c("metering", "envelope", "hvac", "lighting", "GSALink", "bundle")) {
  retrofit = retrofits[[fileSuffix]]
  if (warmth_class_method == "building temperature") {
    p <-
      readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/%s_buildingTemp_event_study_%s%s.txt", duration, fileSuffix, energy_type), skip=7, col_names=FALSE)
  } else if (warmth_class_method == "state temperature") {
    p <-
      readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/%s_event_study_%s%s.txt", duration, fileSuffix, energy_type), skip=7, col_names=FALSE)
  } else if (warmth_class_method == "state latitude") {
    p <-
      readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/%s_stateLat_event_study_%s%s.txt", duration, fileSuffix, energy_type), skip=7, col_names=FALSE)
  }
  ## remove bottom non-conformative rows
  p <- p %>%
    head(-5) %>%
    dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
    tidyr::fill(`X1`) %>%
    tidyr::unite(`temp`, `X1`, `variable`, sep="-") %>%
    ## modify this to X2:X?, ? is the number of models + 1
    tidyr::gather(`model`, `value`, X2:X5) %>%
    tidyr::separate(`temp`, c("X1", "variable"), sep="-") %>%
    dplyr::filter(`variable` != "se",
                  !grepl("o.tau_bounded_post", `X1`, fixed = TRUE),
                  !grepl("0b.tau_bounded_post", `X1`, fixed = TRUE),
                  !grepl("3b.eWarmth", `X1`, fixed = TRUE),
                  !grepl("o.eWarmth", `X1`, fixed = TRUE),
                  !grepl("cdd", `X1`, fixed=TRUE),
                  !grepl("hdd", `X1`, fixed=TRUE),
                  !grepl("Constant", `X1`, fixed=TRUE),
                  !grepl("avgTemp", `X1`, fixed=TRUE),
                  ) %>%
  {.}
  if (duration == "year") {
    p <- p %>%
      dplyr::filter(!grepl(".year", `X1`, fixed = TRUE)) %>%
      {.}
  } else if ((duration == "monthly") | (duration == "quarterly")){
    print("asdf")
    p <- p %>%
      dplyr::filter(!grepl(".eDate", `X1`, fixed = TRUE)) %>%
      {.}
  }
  p <- p %>%
    dplyr::mutate(`value`=gsub("\\(", "", `value`)) %>%
    dplyr::mutate(`value`=gsub("\\)", "", `value`)) %>%
    dplyr::mutate(`value`=gsub("\\*", "", `value`)) %>%
    dplyr::mutate(`value`=gsub(",", "", `value`)) %>%
    tidyr::unite(`temp`, `X1`, `model`, sep="-") %>%
    tidyr::spread(variable, value) %>%
    tidyr::separate(`temp`, c("X1", "model"), sep="-") %>%
    dplyr::filter(!is.na(`coef`)) %>%
    ## readr::write_csv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/%s_coef_ci.csv", fileSuffix))
    dplyr::mutate(`coef`=as.numeric(`coef`),
                  `ci_high`=as.numeric(`ci_high`),
                  `ci_low`=as.numeric(`ci_low`),
                  ) %>%
    dplyr::mutate(warmth = ifelse(grepl("1.eWarmth", X1, fixed=TRUE), "cold",
                          ifelse(grepl("2.eWarmth", X1, fixed=TRUE), "hot", "mild"))) %>%
    dplyr::mutate(`tau`=as.integer(substr(X1, 1, stringi::stri_locate(X1, fixed=".", mode="first")-1))) %>%
    dplyr::mutate(`tau`=ifelse(`tau` <= endbound, `tau`, `tau` - 2*endbound - 1)) %>%
    dplyr::mutate(`warmth`=ifelse(as.numeric(tau) < 0, "pre-retrofit all warmth level", `warmth`)) %>%
    dplyr::mutate_at(vars(model), recode, "X2"="No Control", "X3"="HDD CDD", "X4"="Polynomial degree 2 HDD and CDD",
                      "X5"="Temperature Bin") %>%
    dplyr::select(-`X1`) %>%
    dplyr::bind_rows(data.frame(ci_low=NA, ci_high=NA, coef=0, term=NA,
                                warmth=rep(c("cold", "hot", "mild", "pre-retrofit all warmth level"), 4),
                                model=c(rep("No Control", 4), rep("HDD CDD", 4),
                                        rep("Polynomial degree 2 HDD and CDD", 4), rep("Temperature Bin", 4)),
                                tau=0)) %>%
    {.}
  print(head(p))
  p <- p %>%
    dplyr::filter(`model` %in% models) %>%
    {.}
  p <- p %>%
    ggplot2::ggplot(ggplot2::aes(x=tau, y=coef, color=warmth, group=warmth)) +
    ggplot2::geom_line() +
    ## ggplot2::geom_point() +
    ggplot2::geom_pointrange(ggplot2::aes(x=tau, y=`coef`, ymin=`ci_low`, ymax=`ci_high`, color=warmth, group=warmth), position=pd, shape=18, fatten=fatten_amount, size=size) +
    ggplot2::geom_point(x=0, y=0, shape=18, size=point_size) +
    ggplot2::facet_wrap(`model`~., ncol=2) +
    ggplot2::xlab("Number of years before of after retrofit") +
    ggplot2::ylab(NULL) +
    ## ggplot2::coord_cartesian(ylim=c(-20, 25)) +
    ## ggplot2::scale_color_brewer(palette="Spectral") +
    ## ggplot2::scale_color_brewer(palette="RdBu") +
    ## ggplot2::scale_color_manual(values=c("#5698C4", "#E97550", "grey")) +
    ## ggplot2::scale_color_manual(values=c("#d55e00", "#8B8CB0", "skyblue")) +
    ## ggplot2::scale_color_manual(values=c("#d55e00", "#009e73", "skyblue")) +
    ## logan's
    ## ggplot2::scale_color_manual(values=colorRampPalette(c("#FF0000","#000066"))(3L)) +
    ggplot2::scale_color_manual(values=c("#67A9CF", "#EF8A62", "grey70", "black")) +
    ## ggplot2::theme_dark() +
    ggplot2::theme_bw() +
    ggtitle(sprintf("%s Event study \"%s\" retrofit\nWarmth level by %s",
                    energy_label, retrofit, warmth_class_method)) +
    ggplot2::theme(legend.position = "bottom",
                   plot.title = element_text(size=slide_size))
  print(p)
  ## ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/%s_retrofit_coef_noci_%s.png", duration, fileSuffix), width=8, height=5, units="in")
  if (warmth_class_method == "building temperature") {
    ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/%s_buildingTemp_retrofit_coef_ci_%s_%s%s.png", duration, fileSuffix, length(models), energy_type), width=8, height=plotHeight, units="in")
  } else if (warmth_class_method == "state temperature") {
    ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/%s_retrofit_coef_ci_%s_%s%s.png", duration, fileSuffix, length(models), energy_type), width=8, height=plotHeight, units="in")
  } else if (warmth_class_method == "state latitude") {
    ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/%s_stateLat_retrofit_coef_ci_%s_%s%s.png", duration, fileSuffix, length(models), energy_type), width=8, height=plotHeight, units="in")
  }
}

df_recode_retrofit =
  readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/separate_model_monthly_header_to_retrofit.csv") %>%
  {.}
## plot coefficients for retrofit climate event study with each warmth level fitted separately
## plot all models
## models = c("no_control", "hddcdd", "hdd2cdd2", "bin")
## plotHeight = 5
## plot one model
models = "bin"
plotHeight = 6
slide_size = 22
## warmth_class_method = "state temperature"
## warmth_class_method = "state latitude"
warmth_class_method = "building temperature"

pd <- position_dodge(width = 0.4)
## quarterly plot setting starts -------------------------
duration = "quarterly"
## energy_type = ""
energy_type = "_gas"
endbound = 20
fatten_amount = 2.5
size = 0.2
point_size = 1.0
## quarterly plot setting ends -------------------------
## monthly plot setting starts -------------------------
## duration = "monthly"
## energy_type = "_gas"
## endbound = 60
## fatten_amount = 2.5
## size = 0.2
## point_size = 1.0
## monthly plot setting ends -------------------------
## yearly plot setting starts -------------------------
## duration = "yearly"
## endbound = 5
## fatten_amount = 2.5
## size = 1.0
## point_size = 2.5
## yearly plot setting ends -------------------------
if (energy_type == "") {
  energy_label = "Electricity"
} else if (energy_type == "_gas") {
  energy_label = "Gas"
}
acc <- lapply(models, function(model_type) {
  if (warmth_class_method == "state temperature") {
    dfacc =
      readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/separate_model_%s_retrofit_climate_%s%s.txt", duration, model_type, energy_type), skip=7, col_names=FALSE)
  } else if (warmth_class_method == "state latitude") {
    dfacc =
      readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/separate_model_%s_stateLat_retrofit_climate_%s%s.txt", duration, model_type, energy_type), skip=7, col_names=FALSE)
  } else if (warmth_class_method == "building temperature") {
    dfacc =
      readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/separate_model_%s_buildingTemp_retrofit_climate_%s%s.txt", duration, model_type, energy_type), skip=7, col_names=FALSE)
  }
  dfacc <- dfacc %>%
    ## remove bottom non-conformative rows
    head(-5) %>%
    dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
    tidyr::fill(`X1`) %>%
    tidyr::unite(`temp`, `X1`, `variable`, sep="-") %>%
    ## modify this to X2:X?, ? is the number of models + 1
    tidyr::gather(`header`, `value`, X2:X19) %>%
    tidyr::separate(`temp`, c("X1", "variable"), sep="-") %>%
    dplyr::filter(`variable` != "se",
                  !grepl(".eDate", `X1`, fixed = TRUE),
                  !grepl("o.tau_bounded_post", `X1`, fixed = TRUE),
                  !grepl("0b.tau_bounded_post", `X1`, fixed = TRUE),
                  !grepl("cdd", `X1`, fixed=TRUE),
                  !grepl("hdd", `X1`, fixed=TRUE),
                  !grepl("Constant", `X1`, fixed=TRUE),
                  !grepl("avgTemp", `X1`, fixed=TRUE),
                  ) %>%
    dplyr::mutate(`value`=gsub("\\(", "", `value`)) %>%
    dplyr::mutate(`value`=gsub("\\)", "", `value`)) %>%
    dplyr::mutate(`value`=gsub("\\*", "", `value`)) %>%
    dplyr::mutate(`value`=gsub(",", "", `value`)) %>%
    tidyr::unite(`temp`, `X1`, `header`, sep="-") %>%
    tidyr::spread(variable, value) %>%
    tidyr::separate(`temp`, c("X1", "header"), sep="-") %>%
    dplyr::filter(!is.na(`coef`)) %>%
    dplyr::mutate(`coef`=as.numeric(`coef`),
                  `ci_high`=as.numeric(`ci_high`),
                  `ci_low`=as.numeric(`ci_low`),
                  ) %>%
    dplyr::mutate(`tau`=as.integer(substr(X1, 1, stringi::stri_locate(X1, fixed=".", mode="first")-1))) %>%
    dplyr::mutate(`tau`=ifelse(`tau` <= endbound, `tau`, `tau` - 2*endbound - 1)) %>%
    dplyr::left_join(df_recode_retrofit, by="header") %>%
    dplyr::mutate(`model`=model_type) %>%
    dplyr::select(-`X1`, -`header`) %>%
    {.}
})
modelResult = do.call(rbind, acc)
retrofits = c("Advanced Metering", "Building Envelope", "HVAC", "Lighting", "GSALink", "Bundle")
names(retrofits) = c("metering", "envelope", "hvac", "lighting", "GSALink", "bundle")
for (fileSuffix in names(retrofits)) {
  retrofit_type <- retrofits[[fileSuffix]]
  print(retrofit_type)
  p <- modelResult %>%
    dplyr::bind_rows(data.frame(`tau`=0, `ci_low`=0, `ci_high`=0, `coef`=0, `retrofit`=retrofit_type,
                                `warmth`=rep(c("mild", "cold", "hot"), 4),
                                `model`=c(rep("no_control", 3), rep("hddcdd", 3), rep("hdd2cdd2", 3),
                                          rep("bin", 3)))) %>%
    dplyr::filter(`model` %in% models) %>%
    dplyr::filter(retrofit == retrofit_type) %>%
    ggplot2::ggplot(ggplot2::aes(x=tau, y=coef, color=warmth, group=warmth)) +
    ggplot2::geom_line() +
    ggplot2::geom_pointrange(ggplot2::aes(x=tau, y=`coef`, ymin=`ci_low`, ymax=`ci_high`, color=warmth, group=warmth), position=pd, shape=18, fatten=fatten_amount, size=size) +
    ggplot2::geom_point(x=0, y=0, shape=18, size=point_size) +
    ggplot2::facet_wrap(`model`~., ncol=2) +
    ggplot2::xlab("Number of years before of after retrofit") +
    ggplot2::ylab(NULL) +
    ggplot2::scale_color_manual(values=c("#67A9CF", "#EF8A62", "grey70", "black")) +
    ggplot2::theme_bw() +
    ggtitle(sprintf("%s Event study \"%s\" retrofit\nWarmth level by %s", energy_label, retrofit_type, warmth_class_method)) +
    ggplot2::theme(legend.position = "bottom",
                   plot.title = element_text(size=slide_size))
  print(p)
  if (warmth_class_method == "state temperature") {
    ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/sep_%s_retrofit_coef_ci_%s_%s%s.png", duration, fileSuffix, length(models), energy_type),width=8, height=plotHeight, units="in")
  } else if (warmth_class_method == "state latitude") {
    ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/sep_%s_stateLat_retrofit_coef_ci_%s_%s%s.png", duration, fileSuffix, length(models), energy_type), width=8, height=plotHeight, units="in")
  } else if (warmth_class_method == "building temperature") {
    ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/sep_%s_buildingTemp_retrofit_coef_ci_%s_%s%s.png", duration, fileSuffix, length(models), energy_type), width=8, height=plotHeight, units="in")
  }
}

## generate summary statistics tables
retrofits = c("Advanced Metering", "Building Envelope", "HVAC", "Lighting", "GSALink", "")
names(retrofits) = c("metering", "envelope", "hvac", "lighting", "GSALink", "bundle")
## for (fileSuffix in c("metering")) {
for (fileSuffix in c("metering", "envelope", "hvac", "lighting", "GSALink", "bundle")) {
  retrofit = retrofits[[fileSuffix]]
  summary_head = readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/yearly_event_study_%s.txt", fileSuffix), skip=7, col_names=FALSE) %>%
  ## remove bottom non-conformative rows
  head(-5) %>%
  dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
  tidyr::fill(`X1`) %>%
  dplyr::filter(!(`variable` %in% c("ci_low", "ci_high"))) %>%
  dplyr::filter(
           ## `variable` != "se",
                !grepl(".year", `X1`, fixed = TRUE),
                !grepl("o.tau_bounded_post", `X1`, fixed = TRUE),
                !grepl("0b.tau_bounded_post", `X1`, fixed = TRUE),
                !grepl("3b.eWarmth", `X1`, fixed = TRUE),
                !grepl("o.eWarmth", `X1`, fixed = TRUE),
                ) %>%
  dplyr::mutate(X1 = gsub("1.eWarmth", "cold", X1)) %>%
  dplyr::mutate(X1 = gsub("2.eWarmth", "hot", X1)) %>%
  dplyr::mutate(X1 = gsub("1.tau_bounded_post", "$I[\\\\tau=1]$", X1)) %>%
  dplyr::mutate(X1 = gsub("2.tau_bounded_post", "$I[\\\\tau=2]$", X1)) %>%
  dplyr::mutate(X1 = gsub("3.tau_bounded_post", "$I[\\\\tau=3]$", X1)) %>%
  dplyr::mutate(X1 = gsub("4.tau_bounded_post", "$I[\\\\tau=4]$", X1)) %>%
  dplyr::mutate(X1 = gsub("5.tau_bounded_post", "$I[\\\\tau>=5]$", X1)) %>%
  dplyr::mutate(X1 = gsub("1.tau_bounded_shift", "$I[\\\\tau=1]$", X1)) %>%
  dplyr::mutate(X1 = gsub("2.tau_bounded_shift", "$I[\\\\tau=2]$", X1)) %>%
  dplyr::mutate(X1 = gsub("3.tau_bounded_shift", "$I[\\\\tau=3]$", X1)) %>%
  dplyr::mutate(X1 = gsub("4.tau_bounded_shift", "$I[\\\\tau=4]$", X1)) %>%
  dplyr::mutate(X1 = gsub("5.tau_bounded_shift", "$I[\\\\tau=5]$", X1)) %>%
  dplyr::mutate(X1 = gsub("6.tau_bounded_shift", "$I[\\\\tau<=-5]$", X1)) %>%
  dplyr::mutate(X1 = gsub("7.tau_bounded_shift", "$I[\\\\tau=-4]$", X1)) %>%
  dplyr::mutate(X1 = gsub("8.tau_bounded_shift", "$I[\\\\tau=-3]$", X1)) %>%
  dplyr::mutate(X1 = gsub("9.tau_bounded_shift", "$I[\\\\tau=-2]$", X1)) %>%
  dplyr::mutate(X1 = gsub("10.tau_bounded_shift", "$[\\\\tau=-1]$", X1)) %>%
  dplyr::mutate(X1 = gsub("#", "$\\\\times$", X1)) %>%
  dplyr::mutate(`X1`=ifelse(as.numeric(rownames(.)) %% 2 == 1, `X1`, "")) %>%
  ## dplyr::select(-`variable`) %>%
  {.}
  print(head(summary_head))
  summary_tail = readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/yearly_event_study_%s.txt", fileSuffix), skip=7, col_names=FALSE) %>%
    tail(5) %>%
    dplyr::mutate(`X1` = gsub(" ci_high", "", `X1`)) %>%
    dplyr::mutate(`X1` = gsub("<", "$<$", `X1`)) %>%
    {.}
  print(head(summary_tail))
  summary_export = summary_head %>%
    dplyr::bind_rows(summary_tail) %>%
    {.}
  summary_export %>%
    readr::write_csv(sprintf("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_retrofit_model_%s.csv", fileSuffix))
  summary_export <-
    summary_export %>%
    dplyr::select(-`variable`) %>%
    dplyr::rename(" "=`X1`, "(1)"=`X2`, "(2)"=`X3`, "(3)"=`X4`, "(4)"=`X5`) %>%
    {.}
  result_table_tex <- xtable(summary_export, caption=sprintf("Regression result for event study model of buildings with %s retrofit", fileSuffix))
  print(result_table_tex, tabular.environment = "longtable", include.rownames=FALSE,
        file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/summary_retrofit_model_%s.tex", fileSuffix), size="\\footnotesize", sanitize.text.function=function(x){x})
  ## above sanitize function overwritten so that the math symbols stay the original way
}

## plot coefficients for retrofit climate event study with each warmth level fitted separately
pd <- ggplot2::position_dodge(width = 0.4)
## quarterly plot setting starts -------------------------
energy_type = ""
## energy_type = "_gas"
duration = "quarterly"
additive_keywords = "nonsingle_"
if (additive_keywords == "") {
  df_recode_retrofit = data.frame(header=sprintf("X%s", 2:7),
                                  retrofit=c("Advanced Metering", "Bundle", "Building Envelope", "GSALink", "HVAC",
                                             "Lighting"))
} else if (additive_keywords == "nonsingle_") {
  df_recode_retrofit = data.frame(header=sprintf("X%s", 2:7),
                                  retrofit=c("Advanced Metering", "Building Envelope", "Commissioning", "GSALink",
                                             "HVAC", "Lighting"))
}
endbound = 20
fatten_amount = 2.5
size = 0.2
point_size = 1.0
## quarterly plot setting starts -------------------------
## monthly plot setting starts -------------------------
## energy_type = ""
## ## energy_type = "_gas"
## duration = "monthly"
## endbound = 60
## fatten_amount = 2.5
## size = 0.2
## point_size = 1.0
## produce plots with all 4 model type
## models = c("no_control", "hddcdd", "hdd2cdd2", "bin")
## produce plots with only one model type
models = c("bin")
## monthly plot setting ends -------------------------
## yearly plot setting starts -------------------------
## duration = "yearly"
## endbound = 5
## fatten_amount = 2.5
## size = 1.0
## point_size = 2.5
## yearly plot setting ends -------------------------
acc <- lapply(models, function(model_type) {
  dfacc =
    readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/%s_%sretrofit_no_climate_%s%s.txt", duration, additive_keywords, model_type, energy_type), skip=7, col_names=FALSE) %>%
    ## remove bottom non-conformative rows
    head(-5) %>%
    dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
    tidyr::fill(`X1`) %>%
    tidyr::unite(`temp`, `X1`, `variable`, sep="-") %>%
    ## modify this to X2:X?, ? is the number of models + 1
    tidyr::gather(`header`, `value`, X2:X7) %>%
    tidyr::separate(`temp`, c("X1", "variable"), sep="-") %>%
    dplyr::filter(`variable` != "se",
                  !grepl(".eDate", `X1`, fixed = TRUE),
                  !grepl("cdd", `X1`, fixed=TRUE),
                  !grepl("hdd", `X1`, fixed=TRUE),
                  !grepl("Constant", `X1`, fixed=TRUE),
                  !grepl("avgTemp", `X1`, fixed=TRUE),
                  ) %>%
    dplyr::mutate(`value`=gsub("\\(", "", `value`)) %>%
    dplyr::mutate(`value`=gsub("\\)", "", `value`)) %>%
    dplyr::mutate(`value`=gsub("\\*", "", `value`)) %>%
    dplyr::mutate(`value`=gsub(",", "", `value`)) %>%
    tidyr::unite(`temp`, `X1`, `header`, sep="-") %>%
    tidyr::spread(variable, value) %>%
    tidyr::separate(`temp`, c("X1", "header"), sep="-") %>%
    dplyr::filter(!is.na(`coef`)) %>%
    dplyr::mutate(`coef`=as.numeric(`coef`),
                  `ci_high`=as.numeric(`ci_high`),
                  `ci_low`=as.numeric(`ci_low`),
                  ) %>%
    dplyr::mutate(`tau`=as.integer(substr(X1, 1, stringi::stri_locate(X1, fixed=".", mode="first")-1))) %>%
    dplyr::mutate(`tau`=ifelse(`tau` <= endbound, `tau`, `tau` - 2*endbound - 1)) %>%
    dplyr::left_join(df_recode_retrofit, by="header") %>%
    dplyr::mutate(`model`=model_type) %>%
    dplyr::select(-`X1`, -`header`) %>%
    {.}
})

modelResult = do.call(rbind, acc)

unique(modelResult$model)

tail(modelResult)

if (additive_keywords == "") {
  retrofits = c("Advanced Metering", "Building Envelope", "HVAC", "Lighting", "GSALink", "Bundle")
  names(retrofits) = c("metering", "envelope", "hvac", "lighting", "GSALink", "bundle")
  ylower = c(-1, -1, -4.5, -1.5, -5, -1)
  names(ylower) = c("metering", "envelope", "hvac", "lighting", "GSALink", "bundle")
  yupper = c(1.2, 0.9, 2.2, 1.4, 5, 0.6)
  names(yupper) = c("metering", "envelope", "hvac", "lighting", "GSALink", "bundle")
} else if (additive_keywords == "nonsingle_") {
  retrofits = c("Advanced Metering", "Building Envelope", "HVAC", "Lighting", "GSALink", "Commissioning")
  names(retrofits) = c("metering", "envelope", "hvac", "lighting", "GSALink", "commissioning")
  ylower = c(-0.6, -0.8, -0.6, -0.75, -2, -0.75)
  names(ylower) = c("metering", "envelope", "hvac", "lighting", "GSALink", "commissioning")
  yupper = c(0.2, 0.25, 0.2, 0.25, 1.5, 0.25)
  names(yupper) = c("metering", "envelope", "hvac", "lighting", "GSALink", "commissioning")
}

if (energy_type == "") {
  energy_label = "Electricity"
} else if (energy_type == "_gas") {
  energy_label = "Gas"
}
slide_size = 22
for (fileSuffix in names(retrofits)) {
  retrofit_type <- retrofits[[fileSuffix]]
  print(retrofit_type)
  p <-
    modelResult %>%
    dplyr::filter(retrofit == retrofit_type) %>%
    dplyr::bind_rows(data.frame(`tau`=0, `ci_low`=0, `ci_high`=0, `coef`=0, `retrofit`=retrofit_type, `model`=models)) %>%
    ggplot2::ggplot(ggplot2::aes(x=tau, y=coef)) +
    ggplot2::geom_line() +
    ggplot2::geom_pointrange(ggplot2::aes(x=tau, y=`coef`, ymin=`ci_low`, ymax=`ci_high`), position=pd, shape=18, fatten=fatten_amount, size=size) +
    ggplot2::geom_point(x=0, y=0, shape=18, size=point_size)
  if (length(models) == 1) {
    p <- p +
      ggplot2::facet_wrap(`model`~.)
  } else {
    p <- p +
      ggplot2::facet_wrap(`model`~., ncol=2)
  }
  p <- p +
    ggplot2::xlab("Number of periods before or after retrofit") +
    ggplot2::ylab(NULL) +
    ggplot2::theme_bw() +
    ggtitle(sprintf("%s Event study model of \"%s\"", energy_label, retrofit_type))
  if (!is.null(ylower)) {
    p <- p +
      ggplot2::coord_cartesian(ylim=c(ylower[[fileSuffix]], yupper[[fileSuffix]]))
  }
  p <- p +
    ggplot2::theme(legend.position = "bottom",
                   plot.title = element_text(size=slide_size))
  print(p)
  ggplot2::ggsave(file=sprintf("~/Dropbox/thesis/code/pubPriCmp/image/%s_%sretrofit_no_climate_coef_ci_%s_%s%s.png", duration, additive_keywords, fileSuffix, length(models), energy_type), width=8, height=5, units="in")
}

## slide summary stats
df1 = allDataGreen %>%
  dplyr::mutate(`<40`=`<10` + `[10-20)` + `[20-30)` + `[30-40)`,
                `>80`=`[80-90)` + `>90`) %>%
  dplyr::select(`eui_elec`, `eui_gas`, `<40`, `>80`, `private`, `Name`) %>%
  dplyr::group_by(`private`) %>%
  dplyr::summarise(`eui_elec`=mean(`eui_elec`), `eui_gas`=mean(`eui_gas`), `<40`=mean(`<40`), `>80`=mean(`>80`), `building count`=n_distinct(Name)) %>%
  {.}

df2 = allDataGreen %>%
  dplyr::mutate(`<40`=`<10` + `[10-20)` + `[20-30)` + `[30-40)`,
                `>80`=`[80-90)` + `>90`) %>%
  dplyr::select(`eui_elec`, `eui_gas`, `<40`, `>80`, `private`, `region_wiki`, `Name`) %>%
  dplyr::group_by(`private`, `region_wiki`) %>%
  dplyr::summarise(`eui_elec`=mean(`eui_elec`), `eui_gas`=mean(`eui_gas`), `<40`=mean(`<40`), `>80`=mean(`>80`), `building count`=n_distinct(Name)) %>%
  {.}

df3 = allDataGreen %>%
  dplyr::mutate(`<40`=`<10` + `[10-20)` + `[20-30)` + `[30-40)`,
                `>80`=`[80-90)` + `>90`) %>%
  dplyr::select(`eui_elec`, `eui_gas`, `<40`, `>80`, `private`, `Greeness`, `Name`) %>%
  dplyr::group_by(`private`, `Greeness`) %>%
  dplyr::summarise(`eui_elec`=mean(`eui_elec`), `eui_gas`=mean(`eui_gas`), `<40`=mean(`<40`), `>80`=mean(`>80`), `building count`=n_distinct(Name)) %>%
  {.}

df1 %>%
  dplyr::bind_rows(df2, df3) %>%
  readr::write_csv("slide_summary_eui_weather_cnt.csv")

allDataGreen %>%
  dplyr::distinct(`Name`, `Organization`, `region_wiki`) %>%
  dplyr::group_by(`Organization`, `region_wiki`) %>%
  dplyr::summarise(n())

## comparing static characteristics of the two portfolio
load("../data/allDataGreen.rda")

allDataGreen %>%
  names()

dfstatic = allDataGreen %>%
  dplyr::select(-`latitude`, -`longitude`, -`City`, -`State`, -`year`, -`Organization`, -`Date`) %>%
  dplyr::mutate(Portfolio=ifelse(private==0, "public", "private")) %>%
  {.}

## comparing building types of the two portfolio
num.building.per.type <- dfstatic %>%
  dplyr::distinct(Name, Portfolio, `type_general`) %>%
  dplyr::group_by(Portfolio, `type_general`) %>%
  dplyr::summarise(n()) %>%
  dplyr::ungroup() %>%
  tidyr::spread(`Portfolio`, `n()`, fill=0L) %>%
  dplyr::mutate(ordering = pmax(private, public)) %>%
  dplyr::arrange(desc(ordering)) %>%
  dplyr::select(-ordering) %>%
  dplyr::mutate_at(vars(`type_general`), ~replace(., is.na(.), "Unknown")) %>%
  {.}
result_table_tex <- xtable(num.building.per.type, caption="Building type count")
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file="~/Dropbox/thesis/writeups/policy_cmp/tables/num_building_per_type.tex")

## comparing static info of the two portfolio
s1 =
  dfstatic %>%
  dplyr::group_by(Portfolio) %>%
  dplyr::summarise(GSF=mean(GSF), `Sum of Employees`=mean(`Sum of Employees`)) %>%
  dplyr::ungroup() %>%
  tibble::column_to_rownames("Portfolio") %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable") %>%
  {.}

s2 =
  dfstatic %>%
  dplyr::mutate(Ownership = sprintf("%s percent", Ownership)) %>%
  dplyr::group_by(Portfolio, Ownership) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Portfolio) %>%
  dplyr::mutate(percentage=count/sum(count) * 100) %>%
  dplyr::ungroup() %>%
  dplyr::select(-count) %>%
  tidyr::spread(Portfolio, `percentage`) %>%
  dplyr::rename(variable=Ownership) %>%
  {.}

s3 =
  dfstatic %>%
  dplyr::mutate(`Building Type`=ifelse(`Building Type` %in% c("Office", "Retail"), `Building Type`, "Other")) %>%
  dplyr::group_by(Portfolio, `Building Type`) %>%
  dplyr::summarise(count=n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Portfolio) %>%
  dplyr::mutate(percentage = count/sum(count) * 100) %>%
  dplyr::ungroup() %>%
  dplyr::select(-count) %>%
  tidyr::spread(Portfolio, `percentage`, fill=0) %>%
  dplyr::rename(variable=`Building Type`) %>%
  dplyr::arrange(desc(private)) %>%
  {.}

t.test(dfstatic %>% dplyr::filter(Portfolio == "private") %>% .$GSF,
       dfstatic %>% dplyr::filter(Portfolio == "public") %>% .$GSF)

static.cmp =
  s1 %>%
  bind_rows(s2) %>%
  bind_rows(s3) %>%
  dplyr::mutate(category=c("size", "", "Ownership", "", "", "Type", "", "")) %>%
  dplyr::select(category, everything()) %>%
  {.}
result_table_tex <- xtable(static.cmp, caption="Building static feature comparison")
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file="~/Dropbox/thesis/writeups/policy_cmp/tables/static_feature_cmp.tex")

allData %>%
  dplyr::distinct(Organization, State, Name) %>%
  dplyr::group_by(Organization, State) %>%
  print()

## Plot GSA energy along with the policy change timeline
devtools::load_all("~/Dropbox/gsa_2017/db.interface")

gsa_energy =
  db.interface::read_table_from_db(dbname = "all", tablename = "EUAS_monthly",
                                   cols=c("Building_Number", "state_abbr", "Gross_Sq.Ft", "year", "month",
                                          "Electric_(kBtu)", "Gas_(kBtu)", "Gross_Sq.Ft"
                                          ##, "Occupancy", "datacenter_sqft", "lab_sqft"
                                          )) %>%
  dplyr::rename(`Name`=`Building_Number`) %>%
  ## na.omit() %>%
  {.}

## plot energy
toplot = gsa_energy %>%
  dplyr::rename(`GSF`=`Gross_Sq.Ft`) %>%
  dplyr::filter(`GSF`>0) %>%
  dplyr::mutate(`Energy`=`Electric_(kBtu)` + `Gas_(kBtu)`) %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarise(Energy=sum(Energy), GSF=sum(GSF)) %>%
  dplyr::ungroup() %>%
  ## dplyr::mutate(target=Energy) %>%
  dplyr::mutate(target=Energy/GSF) %>%
  {.}
ylimit = max(toplot$target)
toplot %>%
  dplyr::mutate(Time=zoo::as.yearmon(sprintf("%d-%d", year, month), format="%Y-%m")) %>%
  ggplot2::ggplot(ggplot2::aes(x=Time, y=target)) +
  ggplot2::geom_line() +
  ggplot2::geom_vline(xintercept=as.yearmon("2005-08", format="%Y-%m"), linetype="dashed") +
  geom_point(aes(x=as.yearmon("2005-08", format="%Y-%m"), y=1.0*ylimit), colour="red") +
  ggplot2::annotate("text", x=as.yearmon("2005-08", format="%Y-%m"), y=1.0*ylimit, label="EPAct 2005") +
  ggplot2::geom_vline(xintercept=as.yearmon("2006-01", format="%Y-%m"), linetype="dashed") +
  geom_point(aes(x=as.yearmon("2006-01", format="%Y-%m"), y=0.9*ylimit), colour="red") +
  ggplot2::annotate("text", x=as.yearmon("2006-01", format="%Y-%m"), y=0.9*ylimit, label="Guiding Principles\n(Memorandum in 2006)") +
  ggplot2::geom_vline(xintercept=as.yearmon("2007-01", format="%Y-%m"), linetype="dashed") +
  geom_point(aes(x=as.yearmon("2007-01", format="%Y-%m"), y=0.95*ylimit), colour="red") +
  ggplot2::annotate("text", x=as.yearmon("2007-01", format="%Y-%m"), y=0.95*ylimit, label="Executive Order 13423") +
  ggplot2::geom_vline(xintercept=as.yearmon("2007-12", format="%Y-%m"), linetype="dashed") +
  geom_point(aes(x=as.yearmon("2007-12", format="%Y-%m"), y=1.0*ylimit), colour="red") +
  ggplot2::annotate("text", x=as.yearmon("2007-12", format="%Y-%m"), y=1.0*ylimit, label="EISA 2007") +
  ggplot2::geom_vline(xintercept=as.yearmon("2008-12", format="%Y-%m"), linetype="dashed") +
  geom_point(aes(x=as.yearmon("2008-12", format="%Y-%m"), y=0.95*ylimit), colour="red") +
  ggplot2::annotate("text", x=as.yearmon("2008-12", format="%Y-%m"), y=0.95*ylimit, label="Guiding Principles\n(revision in 2008)") +
  ggplot2::geom_vline(xintercept=as.yearmon("2009-02", format="%Y-%m"), linetype="dashed") +
  geom_point(aes(x=as.yearmon("2009-02", format="%Y-%m"), y=0.9*ylimit), colour="red") +
  ggplot2::annotate("text", x=as.yearmon("2009-02", format="%Y-%m"), y=0.9*ylimit, label="Recovery Act") +
  ggplot2::geom_vline(xintercept=as.yearmon("2009-10", format="%Y-%m"), linetype="dashed") +
  geom_point(aes(x=as.yearmon("2009-10", format="%Y-%m"), y=1.0*ylimit), colour="red") +
  ggplot2::annotate("text", x=as.yearmon("2009-10", format="%Y-%m"), y=1.0*ylimit, label="Executive Order 13514") +
  ggplot2::geom_vline(xintercept=as.yearmon("2011-02", format="%Y-%m"), linetype="dashed") +
  geom_point(aes(x=as.yearmon("2011-02", format="%Y-%m"), y=0.9*ylimit), colour="red") +
  ggplot2::annotate("text", x=as.yearmon("2011-02", format="%Y-%m"), y=0.9*ylimit, label="President Memorandum") +
  ggplot2::geom_vline(xintercept=as.yearmon("2011-06", format="%Y-%m"), linetype="dashed") +
  geom_point(aes(x=as.yearmon("2011-06", format="%Y-%m"), y=0.95*ylimit), colour="red") +
  ggplot2::annotate("text", x=as.yearmon("2011-06", format="%Y-%m"), y=0.95*ylimit, label="Better Buildings Initiative") +
  ggplot2::geom_vline(xintercept=as.yearmon("2015-03", format="%Y-%m"), linetype="dashed") +
  geom_point(aes(x=as.yearmon("2015-03", format="%Y-%m"), y=0.9*ylimit), colour="red") +
  ggplot2::annotate("text", x=as.yearmon("2015-03", format="%Y-%m"), y=0.9*ylimit, label="Excecutive Order 13693") +
  ggplot2::theme_bw() +
  ggplot2::ylab("Electricity + Gas (kBtu/sqft)") +
  ggplot2::ggtitle("The energy consumption trend and policy timelines of the public portfolio") +
  ggplot2::theme()
ggplot2::ggsave(file="~/Dropbox/thesis/code/pubPriCmp/image/gsa_eui_trend_policy_time.png", width=13, height=5, units="in")

## plot GSF
ylimit = max(toplot$GSF)
toplot %>%
  dplyr::mutate(Time=zoo::as.yearmon(sprintf("%d-%d", year, month), format="%Y-%m")) %>%
  ggplot2::ggplot(ggplot2::aes(x=Time, y=GSF)) +
  ggplot2::geom_line() +
  ggplot2::geom_vline(xintercept=as.yearmon("2010-06", format="%Y-%m"), linetype="dashed") +
  ggplot2::annotate("text", x=as.yearmon("2010-06", format="%Y-%m"), y=0.9*ylimit, label="Memorandum June 2010\n(Disposing of Unneeded Federal Real Estate)") +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("The GSF and time line the public portfolio") +
  ggplot2::theme()
