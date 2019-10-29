library("sf")
library("ggplot2")
library("dplyr")
library("scales")
library("RColorBrewer")

## ## ## ## ## ## ## ## ## ## ##
## plot greeness level
## ## ## ## ## ## ## ## ## ## ##
## read in data
greeness_keyword = "greeness2category"
## ## for 3 category use this
## greeness_keyword = "greeness"
df_green = readr::read_csv(sprintf("%s_combine_source_majority_vote.csv", greeness_keyword)) %>%
  tibble::as_tibble() %>%
  {.}

df_green %>%
  distinct(Majority)

df_green_howe_state = readr::read_csv(sprintf("%s_howe_state.csv", greeness_keyword)) %>%
  tibble::as_tibble() %>%
  dplyr::rename(`STUSPS`=`State`, `Rating`=`greeness.howe.state`) %>%
  dplyr::mutate(Source="Howe") %>%
  dplyr::select(-`x65_happening_state`) %>%
  {.}

df_green_long = df_green %>%
  tidyr::gather(`Source`, `Rating`, `Majority`:`Forbes`) %>%
  dplyr::mutate_at(vars(Rating), dplyr::recode, "Morderate Green"="Moderate Green") %>%
  dplyr::rename(`STUSPS`=`State`) %>%
  dplyr::bind_rows(df_green_howe_state) %>%
  na.omit() %>%
  {.}

state_shape = sf::st_read("geo_data/cb_2017_us_state_20m.shp")

summary(state_shape)

map_data =
  state_shape %>%
  dplyr::right_join(df_green_long, by="STUSPS") %>%
  {.}

## plot individual source vs majority votes
p <- map_data %>%
  dplyr::filter(!`STUSPS` %in% c("AK", "HI")) %>%
  dplyr::filter(Source != "Howe") %>%
  dplyr::mutate(`Source`=factor(`Source`, levels = c("NCSL", "Forbes", "WalletHub", "Majority"))) %>%
  ggplot() +
  geom_sf(aes(fill = Rating))
if (greeness_keyword == "greeness") {
  p <- p +
    ggplot2::scale_fill_brewer(palette="Greens")
} else if (greeness_keyword == "greeness2category") {
  p <- p +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Greens")[c(2, 3)], name="State Greeness Rating")
}
p <- p +
  ggplot2::theme_bw() +
  ggplot2::theme(text = element_text(size=10), legend.position = "bottom") +
  ggplot2::facet_wrap(~Source, ncol=2)
print(p)
ggplot2::ggsave(file=sprintf("../image/%s_plot.png", greeness_keyword), width=6, height=4, units="in")

## plot majority vs howe
p <-
  map_data %>%
  dplyr::filter(!`STUSPS` %in% c("AK", "HI")) %>%
  dplyr::filter(Source %in% c("Howe", "Majority")) %>%
  dplyr::mutate(`Source`=factor(`Source`, levels = c("Majority", "Howe"))) %>%
  ggplot() +
  geom_sf(aes(fill = Rating))
if (greeness_keyword == "greeness") {
  p <- p +
    ggplot2::scale_fill_brewer(palette="Greens")
} else if (greeness_keyword == "greeness2category") {
  p <- p +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Greens")[c(2, 3)], name="State Greeness Rating")
}
p <- p +
  ggplot2::theme_bw() +
  ggplot2::theme(text = element_text(size=10), legend.position = "bottom") +
  ggplot2::facet_wrap(~Source, ncol=2)
print(p)
ggplot2::ggsave(file=sprintf("../image/%s_plot_majority_howe.png", greeness_keyword), width=6, height=4, units="in")

## show hex for palette
## RColorBrewer::brewer.pal(n=3, name="Greens")

## ## ## ## ## ## ## ## ## ## ##
## plot average state temperature
## ## ## ## ## ## ## ## ## ## ##

df_state_abbr =
  readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/state_abbr.csv") %>%
  {.}

method = "stateAvgTemp"

## method = "stateLat"
if (method == "stateAvgTemp") {
  load("~/Dropbox/thesis/code/pubPriCmp/data/warmthClass.rda")
} else if (method == "stateLat") {
  load("~/Dropbox/thesis/code/pubPriCmp/data/warmthClass_lat.rda")
  warmthClass <- warmthClass_lat
}

head(warmthClass)

state_shape = sf::st_read("geo_data/cb_2017_us_state_20m.shp")

summary(state_shape)

if (method == "stateAvgTemp") {
  df_state_temp =
    warmthClass %>%
    dplyr::rename(`STUSPS`=`State`,
                  `Average Temperature`=`average_temperature`) %>%
    {.}
  summary(df_state_temp)
  map_data =
    state_shape %>%
    dplyr::right_join(df_state_temp, by="STUSPS") %>%
    {.}
  map_data %>%
    dplyr::filter(!`STUSPS` %in% c("AK", "HI")) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = `Average Temperature`)) +
    ggplot2::scale_fill_gradient2(midpoint = median(df_state_temp$`Average Temperature`),
                                  low="deepskyblue3", high="orangered3") +
    ggplot2::theme(text = ggplot2::element_text(size=10),
                   legend.position = "bottom")
  ggplot2::ggsave(file="../image/state_average_temperature.png", width=6, height=4, units="in")
} else if (method == "stateLat") {
  warmthClass <- warmthClass %>%
    dplyr::rename(`STUSPS`=`State`) %>%
    {.}
  map_data =
    state_shape %>%
    dplyr::right_join(warmthClass, by="STUSPS") %>%
    {.}
}

## ## ## ## ## ## ## ## ## ## ##
## plot state warmth level
## ## ## ## ## ## ## ## ## ## ##

boundary_low = 36.9
boundary_high = 42
p <- map_data %>%
  dplyr::filter(!`STUSPS` %in% c("AK", "HI")) %>%
  dplyr::mutate(`warmth`=factor(`warmth`, levels=c("hot", "mild", "cold"))) %>%
  ## dplyr::mutate(`warmth`=factor(`warmth`, levels=c("cold", "mild", "hot"))) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = `warmth`))
if (method == "stateLat") {
  p <- p +
  ## ggplot2::scale_fill_manual(values=rev(colorRampPalette(c("#FF0000","#000066"))(3L))) +
  ggplot2::geom_segment(ggplot2::aes(x=-127, xend=-68, y=boundary_low, yend=boundary_low), size=0.5,
                        linetype="dashed") +
  ggplot2::geom_segment(ggplot2::aes(x=-127, xend=-68, y=boundary_high, yend=boundary_high), size=0.5,
                        linetype="dashed")
}
p <- p +
  ggplot2::scale_fill_brewer(palette = "RdBu") +
  ggplot2::theme(text = ggplot2::element_text(size=10),
                 legend.position = "bottom")
print(p)
ggplot2::ggsave(file=sprintf("../image/warmth_class_%s.png", method), width=6, height=4, units="in")

## ## ## ## ## ## ## ## ## ## ##
## plot individual building's average temperature
## ## ## ## ## ## ## ## ## ## ##

load("../data/retrofit_allData.rda")

buildinglatlng = retrofit_allData %>%
  dplyr::distinct(`Name`, `latitude`, `longitude`)

buildingTemp =
  readr::read_csv("building_mean_temperature_05to17.csv") %>%
  tibble::as_tibble() %>%
  dplyr::left_join(buildinglatlng, by="Name") %>%
  dplyr::rename(`average monthly temperature`=`mean_avgminmax`) %>%
  {.}

state_shape %>%
  dplyr::filter(!`STUSPS` %in% c("AK", "HI", "VI", "GU", "PR", "VI")) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill="grey") +
  ggplot2::geom_point(ggplot2::aes(y=`latitude`, x=`longitude`, color=`average monthly temperature`),
                      data=buildingTemp) +
  ggplot2::scale_color_gradient2(midpoint = median(buildingTemp$`average monthly temperature`),
                                low="deepskyblue3", high="orangered3") +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(file="../image/building_avgtemp_05to17.png", width=6, height=4, units="in")

## ## ## ## ## ## ## ## ## ## ##
## plot individual building's warmth class
## ## ## ## ## ## ## ## ## ## ##

load("../data/retrofit_allData.rda")

buildingWarmth <-
  retrofit_allData %>%
  dplyr::distinct(`Name`, `latitude`, `longitude`, `warmth`, `warmth_stateLat`, `warmth_buildingTemp`) %>%
  dplyr::mutate(`warmth_buildingTemp`=factor(`warmth_buildingTemp`, levels=c("hot", "mild", "cold"))) %>%
  dplyr::mutate(`warmth_stateLat`=factor(`warmth_stateLat`, levels=c("hot", "mild", "cold"))) %>%
  dplyr::mutate(`warmth`=factor(`warmth`, levels=c("hot", "mild", "cold"))) %>%
  {.}

## warmth_col = "warmth_buildingTemp"
## file_suffix = "_buildingTemp"
## legend_title = "warmth level by building temperature"
## warmth_col = "warmth_stateLat"
## file_suffix = "_stateLat"
## legend_title = "warmth level by state latitude"
warmth_col = "warmth"
file_suffix = "_stateTemp"
legend_title = "warmth level by state temperature"
state_shape %>%
  dplyr::filter(!`STUSPS` %in% c("AK", "HI", "VI", "GU", "PR", "VI")) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(fill="grey") +
  ggplot2::geom_point(ggplot2::aes(y=`latitude`, x=`longitude`, color=!!rlang::sym(warmth_col)), data=buildingWarmth) +
  ggplot2::scale_color_brewer(palette = "RdBu", name=legend_title) +
  ## guides(fill=guide_legend(title="New Legend Title")) +
  ## ggplot2::scale_color_discrete(name = "New Legend Title") +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(file=sprintf("../image/building_warmth_class%s.png", file_suffix),
                width=6, height=4, units="in")

## ## ## ## ## ## ## ## ## ## ##
## plot US region north vs south
## ## ## ## ## ## ## ## ## ## ##

df_state_abbr =
  readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/state_abbr.csv") %>%
  {.}

region_name = "USregionWiki"
## region_name = "USregion"
if (region_name == "USregion") {
  load("../data/USregion.rda")
  regionfile = USregion
  region_col = "USregion"
} else if (region_name == "USregionWiki") {
  load("../data/USregionWiki.rda")
  regionfile = USregionWiki
  region_col = "region_wiki"
}

load("../data/allData.rda")
allData %>%
  head()

state_shape = sf::st_read("geo_data/cb_2017_us_state_20m.shp")

summary(state_shape)

map_data = regionfile %>%
  ## map_data = USregion %>%
  dplyr::rename(`STUSPS`=`State`) %>%
  dplyr::left_join(state_shape, by="STUSPS") %>%
  {.}

head(map_data)

boundary_low = 36.9
p <- map_data %>%
  dplyr::filter(!`STUSPS` %in% c("AK", "HI")) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = !!rlang::sym(region_col))) +
  ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(3, "RdBu")[c(3, 1)], name="US Region") +
  ggplot2::theme(text = ggplot2::element_text(size=10), legend.position = "bottom")
if (region_name == "USregion") {
  p <- p +
    ggplot2::geom_segment(ggplot2::aes(x=-127, xend=-68, y=boundary_low, yend=boundary_low), size=0.5,
                          linetype="dashed")
}
p <- p +
  ## ggplot2::geom_point(ggplot2::aes(y=`latitude`, x=`longitude`, color=`USRegion`), data=allData) +
  ## ggplot2::xlim(c(-100, NA)) +
  ggplot2::ylab(NULL) +
  ggplot2::xlab(NULL)
print(p)
ggplot2::ggsave(file=sprintf("../image/%s.png", region_name), width=6, height=4, units="in")

## ## ## ## ## ## ## ## ## ## ##
## plot individual buildings in the north and south
## ## ## ## ## ## ## ## ## ## ##

buildingLoc = allData %>%
  dplyr::distinct(`latitude`, `longitude`, !!rlang::sym(region_col), `Organization`) %>%
  dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
  dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
  {.}
head(buildingLoc)

boundary_low = 36.9
p <- buildingLoc %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(y=`latitude`, x=`longitude`, color=!!rlang::sym(region_col)),
                      size=0.5) +
  ggplot2::xlim(c(-95, NA)) +
  ggplot2::geom_sf(data=map_data[!(map_data$STUSPS) %in% c("AK", "HI"),], fill=NA)
if (region_name == "USregion") {
  p <- p +
    ggplot2::geom_segment(ggplot2::aes(x=-95, xend=-68, y=boundary_low, yend=boundary_low), size=0.5,
                          linetype="dashed")
}
p <- p +
  ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(3, "RdBu")[c(3, 1)], name="US Region") +
  ggplot2::facet_wrap(.~Organization) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")
print(p)
ggplot2::ggsave(file=sprintf("../image/building%s_facet.png", region_name), width=8, height=4, units="in")

allData %>%
  dplyr::mutate(`region_wiki`=ifelse(`State` %in% southern_states, "South", "North")) %>%
  ## dplyr::distinct(`Organization`, `Name`, `USRegion`) %>%
  dplyr::distinct(`Organization`, `Name`, `region_wiki`) %>%
  dplyr::group_by(`region_wiki`, `Organization`) %>%
  dplyr::summarise(n())

allData %>%
  dplyr::distinct(`Organization`, `Name`) %>%
  dplyr::group_by(`Organization`) %>%
  dplyr::summarise(n())

## building count by state
allData %>%
  dplyr::distinct(`State`, `Organization`, `Name`) %>%
  dplyr::group_by(`Organization`, `State`) %>%
  dplyr::summarise(`building count`=n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
  dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
  dplyr::rename(`STUSPS`=`State`) %>%
  dplyr::left_join(state_shape, by="STUSPS") %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=`building count`)) +
  ggplot2::facet_wrap(.~`Organization`) +
  scale_fill_gradient(low = "white", high = "black") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(file="../image/building_count_by_state_map.png", width=8, height=4, units="in")

southern_states =
  c("TX", "OK", "AR", "LA", "MS", "TN", "AL", "GA", "SC", "NC", "FL", "KY", "WV", "VA", "MD", "DE")

map_data %>%
  dplyr::filter(!`STUSPS` %in% c("AK", "HI")) %>%
  dplyr::mutate(`region_wiki`=ifelse(`STUSPS` %in% southern_states, "South", "North")) %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=`region_wiki`))

## ## ## ## ## ## ## ## ## ## ##
## plot individual buildings two greeness levels
## ## ## ## ## ## ## ## ## ## ##

getwd()

load("../data/allDataGreen.rda")

unique(state_shape$STUSPS)

cols = c("Greeness", "greeness.howe.state", "greeness.howe.county")
captions = c("Greeness from Majority Vote of 3 Sources",
             "Greeness from State Level Data of Howe 2015",
             "Greeness from County Level Data of Howe 2015")

for (i in seq_along(cols)) {
  greenessCol = cols[i]
  suf = gsub(".", "_", cols[i], fixed=TRUE)
  allDataGreen %>%
    distinct(`Organization`, !!rlang::sym(greenessCol), `latitude`, `longitude`) %>%
    dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
    dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(y=`latitude`, x=`longitude`, color=!!rlang::sym(greenessCol)), size=0.5) +
    ggplot2::geom_sf(data=state_shape[!(state_shape$STUSPS) %in% c("AK", "HI", "PR"),], fill=NA) +
    ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(3, "Greens")[c(2, 3)], name="Greeness Rating") +
    ggplot2::facet_wrap(.~Organization) +
    ggplot2::theme_bw() +
    ggplot2::xlim(c(-95, NA)) +
    ggplot2::ggtitle(captions[i]) +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(file=sprintf("../image/buildingGreeness2category_facet_%s.png", suf),
                  width=8, height=4, units="in")
  writefile <- file(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/buildingGreeness2category_facet_%s.tex", suf), "w+")
  sink(writefile)
  allDataGreen %>%
    distinct(`Organization`, !!rlang::sym(greenessCol), Name) %>%
    dplyr::group_by(Organization, !!rlang::sym(greenessCol)) %>%
    dplyr::summarise(count=n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(!!rlang::sym(greenessCol), count) %>%
    knitr::kable("latex", booktabs = T) %>%
    print()
  sink()
  close(writefile)
}

allDataGreen %>%
  filter(is.na(greeness.howe.state)) %>%
  head()

## ## ## ## ## ## ## ## ## ## ##
## plot individual buildings two portfolio
## ## ## ## ## ## ## ## ## ## ##

load("../data/allDataGreen.rda")

unique(state_shape$STUSPS)

meridian=-100
allDataGreen %>%
  distinct(`Organization`, `latitude`, `longitude`) %>%
  dplyr::mutate(`Organization`=recode(`Organization`, "GSA"="public", "PNC"="private")) %>%
  dplyr::mutate(`Organization`=factor(`Organization`, levels=c("public", "private"))) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(y=`latitude`, x=`longitude`, color=`Organization`), size=0.5) +
  ggplot2::geom_sf(data=state_shape[!(state_shape$STUSPS) %in% c("AK", "HI", "PR"),], fill=NA) +
  ## ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(3, "Set2")[c(3, 1)], name="State Greeness Rating") +
  ggplot2::geom_segment(ggplot2::aes(x=meridian, xend=meridian, y=25, yend=50), size=0.5,
                        linetype="dashed") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(file="../image/buildingByOrganization.png", width=8, height=5, units="in")
