## Following steps from
##https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/#clear-workspace-and-install-necessary-packages

detachAllPackages <- function() {
  basic.packages.blank <-  c("stats",
                             "graphics",
                             "grDevices",
                             "utils",
                             "datasets",
                             "methods",
                             "base")
  basic.packages <- paste("package:", basic.packages.blank, sep = "")

  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1,
                                  TRUE,
                                  FALSE)]

  package.list <- setdiff(package.list, basic.packages)

  if (length(package.list) > 0)  for (package in package.list) {
    detach(package, character.only = TRUE)
    print(paste("package ", package, " detached", sep = ""))
  }
}

detachAllPackages()

if (!require(rgeos)) {
  install.packages("rgeos", repos = "http://cran.us.r-project.org")
  require(rgeos)
}
if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(rgdal)
}
if (!require(raster)) {
  install.packages("raster", repos = "http://cran.us.r-project.org")
  require(raster)
}
if(!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cloud.r-project.org")
  require(ggplot2)
}
if(!require(viridis)) {
  install.packages("viridis", repos="http://cloud.r-project.org")
  require(viridis)
}
if(!require(dplyr)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org/")
  require(dplyr)
}
if(!require(gtable)) {
  install.packages("gtable", repos = "https://cloud.r-project.org/")
  require(gtable)
}
if(!require(grid)) {
  install.packages("grid", repos = "https://cloud.r-project.org/")
  require(grid)
}
if(!require(readxl)) {
  install.packages("readxl", repos = "https://cloud.r-project.org/")
  require(readxl)
}
if(!require(magrittr)) {
  install.packages("magrittr", repos = "https://cloud.r-project.org/")
  require(magrittr)
}
if(!require(tidyr)) {
  install.packages("tidyr", repos = "https://cloud.r-project.org/")
  require(tidyr)
}

if(!require(maptools)) {
  install.packages("maptools", repos = "https://cloud.r-project.org/")
  require(maptools)
}

## Generic theme
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
                                        # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

## read in data
df_green = readr::read_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/greeness_combine_source.csv") %>%
  tibble::as_data_frame() %>%
  {.}

df_green_long = df_green %>%
  tidyr::gather(`Source`, `Rating`, `Majority`:`Forbes`) %>%
  {.}

## read in geo-data
gde_state <-
  rgdal::readOGR("~/Dropbox/thesis/code/pubPriCmp/data-raw/geo_data/cb_2017_us_state_20m.shp",
                 layer="cb_2017_us_state_20m")

## projection settings
crs(gde_state) <- "+proj=somerc +lat_0=46.95240555555556
+lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000
+ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"

## turn shapefile in to dataframe
map_state_fortified = ggplot2::fortify(gde_state, region="NAME")

## Cannot tell which state is which
head(map_state_fortified)

## takes long, not finishing
map_state_fortified %>%
  ggplot2::ggplot() +
  ggplot2::geom_polygon(ggplot2::aes(x=lat, y=long, group=group))
