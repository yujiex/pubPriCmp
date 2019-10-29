## This file creates allDataGreen.rda with a two category "Greeness" column, use greeness.R for 3 category
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(xtable)
library(nnet)
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
                    "voluntary renewable energy standard or target" = "Least Green",
                    "no standard or target" = "Least Green") %>%
  dplyr::select(`State`, `Greeness`) %>%
  dplyr::mutate(`Source`="NCSL") %>%
  {.}

head(df1)
df1 %>%
  dplyr::group_by(`Greeness`) %>%
  dplyr::summarise(n())

df2 =
  readr::read_csv("wallethubGreenessScore.csv") %>%
  as_data_frame() %>%
  dplyr::mutate(`Greeness` = cut(`Overall Rank`, breaks = c(0, 25, 50))) %>%
  dplyr::mutate_at(vars(`Greeness`), recode, `(0,25]`="Most Green",
                        `(25,50]`="Least Green",
                        ) %>%
  dplyr::mutate(`State`=toupper(`State`)) %>%
  inner_join(df_state, by="State") %>%
  dplyr::select(`state_abbr`, `Greeness`) %>%
  dplyr::rename(`State`=`state_abbr`) %>%
  dplyr::mutate(`Source`="WalletHub") %>%
  {.}

head(df2)
df2 %>%
  dplyr::group_by(`Greeness`) %>%
  dplyr::summarise(n())

df3 =
  readr::read_csv("forbesGreenessScore.csv") %>%
  as_data_frame() %>%
  dplyr::mutate(`Greeness` = cut(`Rank`, breaks = c(0, 25, 50))) %>%
  dplyr::mutate_at(vars(`Greeness`), recode, `(0,25]`="Most Green",
                        `(25,50]`="Least Green",
                        ) %>%
  dplyr::mutate(`State`=toupper(`State`)) %>%
  inner_join(df_state, by="State") %>%
  dplyr::select(`state_abbr`, `Greeness`) %>%
  dplyr::rename(`State`=`state_abbr`) %>%
  dplyr::mutate(`Source`="Forbes") %>%
  {.}

head(df3)
df3 %>%
  dplyr::group_by(`Greeness`) %>%
  dplyr::summarise(n())

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

df_green_majority =
  df %>%
  tidyr::spread(`Source`, `Greeness`) %>%
  dplyr::select(`State`, `NCSL`, `WalletHub`, `Forbes`) %>%
  left_join(majority) %>%
  dplyr::arrange(desc(Majority), desc(`NCSL`), desc(`WalletHub`),
                  desc(`Forbes`), State) %>%
  dplyr::select(c(1, 5, 2:4)) %>%
  {.}

df_green_majority %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/greeness2category_combine_source_majority_vote.csv")

df_green =
  df_green_majority %>%
  dplyr::rename(`Greeness`=`Majority`) %>%
  dplyr::select(State, Greeness) %>%
  {.}

df_green %>%
  readr::write_csv("greeness2category.csv")

## Fixme: try this
## Another way to combine the three ratings: convert them all to continuous:
## transform each of them to Gaussian Corpula: phi_{Gaussian}(inverse of
## empirical CDF(x))

load("~/Dropbox/thesis/code/pubPriCmp/data/allData.rda")

df_green_howe_state <-
  readxl::read_excel("Howe_et_al_NatClimCh_2015_SI2.xls") %>%
  dplyr::select(`State_abb`, `x65_happening`) %>%
  dplyr::rename(`State`=`State_abb`) %>%
  dplyr::arrange(desc(`x65_happening`)) %>%
  dplyr::mutate(Rank=as.numeric(rownames(.))) %>%
  dplyr::mutate(`greeness.howe.state` = cut(`Rank`, breaks = c(0, 25, 51), labels=c("Most Green", "Least Green"))) %>%
  dplyr::select(-Rank) %>%
  dplyr::rename(`x65_happening_state`=`x65_happening`) %>%
  {.}

df_green_howe_state %>%
  readr::write_csv("greeness2category_howe_state.csv")

df_green_howe_county <- readxl::read_excel("Howe_et_al_NatClimCh_2015_SI3.xls") %>%
  dplyr::select(`County_FIPS`, `County_name`, `x65_happening`) %>%
  dplyr::mutate(`County_FIPS`=sprintf("%05d", `County_FIPS`)) %>%
  dplyr::arrange(desc(`x65_happening`)) %>%
  ## add back in DC data
  dplyr::bind_rows(data.frame(`County_FIPS`="11001", `County_name`="Washington, District of Columbia",
                              `x65_happening`=81.4)) %>%
  dplyr::mutate(Rank=as.numeric(rownames(.))) %>%
  dplyr::mutate(`greeness.howe.county` = cut(`Rank`, breaks = c(0, floor(nrow(.)/2), nrow(.)), labels=c("Most Green", "Least Green"))) %>%
  dplyr::select(-Rank) %>%
  dplyr::rename(`x65_happening_county`=`x65_happening`) %>%
  {.}

df_green_howe_county %>%
  readr::write_csv("greeness2category_howe_county.csv")

load("../data/latlon2county.nodup.rda")

latlon2county.nodup %>%
  head()

allDataGreen = allData %>>%
  left_join(df_green, by="State") %>>%
  dplyr::left_join(df_green_howe_state, by="State") %>>%
  (?nrow(.)) %>>%
  dplyr::left_join(latlon2county.nodup, by=c("latitude", "longitude")) %>>%
  (?nrow(.)) %>>%
  ## 5 buildings eliminated from this: AR0057ZZ, DeerfieldNorthbrook,
  ## EightMileRexOffice, RawsonvilleandI94Office, NorthbyNortheast
  dplyr::filter(!is.na(`county_fips`)) %>>%
  ## 3 buildings eliminated from this: WA0000KA, Glenridge, ChandlerCommons
  dplyr::filter(State == `state_code`) %>>%
  dplyr::rename(`County_FIPS`=`county_fips`) %>>%
  dplyr::select(-`state_code`, -`county_name`) %>>%
  dplyr::left_join(df_green_howe_county, by=c("County_FIPS")) %>>%
  {.}

allDataGreen %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data/allDataGreen.csv")

usethis::use_data(allDataGreen, internal = FALSE, overwrite = TRUE)
