## This file creates allDataGreen.rda with a two category "Greeness" column, use greeness.R for 3 category
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(xtable)
library(nnet)

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

allDataGreen =
  allData %>%
  left_join(df_green, by="State") %>%
  {.}

allDataGreen %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data/allDataGreen.csv")

devtools::use_data(pkg="~/Dropbox/thesis/code/pubPriCmp", allDataGreen, overwrite = TRUE)
