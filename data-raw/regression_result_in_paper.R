library("dplyr")
library("xtable")

## generate regression result summary table for pub pri cmp using hdd and cdd
## input data frame is standard output from outreg2 with 4 variables coef, se, ci_low, and ci_high
## output a data frame with each cell being coef---se
combine_to_one_row <- function(filename, fields_to_remove, outreg_vars, vars_to_keep) {
  df =
    readr::read_tsv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/%s", filename), skip=7, col_names=FALSE) %>%
    head(-5) %>%
    dplyr::mutate(`variable`=rep(outreg_vars, nrow(.)/4)) %>%
    tidyr::fill(`X1`) %>%
    dplyr::filter(`variable` %in% vars_to_keep) %>%
    {.}
  for (field in fields_to_remove) {
    df <- df %>%
      dplyr::filter(!grepl(field, `X1`, fixed = TRUE)) %>%
      {.}
  }
  df <- df %>%
    dplyr::group_by(`X1`) %>%
    dplyr::summarise_at(vars(starts_with("X")), function(x) return(sprintf("%s---%s", first(x), last(x)))) %>%
    dplyr::ungroup() %>%
    {.}
  return(df)
}

## transpose a data frame using the first column as the new header
transpose_df_header_1st_col <- function(df, colname) {
  transposed = setNames(data.frame(t(df[,-1])), df[,1][, colname]) %>%
    {.}
  return(transposed)
}

## generate regression result summary table for pub pri cmp temperature bin
transpose_result_bin <- function(filename, fields_to_remove, fields_to_keep,
                                 outreg_vars, vars_to_keep, model_col, status_col) {
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
  df <- df %>%
    dplyr::mutate(`X1`=gsub("1.private#c.", "", `X1`)) %>%
    dplyr::group_by(`X1`) %>%
    dplyr::summarise_at(vars(starts_with("X")), function(x) return(sprintf("%s---%s", first(x), last(x)))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(`X1`=gsub("1.private#c.", "", `X1`)) %>%
    {.}
  transposed_df = setNames(data.frame(t(df[,-1])), df[,1]$X1) %>%
    {.}
  splited_df =
    tidyr::separate_rows(transposed_df,`avgTemp10to20`, `avgTemp20to30`, `avgTemp30to40`, `avgTemp40to50`,
                        `avgTemp50to60`, `avgTemp70to80`, `avgTemp80to90`, `avgTempAbove90`,
                        `avgTempBelow10`, sep="---") %>%
    dplyr::rename_all(function(x) {sprintf("%sF", gsub("avgTemp", "", x))}) %>%
    {.}
  return(splited_df)
}

setwd("~/Dropbox/thesis/writeups/policy_cmp/tables")
filename = "regression_results_bin_0720.txt"
fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "0b.private", "1.private")
fields_to_keep = NULL
outreg_vars = c("coef", "se", "ci_low", "ci_high")
vars_to_keep = c("coef", "se")
model_col = c("baseline", "", "state specific trend", "", "state trend x private", "")
status_col = c("public", rep("", 5))
splited_public = transpose_result_bin(filename=filename, fields_to_remove=fields_to_remove,
                                 fields_to_keep=fields_to_keep, outreg_vars=outreg_vars,
                                 vars_to_keep=vars_to_keep, model_col=model_col, status_col=status_col) %>%
  dplyr::mutate(`model`=model_col) %>%
  dplyr::mutate(`status`=status_col) %>%
  {.}
head(splited_public)

filename = "regression_results_bin_0720.txt"
fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "0b.private")
fields_to_keep = c("1.private")
outreg_vars = c("coef", "se", "ci_low", "ci_high")
vars_to_keep = c("coef", "se")
model_col = c("baseline", "", "state specific trend", "", "state trend x private", "")
status_col = c("private", rep("", 5))
splited_private = transpose_result_bin(filename=filename, fields_to_remove=fields_to_remove,
                                      fields_to_keep=fields_to_keep, outreg_vars=outreg_vars,
                                      vars_to_keep=vars_to_keep, model_col=model_col, status_col=status_col) %>%
dplyr::mutate(`model`=model_col) %>%
  dplyr::mutate(`status`=status_col) %>%
    {.}
head(splited_private)

filename = "regression_results_bin_0720_gsa.txt"
fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "0b.private")
fields_to_keep = NULL
outreg_vars = c("coef", "se", "ci_low", "ci_high")
vars_to_keep = c("coef", "se")
model_col = c("baseline", "", "state specific trend", "")
status_col = c("public separate model", rep("", 3))
splited_sep_public = transpose_result_bin(filename=filename, fields_to_remove=fields_to_remove,
                                         fields_to_keep=fields_to_keep, outreg_vars=outreg_vars,
                                         vars_to_keep=vars_to_keep, model_col=model_col, status_col=status_col) %>%
dplyr::mutate(`model`=model_col) %>%
  dplyr::mutate(`status`=status_col) %>%
    {.}
head(splited_sep_public)

filename = "regression_results_bin_0720_pnc.txt"
fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "0b.private")
fields_to_keep = NULL
outreg_vars = c("coef", "se", "ci_low", "ci_high")
vars_to_keep = c("coef", "se")
model_col = c("baseline", "", "state specific trend", "")
status_col = c("private separate model", rep("", 3))
splited_sep_private = transpose_result_bin(filename=filename, fields_to_remove=fields_to_remove,
                                          fields_to_keep=fields_to_keep, outreg_vars=outreg_vars,
                                          vars_to_keep=vars_to_keep, model_col=model_col, status_col=status_col) %>%
dplyr::mutate(`model`=model_col) %>%
  dplyr::mutate(`status`=status_col) %>%
    {.}
head(splited_sep_private)

summary_pub_pri =
  splited_public %>%
  dplyr::bind_rows(splited_private) %>%
  dplyr::bind_rows(splited_sep_public) %>%
  dplyr::bind_rows(splited_sep_private) %>%
  dplyr::select(status, model, Below10F, ends_with("F")) %>%
  {.}
summary_pub_pri %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_pub_pri_cmp.csv")

result_table_tex <- xtable(summary_pub_pri, caption=sprintf("Regression result: comparing public vs private response to weather shock"))
print(result_table_tex, tabular.environment = "longtable", include.rownames=FALSE,
      file="~/Dropbox/thesis/writeups/policy_cmp/tables/summary_pub_pri_cmp.tex", size="\\footnotesize")

transposed_sep_private = setNames(data.frame(t(df_sep_private[,-1])), df_sep_private[,1]$X1) %>%
  ## dplyr::mutate(`status`="private") %>%
  {.}

## generate regression result summary table for greeness with each column being a model
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

## generate regression result summary table for greeness with each column being a temperature bin
format_reg_result_greeness <- function(filename, status_code) {
  df =
    readr::read_tsv(filename, skip=7, col_names=FALSE) %>%
    head(-5) %>%
    dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
      tidyr::fill(`X1`) %>%
      dplyr::filter(!(`variable` %in% c("ci_low", "ci_high")),
                    !grepl("eDate", `X1`, fixed = TRUE),
                    !grepl("eState", `X1`, fixed = TRUE),
                    !grepl("1b.eGreeness", `X1`, fixed = TRUE),
                    !grepl("Constant", `X1`, fixed = TRUE),
                    !grepl("hasretrofitbeforethismonth", `X1`, fixed = TRUE),
                    ) %>%
      dplyr::group_by(`X1`) %>%
      dplyr::summarise_at(vars(X2, X3), function(x) return(sprintf("%s---%s", first(x), last(x)))) %>%
      dplyr::ungroup() %>%
      {.}
  df_least = df %>%
    dplyr::filter(!grepl(".eGreeness", X1, fixed=TRUE)) %>%
    {.}
  transposed_least = setNames(data.frame(t(df_least[,-1])), df_least[,1]$X1) %>%
    {.}
  splited_least =
    tidyr::separate_rows(transposed_least,`avgTemp10to20`, `avgTemp20to30`, `avgTemp30to40`, `avgTemp40to50`,
                      `avgTemp50to60`, `avgTemp70to80`, `avgTemp80to90`, `avgTempAbove90`,
                      `avgTempBelow10`, sep="---") %>%
    dplyr::rename_all(function(x) {sprintf("%sF", gsub("avgTemp", "", x))}) %>%
    dplyr::mutate(`model`=c("baseline", "", "state specific trend", "")) %>%
    dplyr::mutate(`Greeness`=c("Least", rep("", 3))) %>%
    {.}
  head(splited_least)
  df_moderate = df %>%
    dplyr::filter(grepl("2.eGreeness", X1, fixed=TRUE)) %>%
    dplyr::mutate(`X1`=gsub("2.eGreeness#c.", "", `X1`)) %>%
    {.}
  transposed_moderate = setNames(data.frame(t(df_moderate[,-1])), df_moderate[,1]$X1) %>%
    {.}
  splited_moderate =
    tidyr::separate_rows(transposed_moderate,`avgTemp10to20`, `avgTemp20to30`, `avgTemp30to40`, `avgTemp40to50`,
                        `avgTemp50to60`, `avgTemp70to80`, `avgTemp80to90`, `avgTempAbove90`,
                        `avgTempBelow10`, sep="---") %>%
    dplyr::rename_all(function(x) {sprintf("%sF", gsub("avgTemp", "", x))}) %>%
    dplyr::mutate(`model`=c("baseline", "", "state specific trend", "")) %>%
    dplyr::mutate(`Greeness`=c("Moderate", rep("", 3))) %>%
    {.}
  head(splited_moderate)
  df_most = df %>%
    dplyr::filter(grepl("3.eGreeness", X1, fixed=TRUE)) %>%
    dplyr::mutate(`X1`=gsub("3.eGreeness#c.", "", `X1`)) %>%
    {.}
  transposed_most = setNames(data.frame(t(df_most[,-1])), df_most[,1]$X1) %>%
    {.}
  splited_most =
    tidyr::separate_rows(transposed_most,`avgTemp10to20`, `avgTemp20to30`, `avgTemp30to40`, `avgTemp40to50`,
                        `avgTemp50to60`, `avgTemp70to80`, `avgTemp80to90`, `avgTempAbove90`,
                        `avgTempBelow10`, sep="---") %>%
    dplyr::rename_all(function(x) {sprintf("%sF", gsub("avgTemp", "", x))}) %>%
    dplyr::mutate(`model`=c("baseline", "", "state specific trend", "")) %>%
    dplyr::mutate(`Greeness`=c("Most", rep("", 3))) %>%
    {.}
  head(splited_most)
  summary_greeness =
    splited_least %>%
    dplyr::bind_rows(splited_moderate) %>%
    dplyr::bind_rows(splited_most) %>%
    dplyr::mutate(`status`=c(sprintf("%s separate model", status_code), rep("", 11))) %>%
    dplyr::select(status, Greeness, model, Below10F, ends_with("F")) %>%
    {.}
  return(summary_greeness)
}

filename = "~/Dropbox/thesis/writeups/policy_cmp/tables/regression_results_bin_greeness_0720_gsa.txt"
summary_public_green = format_reg_result_greeness(filename=filename, status_code="public")
filename = "~/Dropbox/thesis/writeups/policy_cmp/tables/regression_results_bin_greeness_0720_pnc.txt"
summary_private_green = format_reg_result_greeness(filename=filename, status_code="private")

summary_greeness = summary_public_green %>%
  dplyr::bind_rows(summary_private_green) %>%
  {.}

summary_greeness %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_greeness_wide.csv")

energy_type = "_gas"
setwd("~/Dropbox/thesis/writeups/policy_cmp/tables")
filename = sprintf("regression_greeness_baseline%s.txt", energy_type)
fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "0b.private", "1.private")
fields_to_keep = NULL
outreg_vars = c("coef", "se", "ci_low", "ci_high")
vars_to_keep = c("coef", "se")
## model_col = c("baseline", rep("", 11))
## status_col = c("public", rep("", 5))
## model_col = "x"
## status_col = "x"
splited_baseline = transpose_result_bin(filename=filename, fields_to_remove=fields_to_remove,
                                        fields_to_keep=fields_to_keep, outreg_vars=outreg_vars,
                                        vars_to_keep=vars_to_keep, model_col=model_col, status_col=status_col) %>%
  dplyr::mutate(model=c("baseline", rep("", 11)), status=c("public", rep("", 5), "private", rep("", 5)),
                Rating=rep(c("Least Green", "", "Moderate Green", "", "Most Green", ""), 2)) %>%
  {.}

filename = "regression_greeness_statetrend.txt"
splited_statetrend = transpose_result_bin(filename=filename, fields_to_remove=fields_to_remove,
                                        fields_to_keep=fields_to_keep, outreg_vars=outreg_vars,
                                        vars_to_keep=vars_to_keep, model_col=model_col, status_col=status_col) %>%
  dplyr::mutate(model=c("state specific trend", rep("", 11)), status=c("public", rep("", 5), "private", rep("", 5)),
                Rating=rep(c("Least Green", "", "Moderate Green", "", "Most Green", ""), 2)) %>%
  {.}

splited_statetrend

summary_green = splited_baseline %>%
  dplyr::bind_rows(splited_statetrend) %>%
  dplyr::select(model, status, `Rating`, Below10F, ends_with("F")) %>%
  {.}

summary_green

summary_green %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_green_wide_sep_organization_greeness.csv")

result_table_tex <- xtable(summary_green, caption=sprintf("Regression result: comparing response to weather shock by state greeness"))
print(result_table_tex, tabular.environment = "longtable", include.rownames=FALSE,
      file="~/Dropbox/thesis/writeups/policy_cmp/tables/summary_green_wide_sep_organization_greeness.tex", size="\\footnotesize")

## generate regression result summary table for climate region
df_base =
  readr::read_tsv("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_climateregion_baseline.txt", skip=7, col_names=FALSE) %>%
  head(-5) %>%
  dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
    tidyr::fill(`X1`) %>%
    dplyr::filter(!(`variable` %in% c("ci_low", "ci_high")),
                  !grepl("0b.private", `X1`, fixed = TRUE),
                  !grepl("eDate", `X1`, fixed = TRUE),
                  !grepl("eState", `X1`, fixed = TRUE),
                  ## !grepl("1.private", `X1`, fixed = TRUE),
                  !grepl("Constant", `X1`, fixed = TRUE),
                  !grepl("hasretrofitbeforethismonth", `X1`, fixed = TRUE),
                  !grepl("o.avgTempBelow10", `X1`, fixed = TRUE),
                  ) %>%
    dplyr::group_by(`X1`) %>%
    dplyr::summarise_at(vars(X2:X9), function(x) return(sprintf("%s---%s", first(x), last(x)))) %>%
    dplyr::ungroup() %>%
    {.}
transposed_base = setNames(data.frame(t(df_base[,-1])), df_base[,1]$X1) %>%
  ## dplyr::mutate(`status`="public") %>%
  {.}
splited_base =
  tidyr::separate_rows(transposed_base,`avgTemp10to20`, `avgTemp20to30`, `avgTemp30to40`, `avgTemp40to50`,
                     `avgTemp50to60`, `avgTemp70to80`, `avgTemp80to90`, `avgTempAbove90`,
                     `avgTempBelow10`, sep="---") %>%
  dplyr::rename_all(function(x) {sprintf("%sF", gsub("avgTemp", "", x))}) %>%
  dplyr::mutate(`model`=c("baseline", rep("", 15))) %>%
  dplyr::mutate(`status`=c("public", rep("", 7), "private", rep("", 7))) %>%
  dplyr::mutate(`climate region`=rep(c("central", "", "East North Central", "", "Northeast", "", "Southeast", ""), 2)) %>%
  {.}

df_statetrend =
  readr::read_tsv("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_climateregion_statetrend.txt", skip=7, col_names=FALSE) %>%
  head(-5) %>%
  dplyr::mutate(`variable`=rep(c("coef", "se", "ci_low", "ci_high"), nrow(.)/4)) %>%
    tidyr::fill(`X1`) %>%
    dplyr::filter(!(`variable` %in% c("ci_low", "ci_high")),
                  !grepl("0b.private", `X1`, fixed = TRUE),
                  !grepl("eDate", `X1`, fixed = TRUE),
                  !grepl("eState", `X1`, fixed = TRUE),
                  ## !grepl("1.private", `X1`, fixed = TRUE),
                  !grepl("Constant", `X1`, fixed = TRUE),
                  !grepl("hasretrofitbeforethismonth", `X1`, fixed = TRUE),
                  !grepl("o.avgTempBelow10", `X1`, fixed = TRUE),
                  ) %>%
    dplyr::group_by(`X1`) %>%
    dplyr::summarise_at(vars(X2:X9), function(x) return(sprintf("%s---%s", first(x), last(x)))) %>%
    dplyr::ungroup() %>%
    {.}
transposed_statetrend = setNames(data.frame(t(df_statetrend[,-1])), df_statetrend[,1]$X1) %>%
  ## dplyr::mutate(`status`="public") %>%
  {.}
splited_statetrend =
  tidyr::separate_rows(transposed_statetrend,`avgTemp10to20`, `avgTemp20to30`, `avgTemp30to40`, `avgTemp40to50`,
                     `avgTemp50to60`, `avgTemp70to80`, `avgTemp80to90`, `avgTempAbove90`,
                     `avgTempBelow10`, sep="---") %>%
  dplyr::rename_all(function(x) {sprintf("%sF", gsub("avgTemp", "", x))}) %>%
  dplyr::mutate(`model`=c("state specific trend", rep("", 15))) %>%
  dplyr::mutate(`status`=c("public", rep("", 7), "private", rep("", 7))) %>%
  dplyr::mutate(`climate region`=rep(c("central", "", "East North Central", "", "Northeast", "", "Southeast", ""), 2)) %>%
  {.}
splited_statetrend

summary_climate = splited_base %>%
  dplyr::bind_rows(splited_statetrend) %>%
  dplyr::select(model, status, `climate region`, Below10F, ends_with("F")) %>%
  {.}

summary_climate %>%
  readr::write_csv("~/Dropbox/thesis/code/pubPriCmp/data-raw/summary_climate_wide.csv")

result_table_tex <- xtable(summary_climate, caption=sprintf("Regression result: comparing response to weather shock by climate region"))
print(result_table_tex, tabular.environment = "longtable", include.rownames=FALSE,
      file="~/Dropbox/thesis/writeups/policy_cmp/tables/summary_climate_wide.tex", size="\\footnotesize")


## generate regression result summary table for retrofit
## todo

## generate appendix tables

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

## appendix table comparing public and private
filename = "regression_results_bin_0720_gsa.txt"
fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "0b.private")
fields_to_keep = NULL
outreg_vars = c("coef", "se", "ci_low", "ci_high")
vars_to_keep = c("coef", "se")
df <- read_outreg2(filename=filename, fields_to_remove= fields_to_remove, fields_to_keep= fields_to_keep,
                   outreg_vars=outreg_vars, vars_to_keep=vars_to_keep)

fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "0b.private")
fields_to_keep = NULL
outreg_vars = c("coef", "se", "ci_low", "ci_high")
vars_to_keep = c("coef", "se")
setwd("~/Dropbox/thesis/writeups/policy_cmp/tables")
## ## for electric
## energy_type = ""
## for gas
## energy_type = "_gas"
energy_type = "_total"
df_gsa = read_outreg2(filename=sprintf("regression_results_bin_0720_gsa%s.txt", energy_type),
                      fields_to_remove= fields_to_remove,
                      fields_to_keep= fields_to_keep, outreg_vars=outreg_vars, vars_to_keep=vars_to_keep)

df_pnc = read_outreg2(filename=sprintf("regression_results_bin_0720_pnc%s.txt", energy_type),
                      fields_to_remove= fields_to_remove,
                      fields_to_keep= fields_to_keep, outreg_vars=outreg_vars, vars_to_keep=vars_to_keep)

df_comb = read_outreg2(filename=sprintf("regression_results_bin_0720%s.txt", energy_type),
                       fields_to_remove= fields_to_remove,
                       fields_to_keep= fields_to_keep, outreg_vars=outreg_vars, vars_to_keep=vars_to_keep)

df_gsa_comb = df_comb %>%
  dplyr::filter(!grepl("1.private#", `X1`, fixed=TRUE)) %>%
  {.}
df_pnc_comb = df_comb %>%
  dplyr::filter(grepl("1.private#", `X1`, fixed=TRUE)) %>%
  dplyr::mutate(`X1`=gsub("1.private#c.", "", fixed=TRUE, `X1`)) %>%
  {.}

labels =
  breaklabels = c("<10", "[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)", "[70-80)", "[80-90)",
                  ">90")
omits = rep("", length(labels))
rownames = c(rbind(labels, omits))
model_mapping = readr::read_csv("pub_pri_cmp_model_mapping.csv")
dfresult =
  data.frame(`var`=rownames, `(1)`=df_gsa_comb$X2, `(2)`=df_gsa_comb$X3, `(3)`=df_gsa_comb$X4,
            `(4)`=df_pnc_comb$X2, `(5)`=df_pnc_comb$X3, `(6)`=df_pnc_comb$X4,
            `(7)`=df_gsa$X2, `(8)`=df_gsa$X3, `(9)`=df_pnc$X2, `(10)`=df_pnc$X3,
            check.names=FALSE) %>%
  dplyr::bind_rows(model_mapping) %>%
  {.}

result_table_tex <- xtable(dfresult, caption=sprintf("Regression result: comparing public vs private response to weather shock"))
print(result_table_tex, tabular.environment = "table", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/summary_pub_pri_cmp_long%s.tex", energy_type), size="\\footnotesize")

result_table_tex <- xtable(dfresult%>%select(var, `(1)`:`(6)`), caption=sprintf("Regression result: comparing public vs private response to weather shock (combined model)"))
print(result_table_tex, tabular.environment = "table", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/summary_pub_pri_cmp_long_combined%s.tex", energy_type), size="\\footnotesize")

result_table_tex <- xtable(dfresult%>%select(var, `(7)`:`(10)`), caption=sprintf("Regression result: comparing public vs private response to weather shock (separate model)"))
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/summary_pub_pri_cmp_long_separate%s.tex", energy_type), size="\\footnotesize")

## plot model result for climate region
## get result table for each model for climate heterogeneity analysis
## energy_type: "" for elec, "_gas" for gas, "_total" for elec + gas
process_region_table <- function(breaklabels, rownames, energy_type, modelname,
                                 region_name, region_label) {
  filename = sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_%s_%s%s.txt",
                    region_name, modelname, energy_type)

  fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "0b.private", "o.avgTempBelow10")
  ields_to_keep = NULL
  outreg_vars = c("coef", "se", "ci_low", "ci_high")
  vars_to_keep = c("coef", "se")
  if (modelname == "baseline") {
    modelyn = "N"
    shift = 0
  } else {
    modelyn = "Y"
    shift = 4
  }
  colnames = c("var", sprintf("(%s)", (1 + shift):(ncol(dfresult) - 1 + shift)))
  dfresult <- read_outreg2(filename=filename, fields_to_remove= fields_to_remove, fields_to_keep= fields_to_keep,
                          outreg_vars=outreg_vars, vars_to_keep=vars_to_keep) %>%
    dplyr::select(-`variable`) %>%
    {.}
  names(dfresult)=colnames
  dfresult$var = rownames
  if (region_name == "climateregion") {
    model_mapping = readr::read_csv(sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/%s_model_mapping.csv", region_name))
  } else if (region_name == "region_wiki") {
    model_mapping = rbind.data.frame(c("private", "N", "N", "Y", "Y"),
                                    c("US Region", rep(c("north", "south"), 2)),
                                    c("state trend", rep(modelyn, 4)))
    names(model_mapping) = colnames
  }
  dfresult <- dfresult %>%
    dplyr::bind_rows(model_mapping) %>%
    {.}
  return(dfresult)
}

labels =
  breaklabels = c("<10", "[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)", "[70-80)", "[80-90)",
                  ">90")
omits = rep("", length(labels))
rownames = c(rbind(labels, omits))

## ## for climate region
## result_baseline = process_region_table(breaklabels=breaklabels, rownames=rownames, energy_type="_total", modelname="baseline", region_name="climateregion", region_label="climate region")
## for US region
result_baseline = process_region_table(breaklabels=breaklabels, rownames=rownames, energy_type="_total", modelname="baseline", region_name="region_wiki", region_label="US region")
head(result_baseline)
result_statetrend = process_region_table(breaklabels=breaklabels, rownames=rownames, energy_type="_total", modelname="statetrend", region_name="region_wiki", region_label="US region")
head(result_statetrend)

## use two tables, each has a model type
modelname = "baseline"
result_table_tex <- xtable(result_baseline, caption=sprintf("Regression result: comparing response to weather shock by %s (%s model)", region_label, modelname))
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/summary_%s_long_%s%s.tex", region_name, modelname, energy_type),
      size="\\footnotesize")
modelname = "statetrend"
result_table_tex <- xtable(result_statetrend, caption=sprintf("Regression result: comparing response to weather shock by %s (%s model)", region_label, modelname))
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/summary_%s_long_%s%s.tex", region_name, modelname, energy_type),
      size="\\footnotesize")

## for US region
dfresult = result_baseline %>%
  dplyr::bind_cols(result_statetrend[,2:ncol(result_statetrend)])

result_table_tex <- xtable(dfresult, caption=sprintf("Regression result: comparing response to weather shock by %s", region_label))
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/summary_%s_long%s.tex", region_name, energy_type),
      size="\\footnotesize")

## slides table: greeness combined model of greeness
## energy_type = "_gas"
## suffix = "pnc"
## status_code = "private"
energy_type = "_gas"
suffix = "gsa"
status_code = "public"
## appendix summary table of greeness public model with greeness x bin
filename = sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_results_bin_greeness_0720_%s%s.txt",
                   suffix, energy_type)
fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "1b.eGreeness")
fields_to_keep = NULL
outreg_vars = c("coef", "se", "ci_low", "ci_high")
vars_to_keep = c("coef", "se")
dfresult <- read_outreg2(filename=filename, fields_to_remove= fields_to_remove, fields_to_keep= fields_to_keep,
                         outreg_vars=outreg_vars, vars_to_keep=vars_to_keep) %>%
  dplyr::select(-`variable`) %>%
  {.}

dfresult <- dfresult %>%
  dplyr::mutate(`Rating`=ifelse(grepl("2.eGreeness#c.", X1), "Moderate Green", "Least Green")) %>%
  dplyr::mutate(`Rating`=ifelse(grepl("3.eGreeness#c.", X1), "Most Green", `Rating`)) %>%
  dplyr::mutate(`X1`=gsub("2.eGreeness#c.", "", `X1`)) %>%
  dplyr::mutate(`X1`=gsub("3.eGreeness#c.", "", `X1`)) %>%
  ## dplyr::mutate(`X1`=ifelse(as.numeric(rownames(.)) %% 2 == 1, `X1`, "")) %>%
  dplyr::mutate(X1=gsub("avgTemp", "", X1), X1=gsub("to", "-", X1), X1=gsub("Above", ">", X1),
                X1=gsub("Below", "<", X1)) %>%
  dplyr::mutate(X1=ifelse(nchar(X1)>4, sprintf("[%s)", X1), X1)) %>%
  {.}
dfleast = dfresult %>%
  dplyr::filter(`Rating`=="Least Green") %>%
  dplyr::select(-`Rating`)
dfmoderate = dfresult %>%
  dplyr::filter(`Rating`=="Moderate Green") %>%
  dplyr::select(-`Rating`, -`X1`)
dfmost = dfresult %>%
  dplyr::filter(`Rating`=="Most Green") %>%
  dplyr::select(-`Rating`, -`X1`)
dfcbind = cbind(dfleast, dfmoderate, dfmost)
names(dfcbind) = c("var", sprintf("(%s)", 1:6))
final = dfcbind %>%
  dplyr::bind_rows(
  data.frame(`var`=c("Moderate Green", "Most Green", "state trend"),
            `(1)`=c("N", "N", "N"), `(2)`=c("N", "N", "Y"), `(3)`=c("Y", "N", "N"),
            `(4)`=c("Y", "N", "Y"), `(5)`=c("N", "Y", "N"), `(6)`=c("N", "Y", "Y"), check.names = FALSE)) %>%
  dplyr::mutate(`var`=ifelse(as.numeric(rownames(.)) %% 2 == 1, `var`, ""))

result_table_tex <- xtable(final, caption=sprintf("Regression result: comparing response to weather shock by state greeness (%s portfolio)", status_code))
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/summary_greeness_long_%s%s.tex", suffix,
                   energy_type), size="\\footnotesize")

energy_type = "_gas"
suffix = "gsa"
status_code = "public"
## appendix summary table of greeness public model with greeness x bin
filename = sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/regression_results_bin_greeness_0720_%s%s.txt",
                   suffix, energy_type)
fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "1b.eGreeness")
fields_to_keep = NULL
outreg_vars = c("coef", "se", "ci_low", "ci_high")
vars_to_keep = c("coef", "se")
dfresult <- read_outreg2(filename=filename, fields_to_remove= fields_to_remove, fields_to_keep= fields_to_keep,
                         outreg_vars=outreg_vars, vars_to_keep=vars_to_keep) %>%
  dplyr::select(-`variable`) %>%
  {.}

process_green_table <- function(breaklabels, rownames, energy_type, modeltype) {
  filename = sprintf("regression_greeness_%s%s.txt", modeltype, energy_type)
  fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "0b.private", "1.private")
  fields_to_keep = NULL
  outreg_vars = c("coef", "se", "ci_low", "ci_high")
  vars_to_keep = c("coef", "se")
  dfresult = read_outreg2(filename=filename, fields_to_remove=fields_to_remove,
                          fields_to_keep=fields_to_keep, outreg_vars=outreg_vars,
                          vars_to_keep=vars_to_keep) %>%
    dplyr::select(-`variable`) %>%
    {.}
  if (modeltype == "baseline") {
    modelyn = "N"
    shift = 0
  } else {
    modelyn = "Y"
    shift = 4
  }
  colnames = c("var", sprintf("(%s)", (1 + shift):(ncol(dfresult) - 1 + shift)))
  dfresult <-
    dfresult %>%
    dplyr::mutate(X1=gsub("avgTemp", "", X1), X1=gsub("to", "-", X1), X1=gsub("Above", ">", X1),
                  X1=gsub("Below", "<", X1)) %>%
    dplyr::mutate(X1=ifelse(nchar(X1)>4, sprintf("[%s)", X1), X1)) %>%
    {.}
  names(dfresult)=colnames
  dfresult$var = rownames
  if (ncol(dfresult) == 7) {
    print("asdf")
    dfresult <- dfresult %>%
      dplyr::bind_rows(data.frame(`var`=c("Private", "Moderate Green", "Most Green", "state trend"),
                                  `(1)`=c("N", "N", "N", "N"), `(2)`=c("N", "Y", "N", "N"),
                                  `(3)`=c("N", "N", "Y", "N"), `(4)`=c("Y", "N", "N", "N"),
                                  `(5)`=c("Y", "Y", "N", "N"), `(6)`=c("Y", "N", "Y", "N"),
                                  check.names = FALSE)) %>%
      {.}
  } else if (ncol(dfresult) == 5) {
    model_mapping = rbind.data.frame(c("private", "N", "N", "Y", "Y"),
                                     c("Greeness", rep(c("Least", "Most"), 2)),
                                     c("state trend", rep(modelyn, 4)))
    names(model_mapping) = colnames
    dfresult <- dfresult %>%
      ## dplyr::bind_rows(data.frame(`var`=c("Private", "Most Green", "state trend"),
      ##                             `(1)`=c("N", "N", "N"), `(2)`=c("N", "Y", "N"),
      ##                             `(3)`=c("Y", "N", "N"), `(4)`=c("Y", "Y", "N"),
      ##                             check.names = FALSE)) %>%
      dplyr::bind_rows(model_mapping) %>%
      {.}
  }
  return(dfresult)
}

breaklabels = c("<10", "[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)", "[70-80)", "[80-90)",
                ">90")
omits = rep("", length(breaklabels))
rownames = c(rbind(breaklabels, omits))
setwd("~/Dropbox/thesis/writeups/policy_cmp/tables")
modeltype = "baseline"
result_baseline = process_green_table(breaklabels=breaklabels, rownames=rownames, energy_type="_total", modeltype=modeltype)
result_table_tex <- xtable(result_baseline, caption=sprintf("Regression result: comparing response to weather shock by greeness (%s model)", modeltype))
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/summary_greeness_separate_organization_%s%s.tex", modeltype, energy_type),
      size="\\footnotesize")

modeltype = "statetrend"
result_statetrend = process_green_table(breaklabels=breaklabels, rownames=rownames, energy_type="_total", modeltype=modeltype)
result_table_tex <- xtable(result_baseline, caption=sprintf("Regression result: comparing response to weather shock by greeness (%s model)", modeltype))
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/summary_greeness_separate_organization_%s%s.tex", modeltype, energy_type),
      size="\\footnotesize")

dfresult = result_baseline %>%
  dplyr::bind_cols(result_statetrend[,2:ncol(result_statetrend)])

dfresult

result_table_tex <- xtable(result_baseline, caption="Regression result: comparing response to weather shock by greeness")
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/summary_greeness_separate_organization%s.tex", energy_type),
      size="\\footnotesize")

setwd("~/Dropbox/thesis/writeups/policy_cmp/tables")
## read result of additive regression model
## energy_type = ""
energy_type = "_gas"
if (energy_type == "") {
  energy_label = "electricity"
} else if (energy_type == "_gas") {
  energy_label = "gas"
}
filename = sprintf("additive_retrofit_bin%s.txt", energy_type)
fields_to_remove = c(".eDate", "eState", "Constant", "avgTemp")
fields_to_keep = NULL
outreg_vars = c("coef", "se", "ci_low", "ci_high")
vars_to_keep = c("coef", "se")
df <-
  read_outreg2(filename=filename, fields_to_remove= fields_to_remove, fields_to_keep= fields_to_keep,
                   outreg_vars=outreg_vars, vars_to_keep=vars_to_keep) %>%
  dplyr::mutate(`X1`=ifelse(as.numeric(rownames(.)) %% 2 == 1, `X1`, "")) %>%
  dplyr::select(-`variable`) %>%
  dplyr::rename(`Retrofit`=`X1`, `(1)`=`X2`, `(2)`=`X3`) %>%
  {.}

model_mapping = rbind.data.frame(c("state trend", "N", "Y"))
names(model_mapping) = names(df)
df <- df %>%
  dplyr::bind_rows(model_mapping)

result_table_tex <- xtable(df, caption=sprintf("Regression result: retrofit DD model (%s)", energy_label))
print(result_table_tex, tabular.environment = "tabular", include.rownames=FALSE,
      file=sprintf("~/Dropbox/thesis/writeups/policy_cmp/tables/additive_model_result%s.tex", energy_type),
      size="\\footnotesize")

## ----------------------------------------------------------------------------------------
## process output from tex_tables.tex from stata output
## ----------------------------------------------------------------------------------------
setwd("~/Dropbox/thesis/writeups/policy_cmp/tables")

folder.suffixes = c("",
                    "_o_o_owned", "_o_oct_owned", "_r_o_owned", "_ro_oct_owned",
                    "_o_o_owned_and_leased", "_o_oct_owned_and_leased", "_r_o_owned_and_leased", "_ro_oct_owned_and_leased"
                    )

captions = c("no restrictions",
              "Public: owned offices; Private: owned offices",
              "Public: owned offices, courthouses, CT/Offices; Private: owned offices",
              "Public: owned offices; Private: owned retails",
              "Public: owned offices, courthouses, CT/Offices; Private: owned offices, retails",
              "Public: offices; Private: offices",
              "Public: offices, courthouses, CT/Offices; Private: offices",
              "Public: offices; Private: retails",
              "Public: offices, courthouses, CT/Offices; Private: offices, retails")

## process main results
for (i in seq_along(folder.suffixes)) {
  s = folder.suffixes[i]
  print(s)
  inputFile <- sprintf("reg_result%s/regression_portfolio_total.tex", s)
  outputFile <- sprintf("reg_result%s/regression_portolio_total_postprocess.tex", s)
  con  <- file(inputFile, open = "r")
  lines <- readLines(con, warn = FALSE)
  close(con)
  ntotal = length(lines)
  newlines <- c("\\begin{frame}",
                "\\centering",
                "\\scriptsize",
                "\\begin{table}",
                lines[4:5],
                lines[8:24], "private & N & N & Y & Y \\\\",
                "state trend & N & Y & N & Y \\\\",
                lines[(ntotal - 5):(ntotal - 1)],
                sprintf("\\caption{%s}", captions[i]),
                "\\end{table}",
                "\\end{frame}")
  con <- file(outputFile, open = "w+")
  writeLines(newlines, con, sep = "\n", useBytes = FALSE)
  close(con)
}


## for (modeltype in c("baseline", "statetrend")) {
##   for (i in seq_along(folder.suffixes)[2:9]) {
##     s = folder.suffixes[i]
##     print(s)
##     inputFile <- sprintf("reg_result%s/regression_region_wiki_%s_total.tex", s, modeltype)
##     inputtxt <- sprintf("reg_result%s/regression_region_wiki_%s_total.txt", s, modeltype)
##     outputFile <- sprintf("reg_result%s/regression_region_wiki_%s_total_postprocess.tex", s, modeltype)
##     file.remove(inputFile)
##     file.remove(inputtxt)
##     file.remove(outputFile)
##   }
## }

## process us region results
for (modeltype in c("baseline", "statetrend")) {
  for (i in seq_along(folder.suffixes)) {
    s = folder.suffixes[i]
    print(s)
    inputFile <- sprintf("reg_result%s/regression_region_wiki_%s_total.tex", s, modeltype)
    outputFile <- sprintf("reg_result%s/regression_region_wiki_%s_total_postprocess.tex", s, modeltype)
    con  <- file(inputFile, open = "r")
    lines <- readLines(con, warn = FALSE)
    close(con)
    ntotal = length(lines)
    newlines <- c("\\begin{frame}",
                  "\\centering",
                  "\\scriptsize",
                  "\\begin{table}",
                  lines[4:5],
                  lines[8:25],
                  "portfolio & public & public & private & private  \\\\",
                  "US region & North & South & North & South \\\\",
                  lines[(ntotal - 5):(ntotal - 1)],
                  sprintf("\\caption{%s}", captions[i]),
                  "\\end{table}",
                  "\\end{frame}")
    con <- file(outputFile, open = "w+")
    writeLines(newlines, con, sep = "\n", useBytes = FALSE)
    close(con)
  }
}

setwd("~/Dropbox/thesis/writeups/policy_cmp/tables")

green = c("", "_howestate", "_howecounty")
sourcename = c("Majority Vote", "State Data from Howe 2015", "County Data from Howe 2015")

## process greenness results
for (modeltype in c("baseline", "statetrend")) {
  for (j in seq_along(green)) {
    green.suffix = green[j]
    for (i in seq_along(folder.suffixes)) {
      s = folder.suffixes[i]
      print(s)
      inputFile <- sprintf("reg_result%s/regression_greeness_%s_total%s.tex", s, modeltype, green.suffix)
      outputFile <- sprintf("reg_result%s/regression_greeness_%s_total_postprocess%s.tex", s, modeltype, green.suffix)
      con  <- file(inputFile, open = "r")
      lines <- readLines(con, warn = FALSE)
      close(con)
      ntotal = length(lines)
      newlines <- c("\\begin{frame}",
                    "\\centering",
                    "\\scriptsize",
                    "\\begin{table}",
                    lines[4:5],
                    lines[8:25],
                    "portfolio & public & public & private & private  \\\\",
                    "Greenness & Least & Most & Least & Most \\\\",
                    lines[(ntotal - 5):(ntotal - 1)],
                    sprintf("\\caption{%s, greenness source: %s, model: %s}", captions[i], sourcename[j], modeltype),
                    "\\end{table}",
                    "\\end{frame}")
      con <- file(outputFile, open = "w+")
      writeLines(newlines, con, sep = "\n", useBytes = FALSE)
      close(con)
    }
  }
}

## process input file the list of tex tables for greenness result
newlines = NULL
for (i in seq_along(folder.suffixes)) {
  for (modeltype in c("baseline", "statetrend")) {
    for (j in seq_along(green)) {
      green.suffix = green[j]
      s = folder.suffixes[i]
      newlines = c(newlines,
                   sprintf("\\input{tables/reg_result%s/regression_greeness_%s_total_postprocess%s}",
              s, modeltype, green.suffix))
    }
  }
}
con <- file("../greeness_tables.tex", open = "w+")
writeLines(newlines, con, sep = "\n", useBytes = FALSE)
close(con)

## outputname = "mean_summary_tables"
## tableprefix = "energy_weather_n_by_region_green"
outputname = "tstat_summary_tables"
tableprefix = "balance_compare"
setwd("~/Dropbox/thesis/writeups/policy_cmp/tables")
## process input file of the list of tex summary tables
newlines = NULL
for (i in seq_along(folder.suffixes)) {
    s = folder.suffixes[i]
    newlines = c(newlines,
                 "\\begin{frame}",
                "\\centering",
                "\\scriptsize",
                sprintf("\\input{tables/%s%s}", tableprefix, s),
                "\\end{frame}")
}
con <- file(sprintf("../%s.tex", outputname), open = "w+")
writeLines(newlines, con, sep = "\n", useBytes = FALSE)
close(con)

