#' @importFrom magrittr %>%
NULL
#' read output table from outreg2 of stata
#'
#' This function reads output from stata outreg2
#' @param filename, name of the file to read
#' @param fields_to_remove, a vector of string, X1 containing substrings in the vector will be removed
#' @param fields_to_keep, a vector of string, X1 containing substrings in the vector will be kept
#' @param outreg_vars, variables output from outreg2, eg, ci_low, se
#' @param vars_to_keep, amone outreg_vars, which to keep
#' @keywords read outreg2
#' @export
#' @examples
#' filename = "regression_results_bin_0720.txt"
#' fields_to_remove = c(".eDate", "eState", "Constant", "hasretrofitbeforethismonth", "0b.private", "1.private")
#' fields_to_keep = NULL
#' outreg_vars = c("coef", "se", "ci_low", "ci_high")
#' vars_to_keep = c("coef", "se")
#' read_outreg2(filename=filename, fields_to_keep=fields_to_keep, fields_to_remove=fields_to_remove, outreg_vars=outreg_vars, vars_to_keep=vars_to_keep)
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
