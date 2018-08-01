## fixme: figure out the source of the location for PNC data
#' Copy raw file from external
#'
#' This function copies in raw data files from an external working directory
#' @keywords copy raw
#' @export
#' @examples
#' get_nearby_isd_stations(lat_lon_df, isd_data=isd_data, radius = 100, limit=5,
copy_raw <- function () {
  ## get energy file
  devtools::use_data_raw(pkg="~/Dropbox/thesis/code/pubPriCmp")
  inputdir = "~/Dropbox/thesis/writeups/policy_cmp/tables"
  outputdir = "~/Dropbox/thesis/code/"
  df = readr::read_csv("~/Dropbox/thesis/writeups/policy_cmp/tables/all_energy.csv") %>%
    as.data.frame() %>%
    readr::write_csv("~/Dropbox/thesis/writeups/policy_cmp/tables/%s", filename)
    {.}
}
