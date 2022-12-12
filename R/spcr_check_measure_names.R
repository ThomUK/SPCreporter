#' (internal function) Check measure names to avoid mismatches
#' Returns error message in cases of mismatch
#'
#' @param ref_no A single reference number to check
#' @param measure_data Data frame of measure data in wide format
#' @param measure_config Data frame of config data
#'
#' @return NULL
#' @noRd
#'
spcr_check_measure_names <- function(ref_no, measure_data, measure_config) {
  # check that the config table includes this ref_no number
  assertthat::assert_that(ref_no %in% measure_config$ref,
    msg = usethis::ui_stop(
      paste0("spcr_check_measure_names: Config data for ref ", ref_no,
      " is missing from the measure_config data frame."
    )))

  # find the titles to compare
  m_title <- measure_data |>
    dplyr::filter(ref == ref_no) |>
    dplyr::pull(measure_name) |>
    unique()
  c_title <- measure_config |>
    dplyr::filter(ref == ref_no) |>
    dplyr::pull(measure_name) |>
    unique()

  # check that the titles match
  assertthat::assert_that(
    isTRUE(m_title == c_title),
    msg = usethis::ui_stop(paste0(
      "spcr_check_measure_names: There is a name mismatch for measure ref: ",
      ref_no,
      ". Check for typos or mismatching refs or data."
    )))

  invisible(TRUE)
}
