#' (internal function) Check measure names to avoid mismatches
#' Returns errors messages in cases of mismatch
#'
#' @param ref_no A single ref_noerence number to check
#' @param measure_data Dataframe of measure data
#' @param measure_config Dataframe of config data
#'
#' @return NULL
#' @noRd
#'
spcr_check_measure_names <- function(ref_no, measure_data, measure_config) {
  # check that the config table includes this ref_no number
  assertthat::assert_that(
    length(measure_config[measure_config$ref == ref_no, ]$ref) > 0,
    msg = paste0(
      "spcr_check_measure_names: Config data for ref ",
      ref_no,
      " is missing from the measure_config dataframe."
    )
  )

  # find the titles to compare
  m_title <- measure_data[measure_data$ref == ref_no, ]$measure_name |> unique()
  c_title <- measure_config |>
    dplyr::filter(measure_config$ref == ref_no) |>
    dplyr::pull(measure_name) |>
    unique()

  # check that the titles match
  assertthat::assert_that(
    isTRUE(m_title == c_title),
    msg = paste0(
      "spcr_check_measure_names: There is a name mismatch for measure ref: ",
      ref_no,
      ". Check for typos or mismatching refs or data."
    )
  )

  return(TRUE)
}
