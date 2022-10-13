#' (internal function) Check the measure config info
#'
#' @param .data Dataframe of measure config
#'
#' @return dataframe after checking
#' @noRd
#'
spcr_check_measure_config <- function(.data) {

  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = "spcr_check_measure_config: config_data must be a data.frame"
  )

  mandatory_columns <- c(
    "ref",
    "measure_name",
    "data_source",
    "data_owner",
    "lead_person",
    "unit",
    "improvement_direction",
    "target",
    "target_set_by",
    "data_quality",
    "baseline_period",
    "rebase_dates",
    "rebase_comment"
  )

  assertthat::assert_that(
    all(mandatory_columns %in% names(.data)),
    msg = "spcr_check_measure_config: config_data is missing a mandatory column.  Check the input spreadsheet."
  )

  return(.data)
}
