#' (internal function) Check the config data
#'
#' @param .data Dataframe of config data
#'
#' @return dataframe after checking
#' @noRd
#'
spcr_check_config_data <- function(.data) {

  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = "spcr_check_config_data: config_data must be a data.frame"
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
    msg = "spcr_check_config_data: config_data is missing a mandatory column.  Check the input spreadsheet."
  )

  return(.data)
}
