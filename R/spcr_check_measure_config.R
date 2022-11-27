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

  # check for column names, and provide a helpful error message if needed
  required_columns <- c(
    "ref",
    "measure_name",
    "data_source",
    "data_owner",
    "accountable_person",
    "unit",
    "improvement_direction",
    "target",
    "target_set_by",
    "data_quality",
    "baseline_period",
    "rebase_dates",
    "rebase_comment"
  )

  # check required cols are present
  spcr_check_for_required_columns(.data, "measure_config", required_columns)

  # convert refs to character vectors
  .data$ref <- as.character(.data$ref)

  # convert target to numeric
  # warnings for coercing "-" to NA are surpressed
  suppressWarnings(
    .data$target <- as.numeric(.data$target)
  )

  return(.data)
}
