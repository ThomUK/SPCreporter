#' (internal function) Check the measure config info
#'
#' @param .data Data frame of measure config
#'
#' @return data frame after checking
#' @noRd
#'
spcr_check_measure_config <- function(.data) {
  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = "spcr_check_measure_config: config_data must be a data frame"
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

  optional_columns <- c(
    "allowable_days_lag",
    "reviewed_at",
    "escalated_to"
  )

  .data |>
    # check required cols are present
    spcr_check_for_required_columns("measure_config", required_columns) |>

    spcr_check_for_optional_columns(optional_columns) |>

    # convert refs to character vectors
    dplyr::mutate(across(ref, as.character))
}
