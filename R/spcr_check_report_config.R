spcr_check_report_config <- function(report_config) {
  assertthat::assert_that(
    inherits(report_config, "data.frame"),
    msg = "spcr_check_report_config: The report config must be a data frame."
  )

  # check for column names, and provide a helpful error message if needed
  required_columns <- c(
    "ref",
    "measure_name",
    "domain",
    "aggregation"
  )

  # check required cols are present
  report_config |>
    spcr_check_for_required_columns("report_config", required_columns) |>
    dplyr::distinct() |>

    # convert refs to character vectors
    dplyr::mutate(across(ref, as.character))
}
