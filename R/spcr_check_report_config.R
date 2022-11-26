spcr_check_report_config <- function(report_config) {
  assertthat::assert_that(
    inherits(report_config, "data.frame"),
    msg = "spcr_check_report_config: The report config must be a dataframe."
  )

  # check for column names, and provide a helpful error message if needed
  required_columns <- c(
    "ref",
    "measure_name",
    "domain",
    "aggregation"
  )

  # check required cols are present
  spcr_check_for_required_columns(report_config, "report_config", required_columns)

  # convert refs to character vectors
  report_config$ref <- as.character(report_config$ref)

  return(report_config)
}
