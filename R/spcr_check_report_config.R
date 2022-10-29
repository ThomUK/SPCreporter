spcr_check_report_config <- function(report_config) {
  assertthat::assert_that(
    inherits(report_config, "data.frame"),
    msg = "spcr_check_report_config: The report config must be a dataframe."
  )

  mandatory_columns <- c(
    "ref",
    "measure_name",
    "domain",
    "aggregation"
  )

  assertthat::assert_that(
    all(mandatory_columns %in% names(report_config)),
    msg = "spcr_check_report_config: report_config is missing a mandatory column.  Check the input spreadsheet."
  )

  # convert refs to character vectors
  report_config$ref <- as.character(report_config$ref)

  return(report_config)
}
