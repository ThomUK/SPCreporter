#' Make a bundle of data including all SPC calcs
#'
#' @param measure_data list. List containing dataframes of data in wide format
#' @param measure_config dataframe. Config information for the measures
#' @param report_config dataframe. Config information for the report
#'
#' @return dataframe. A nested dataframe containing calculated charts and parsed text
#' @export
#'
spcr_make_data_bundle <- function(measure_data, measure_config, report_config) {

  # check report_config
  report_config <- spcr_check_report_config(report_config)

  # check measure_data, and lengthen the different aggregation levels into a single long dataframe
  # adding the frequency in as a column
  measure_data <- spcr_check_measure_data(measure_data) %>%
    purrr::map2_df(.y = names(measure_data), .f = spcr_lengthen_measure_data)

  # check measure_config
  measure_config <- spcr_check_measure_config(measure_config)

  # make a vector of the ref numbers to create charts for
  refs <- report_config %>%
    dplyr::pull("ref") %>% unique()

  # check reference numbers and measure names agree across both data frames
  # this is to guard against typos and errors in reported figures
  # by ensuring a typo in one place (ref or title) will create an error
  purrr::walk(refs, spcr_check_measure_names, measure_data = measure_data, measure_config = measure_config)

  # map over each measure to do the calculations
  result <- purrr::map2_df(
    .x = report_config$ref,
    .y = report_config$aggregation,
    .f = spcr_calculate_row,
    measure_data = measure_data,
    measure_config = measure_config,
    report_config = report_config
  )

  return(result)

}
