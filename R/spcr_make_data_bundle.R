#' Make a bundle of data including all SPC calcs
#'
#' @param measure_data list. List containing dataframes of data in wide format
#' @param report_config dataframe. Config information for the report
#' @param measure_config dataframe. Config information for the measures
#'
#' @return dataframe. A nested dataframe containing calculated charts and parsed text
#' @export
#'
spcr_make_data_bundle <- function(measure_data = test_measure_data,
                                  report_config = test_report_config,
                                  measure_config = test_measure_config) {
  # check report_config
  report_config <- spcr_check_report_config(report_config)

  # check measure_data
  measure_data <- spcr_check_measure_data(measure_data)

  # check measure_config
  measure_config <- spcr_check_measure_config(measure_config)

  # check all required data is supplied
  spcr_check_dataset_is_complete(report_config, measure_data)

  # lengthen the measure data aggregation levels into a single long dataframe
  # adding the frequency in as a column
  measure_data <- measure_data |>
    purrr::map2_df(.y = names(measure_data), .f = spcr_lengthen_measure_data)

  # make a vector of the ref numbers to create charts for
  refs <- report_config |>
    dplyr::pull("ref") |>
    unique()

  # check reference numbers and measure names agree across both data frames
  # this is to guard against typos and errors in reported figures
  # by ensuring a typo in one place (ref or title) will create an error
  purrr::walk(refs,
              .f = spcr_check_measure_names,
              measure_data = measure_data,
              measure_config = measure_config
  )

  # map over each measure to do the calculations
  result <- purrr::map2_df(
    .x = report_config$ref,
    .y = report_config$aggregation,
    .f = spcr_calculate_row,
    measure_data = measure_data,
    measure_config = measure_config,
    report_config = report_config
  )

  add_domain_heading <- function(dtf, sort_col = "sort_order") {
    dtf |>
      dplyr::arrange({{ sort_col }}) |>
      dplyr::mutate(Needs_Domain_Heading = c(TRUE, rep(FALSE, nrow(dtf) - 1)))
  }

  result |>
    dplyr::mutate(sort_order = dplyr::row_number()) |>
    dplyr::group_by(Domain) |>
    dplyr::group_modify(add_domain_heading) |>
    dplyr::ungroup() |>
    dplyr::arrange(sort_order) |>
    # restore original column order (and remove sort_order col)
    dplyr::select(all_of(c(names(result), "Needs_Domain_Heading")))
}
