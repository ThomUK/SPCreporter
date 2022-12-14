#' Check all required data items are provided
#'
#' @param report_config A data frame. The report config detailing required report items
#' @param measure_data Data frame in either wide format
#'
#' @returns logical TRUE if check is successful, else an error message
#' @export
#'
spcr_check_dataset_is_complete <- function(report_config, measure_data) {

  required_data <- report_config |>
    dplyr::distinct(ref, measure_name, aggregation)

  supplied_data <- measure_data |>
    dplyr::select(ref, aggregation)

  missing_data <- required_data |>
    dplyr::anti_join(supplied_data, by = c("ref", "aggregation"))


  # build an error message if there are missing data items
  assertthat::assert_that(
    nrow(missing_data) == 0,
    msg = usethis::ui_stop(
      dplyr::slice(missing_data, 1) |>
        stringr::str_glue_data(
          "spcr_check_dataset_is_complete: Data is missing for
          {nrow(missing_data)} report items. The first is ref {ref},
          '{measure_name}', {aggregation}ly.")))

  invisible(TRUE)
}
