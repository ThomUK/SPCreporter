#' (internal function) Check the measure data
#'
#' @param .data List. List containing dataframes of data in wide format.
#'
#' @return List
#' @noRd
#'
spcr_check_measure_data <- function(.data) {

  assertthat::assert_that(
    class(.data) == "list",
    msg = "spcr_check_measure_data: The data must be a list."
  )

  assertthat::assert_that(
    all(names(.data) %in% c("week", "month")),
    msg = "spcr_check_measure_data: The list items must be from 'week' or 'month'."
  )

  return(.data)
}
