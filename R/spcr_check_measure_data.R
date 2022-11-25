#' Check the measure data
#'
#' @param .data list. List containing dataframes of data in wide format.
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
    all(c("week", "month") %in% names(.data)),
    msg = "spcr_check_measure_data: The list items must be from 'week' or 'month'."
  )

  # convert refs to character vectors
  .data[["week"]]$ref <- as.character(.data[["week"]]$ref)
  .data[["month"]]$ref <- as.character(.data[["month"]]$ref)

  return(.data)
}
