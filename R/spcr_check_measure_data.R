#' Check the measure data
#'
#' @param .data list. List containing dataframes of data in wide format.
#'
#' @return List
#' @noRd
#'
spcr_check_measure_data <- function(.data) {
  assertthat::assert_that(
    inherits(.data, "list"),
    msg = "spcr_check_measure_data: The data must be a list."
  )

  # convert list names to lower case
  names(.data) <- names(.data) |> tolower()

  assertthat::assert_that(
    any(c("week", "month") %in% names(.data)),
    msg = "spcr_check_measure_data: Data for either 'week' or 'month' is required."
  )

  # convert refs to character vectors
  if("week" %in% names(.data)) .data[["week"]]$ref <- as.character(.data[["week"]]$ref)
  if("month" %in% names(.data)) .data[["month"]]$ref <- as.character(.data[["month"]]$ref)

  return(.data)
}
