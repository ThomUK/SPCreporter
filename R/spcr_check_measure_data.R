#' Check the measure data and transform
#'
#' @param .data list. List containing data frames of data in wide format.
#' @param check_names Character vector. Names of elements of .data that are
#' permitted to be used. At least one of the names must be present. Elements
#' with names not included here will be discarded in the data bundle process.
#'
#' @returns A data frame
#' @noRd
#'
spcr_check_measure_data <- function(.data, check_names = c("week", "month")) {
  assertthat::assert_that(
    inherits(.data, "list"),
    msg = "spcr_check_measure_data: The data must be a list."
  )

  # convert list names to lower case
  names(.data) <- tolower(names(.data))

  assertthat::assert_that(
    any(check_names %in% names(.data)),
    msg = "spcr_check_measure_data: A data frame for at least one element of `check_names` is required."
  )


  ### check for column names, and provide a helpful error message if needed
  required_columns <- c("ref", "measure_name", "comment")


  # reduce data down to only desired elements
  .data <- .data[names(.data) %in% check_names]

  # check required cols are present
  .data |>
    purrr::walk2(names(.data), spcr_check_for_required_columns,
                required_columns = required_columns) |>

    # convert ref column to character
    purrr::map(~ dplyr::mutate(., across(ref, as.character)))
}
