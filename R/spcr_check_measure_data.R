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

  # check for column names, and provide a helpful error message if needed
  required_columns <- c("ref", "measure_name", "comment")

  # check required cols are present
  if("week" %in% names(.data)) spcr_check_for_required_columns(.data[["week"]], "measure_data", required_columns)
  if("month" %in% names(.data)) spcr_check_for_required_columns(.data[["month"]], "measure_data", required_columns)

  # convert refs to character vectors
  if("week" %in% names(.data)) .data[["week"]]$ref <- as.character(.data[["week"]]$ref)
  if("month" %in% names(.data)) .data[["month"]]$ref <- as.character(.data[["month"]]$ref)

  return(.data)
}


#' Check that required columns are present in a user-supplied dataframe
#'
#' @param dtf dataframe. The dataframe to check
#' @param name_of_dataframe_being_checked character. The dataframe name to use in the error message
#' @param required_columns character. A vector of the required column names
#'
#' @return logical. TRUE if all are present, or error if not
#' @noRd
#'
spcr_check_for_required_columns <- function(dtf, name_of_dataframe_being_checked, required_columns){
  missing_columns <- required_columns[!required_columns %in% names(dtf)]

  if(length(missing_columns)){

    # find the name of the first missing col for the error message
    first_missing_column <- missing_columns[1]

    # throw the error
    stop(
      "spcr_check_for_required_columns: Column ",
      first_missing_column,
      " is missing from the ",
      name_of_dataframe_being_checked,
      ". Check for typos in the column names."
    )
  }

  return(TRUE)
}
