#' Check that required columns are present in a user-supplied data frame
#'
#' @param .data A data frame. The data frame to check
#' @param df_name character. A data frame name to use in the error message
#' @param required_columns character. A vector of the required column names
#'
#' @returns the original dataframe, if required columns are present, or an error
#'   message if not
#' @noRd
#'
spcr_check_for_required_columns <- function(.data, df_name, required_columns) {

  missing_columns <- setdiff(required_columns, names(.data))

  if (length(missing_columns)) {
    # find the name of the first missing col for the error message
    first_missing_column <- missing_columns[1]

    # throw the error
    usethis::ui_stop(paste0(
      "spcr_check_for_required_columns: Column '",
      first_missing_column,
      "' is missing from the '",
      df_name,
      "' data frame. Check for typos in the column names."
    ))
  } else .data
}


#' Certain variables are optional in measure_config. If supplied, we want to
#' keep them, but if not supplied we want to add them with contents = `NA`.
#'
#' @param optional_columns character. A vector of optional column names
#'
#' @returns the original data frame, plus any optional columns that were missing
#' @noRd
#'
spcr_check_for_optional_columns <- function(.data, optional_columns) {

  missing_columns <- setdiff(optional_columns, names(.data))

  if (length(missing_columns)) {
    .data |>
      dplyr::bind_cols(
        purrr::map_dfc(missing_columns, ~ tibble::tibble(.x = NA)) |>
          purrr::set_names(missing_columns))
    } else .data
}
