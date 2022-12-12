#' Parse rebase dates
#' Parse dates from the config spreadsheet into a format suitable for use in
#' the SPC calculation function.
#'
#' @param input character.  A vector of length 1, containing quoted dates in ymd format, separated with commas
#' eg '"2020-01-01", "2020-03-05"'
#'
#' @return A vector of dates
#' @noRd
spcr_parse_rebase_dates <- function(input) {

  if (is.na(input)) NULL

  else {
    # parse into individual character strings
    vector <- input |>
      stringr::str_split_1("\\s*,\\s*") |>
      stringr::str_remove_all("\\\"") |> # replace internal quotes
      stringr::str_trim() # trim white space

    # wrap the date parsing in tryCatch() to stop()
    # if excel dates are not perfectly formed.
    tryCatch(
      lubridate::ymd(vector),
      error = function(c) stop("error in spcr_parse_rebase_dates: ", c),
      warning = function(c) stop("spcr_parse_rebase_dates: rebase dates must be in 'YYYY-MM-DD' format.")
    )
  }
}
