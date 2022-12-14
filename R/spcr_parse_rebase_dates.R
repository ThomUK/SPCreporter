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

#' Align rebase date to match next data date after rebase, if does not already
#' match a date from the relevant data.
#' This is because plots were not showing rebase changes if the rebase date
#' did not match a date in the data for that measure.
#' https://github.com/ThomUK/SPCreporter/issues/35
#'
#' @inheritParams spcr_parse_rebase_dates
#' @param measure_data data frame containing a column of date values
#'
#' @returns a vector of dates, amended as necessary, or NULL if no dates were present initially
#' @noRd
spcr_align_rebase_dates <- function(input, measure_data) {

  dates <- spcr_parse_rebase_dates(input)

  pull_closest_date <- function(date, dates_list) {
    later_dates <- dates_list[dates_list >= date]
    if (length(later_dates)) min(later_dates) else date
  }

  if (is.null(dates)) NULL
  else dates |>
    purrr::map_dbl(pull_closest_date, dates_list = measure_data$date) |>
    lubridate::as_date()
}
