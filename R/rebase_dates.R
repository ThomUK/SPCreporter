#' Parse rebase dates
#' Parse dates from the config spreadsheet into a format suitable for use in
#' the SPC calculation function. Only needed as a helper function for
#' `align_rebase_dates()`
#'
#' @param input character. A vector of length 1, containing quoted dates in ymd
#' format, separated with commas eg '"2020-01-01", "2020-03-05"'
#'
#' @returns A vector of dates
#' @noRd
parse_rebase_dates <- function(input) {
  if (is.na(input)) {
    NULL
  } else {
    # parse into individual character strings
    vector <- input |>
      stringr::str_split_1("\\s*,\\s*") |>
      stringr::str_remove_all("\\\"") |> # remove internal quotes
      stringr::str_trim() # trim white space

    # wrap the date parsing in tryCatch() to stop()
    # if excel dates are not perfectly formed.
    tryCatch(
      lubridate::ymd(vector),
      error = function(c) stop("error in parse_rebase_dates: ", c),
      warning = function(c) {
        stop(
          "parse_rebase_dates: rebase dates must be in 'YYYY-MM-DD' format."
        )
      }
    )
  }
}


#' Align rebase date to match next data date after rebase, if does not already
#' match a date from the relevant data.
#' This is because plots were not showing rebase changes if the rebase date
#' did not match a date in the data for that measure.
#' https://github.com/ThomUK/SPCreporter/issues/35
#'
#' @inheritParams parse_rebase_dates
#' @param measure_data data frame containing a column of date values
#'
#' @returns a vector of dates, amended as necessary, or NA if no dates were
#'  present initially
#' @noRd
align_rebase_dates <- function(input, measure_data) {
  dates <- parse_rebase_dates(input)
  dates_vec <- as.Date(measure_data[["date"]])

  # "Round up" a rebase date to match the earliest date in the measure data that
  # is equal to or greater than the rebase date.
  pull_closest_date <- function(date, dates_list = dates_vec) {
    if (is.null(date)) {
      NA
    } else {
      later_dates <- dates_list[dates_list >= date]
      if (length(later_dates)) min(later_dates) else date
    }
  }

  dates |>
    purrr::map_vec(pull_closest_date)
}
