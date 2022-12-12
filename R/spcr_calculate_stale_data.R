#' Check whether data is stale
#'
#' @param updated_to_date date. The date of the final day the data relates to
#' @param lag integer. The number of days of update lag allowable before the data is stale
#' @param cutoff_dttm POSIXct. The datetime of the data cutoff, usually the end of the
#' week or month.
#'
#' @return logical. TRUE if the data is stale.
#' @noRd
#'
spcr_calculate_stale_data <- function(updated_to_date_text, lag, cutoff_dttm) {

  updated_to_date <- lubridate::as_date(
    updated_to_date_text, format = "%d-%b-%Y")

  assertthat::assert_that(
    inherits(updated_to_date, "Date"),
    msg = "spcr_calculate_stale_data: Unable to convert the updated_to argument text to a valid date."
  )

  assertthat::assert_that(
    all(lag %% 1 == 0),
    msg = "spcr_calculate_stale_data: The lag argument must be an integer."
  )

  assertthat::assert_that(
    inherits(cutoff_dttm, "POSIXct"),
    msg = "spcr_calculate_stale_data: The cutoff_dttm argument must be a POSIXct."
  )

  lag <- lubridate::days(lag) # convert to a period

  # adjust from 00:00:00 to 23:59:59
  (updated_to_date + lag + lubridate::hms("23:59:59")) < cutoff_dttm
}
