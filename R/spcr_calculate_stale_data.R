#' Check whether data is stale
#'
#' @param updated_to_date date. The date of the final day the data relates to
#' @param lag integer. The number of days of update lag acceptable before the data is stale
#' @param cutoff_dttm POSIXct. The datetime of the data cutoff, usually the end of the
#' week or month.
#'
#' @return logical. TRUE if the data is stale.
#' @noRd
#'
spcr_calculate_stale_data <- function(updated_to_date, lag, cutoff_dttm){

  assertthat::assert_that(
    inherits(updated_to_date, "Date"),
    msg = "spcr_calculate_stale_data: The updated_to argument must be a date."
  )

  assertthat::assert_that(
    all(lag %% 1 == 0),
    msg = "spcr_calculate_stale_data: The lag argument must be an integer."
  )

  assertthat::assert_that(
    inherits(cutoff_dttm, "POSIXct"),
    msg = "spcr_calculate_stale_data: The cutoff_dttm argument must be a POSIXct."
  )

  # adjust from 00:00:00 to 23:59:59
  adjust_23hrs_59mins_59s <- lubridate::hours(23) +
    lubridate::minutes(59) +
    lubridate::seconds(59)

  lag <- lubridate::days(lag) # conver to a period

  dplyr::if_else(updated_to_date + adjust_23hrs_59mins_59s + lag < cutoff_dttm, TRUE, FALSE)

}
