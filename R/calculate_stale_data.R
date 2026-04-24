#' Check whether data is stale
#'
#' @param updated_to date. The date of the final day the data relates to.
#'  Should be provided in "%d-%b-%Y" format
#' @param lag integer. The number of days of update lag allowable before the
#'  data is stale
#' @param cutoff_dttm POSIXct. The datetime of the data cutoff, usually the end
#'  of the week or month.
#'
#' @returns character: "stale" or "fresh"
#' @noRd
calculate_stale_data <- function(updated_to, lag, cutoff_dttm) {
  updated_to <- tryCatch(
    lubridate::dmy(updated_to),
    warning = \(w) "calculate_stale_data: The updated_to date is not in the required '%d-%b-%Y' format."
  )

  assertthat::assert_that(
    !any(is.na(updated_to)),
    all(inherits(updated_to, "Date")),
    msg = "calculate_stale_data: Unable to convert the updated_to argument text to a valid date."
  )

  assertthat::assert_that(
    all(lag %% 1 == 0),
    msg = "calculate_stale_data: The lag argument must be an integer."
  )

  assertthat::assert_that(
    all(inherits(cutoff_dttm, "POSIXct")),
    msg = "calculate_stale_data: The cutoff_dttm argument must be a POSIXct."
  )

  lag <- lubridate::days(lag) + lubridate::hms("23:59:59") # convert to a period
  if_else((updated_to + lag) < cutoff_dttm, "stale", "fresh")
}
