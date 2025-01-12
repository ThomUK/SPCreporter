#' Process event data into a time-between data frame
#'
#' @param event_data data frame. Raw event data
#' @param data_cutoff_dttm POSIXct. The data cutoff datetime used to calculate
#'  the final point position
#'
#' @returns A data frame with calculated dates and time-between information
#' @noRd
process_event_data_t <- function(event_data, data_cutoff_dttm) {

  if (nrow(event_data) == 0) return(NULL)

  event_data |>
    dplyr::mutate(aggregation = "none") |>
    # Remove events after the cutoff time (should only happen for
    # retrospective reports).
    dplyr::filter(
      if_any("event_date_or_datetime", \(x) x <= data_cutoff_dttm)
    ) |>
    dplyr::group_by(pick("ref")) |>
    dplyr::arrange(pick("event_date_or_datetime")) |>

    # add the theoretical "today" event to each group
    dplyr::group_modify(\(x, y) {
      tibble::add_row(x, event_date_or_datetime = data_cutoff_dttm)
    }) |>

    # calculate the time between events, in days
    dplyr::mutate(
      time_between = as.integer(difftime(
        .data[["event_date_or_datetime"]],
        dplyr::lag(.data[["event_date_or_datetime"]]),
        units = "days"
      ))
    ) |>
    dplyr::filter(!is.na(.data$time_between)) |>
    dplyr::ungroup() |>

    # fill in the gaps left by adding the "today" event
    tidyr::fill("aggregation", "measure_name") |>
    dplyr::relocate("aggregation") |>
    dplyr::rename(
      date = "event_date_or_datetime",
      value = "time_between"
    )
}
