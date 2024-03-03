#' Process event data into a time-between data frame
#'
#' @param event_data data frame. Raw event data
#' @param data_cutoff_dttm POSIXct. The data cutoff datetime used to calculate the final point position
#'
#' @return data frame. A data frame with calculated dates and time-between information added
#' @noRd
process_event_data_t <- function(event_data, data_cutoff_dttm){

  if(nrow(event_data) == 0) return(NULL)

  processed_data <- event_data |>
    dplyr::mutate(
      comment = NA, # remove for clarity (a comment against an event may not refer to the whole pivoted row)
      aggregation = "none"
    ) |>
    dplyr::filter(.data$event_date_or_datetime < data_cutoff_dttm) |> # remove events after the cutoff time (should only happen for retrospective reports)
    dplyr::group_by(.data$ref) |>
    dplyr::arrange(.data$event_date_or_datetime) |>

    # add the theoretical "today" event to each group
    dplyr::group_modify(~ tibble::add_row(.x, event_date_or_datetime = data_cutoff_dttm)) |>

    # calculate the time between events, in days
    dplyr::mutate(
      time_between = difftime(.data$event_date_or_datetime, dplyr::lag(.data$event_date_or_datetime), units = "days"),
      time_between = as.integer(.data$time_between),
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
