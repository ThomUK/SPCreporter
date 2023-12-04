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
    dplyr::group_by(ref) |>
    dplyr::arrange(event_date_or_datetime) |>

    # add the theoretical "today" event to each group
    dplyr::group_modify(~ tibble::add_row(.x, event_date_or_datetime = data_cutoff_dttm)) |>
    dplyr::mutate(
      time_between = event_date_or_datetime - dplyr::lag(event_date_or_datetime),
      time_between = as.integer(time_between),
      event_date_or_datetime = as.Date(event_date_or_datetime)
    ) |>
    dplyr::filter(!is.na(time_between)) |>
    dplyr::ungroup() |>

    # fill in the gaps left by adding the "today" event
    tidyr::fill(aggregation, measure_name) |>
    dplyr::relocate(aggregation) |>
    dplyr::rename(
      date = event_date_or_datetime,
      value = time_between
    )
}
