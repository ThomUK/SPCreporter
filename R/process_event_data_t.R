#' Process event data into a time-between data frame
#'
#' @param event_data data frame. Raw event data
#'
#' @return data frame. A data frame with calculated dates and time-between information added
#' @noRd
process_event_data_t <- function(event_data){

  if(nrow(event_data) == 0) return(NULL)

  processed_data <- event_data |>
    dplyr::mutate(
      comment = NA, # remove for clarity (a comment against an event may not refer to the whole pivoted row)
      aggregation = "none"
    ) |>
    dplyr::group_by(ref) |>
    dplyr::arrange(event_date_or_datetime) |>
    dplyr::mutate(
      time_between = event_date_or_datetime - dplyr::lag(event_date_or_datetime),
      time_between = as.integer(time_between),
    ) |>
    dplyr::filter(!is.na(time_between)) |>
    dplyr::ungroup() |>
    dplyr::relocate(aggregation) |>
    dplyr::rename(
      date = event_date_or_datetime,
      value = time_between
    )
}
