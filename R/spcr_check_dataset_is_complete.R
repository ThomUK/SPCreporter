#' Check all required data items are provided
#'
#' @param report_config dataframe. The report config detailing required report items
#' @param measure_data list. The data items to check
#'
#' @return logical
#' @export
#'
spcr_check_dataset_is_complete <- function(report_config, measure_data) {

  required_data <- report_config |>
    dplyr::distinct(ref, measure_name, aggregation)

  supplied_data <- measure_data |>
    dplyr::bind_rows(.id = "aggregation") |>
    dplyr::select(ref, measure_name, aggregation)

  missing <- dplyr::setdiff(
    required_data[, c("ref", "aggregation")], # don't use the measure name for the missing check (it might not match and is checked later in another function
    supplied_data[, c("ref", "aggregation")], # don't use the measure name for the missing check (it might not match and is checked later in another function
    ) |>
    dplyr::left_join(required_data, by = "ref") |> # join to get the measure name back for the error message
    dplyr::select( #
      ref,
      measure_name,
      aggregation = aggregation.x,
    ) |>
    unique() # remove duplicate left_join rows

  # build an error message if there are missing data items
  assertthat::assert_that(
    nrow(missing) == 0,
    msg = paste0(
      "spcr_check_dataset_is_complete: Data is missing for ",
      nrow(missing),
      " report items. The first is ref ",
      missing[1,][["ref"]],
      ", ",
      missing[1,][["measure_name"]],
      ", ",
      missing[1,][["aggregation"]],
      "ly."
    )
  )

  return(TRUE)

}
