#' (internal function) Transform the data from wide to long format
#'
#' @param .data Dataframe.  Data in wide format.
#' @param frequency String.  Typically "weekly" or "monthly"
#'
#' @return Dataframe in long format
#' @noRd
#'
spcr_lengthen_measure_data <- function(.data, frequency){

  assertthat::assert_that(
    "data.frame" %in% class(.data),
    msg = "spcr_lengthen_measure_data: The data must be a dataframe."
  )

  # pivot incoming measure_data from wide to long
  long_data <- .data %>% tidyr::pivot_longer(
    -c(.data$ref, .data$measure_name, .data$comment), names_to = "date", values_to = "value") %>%
    dplyr::mutate(frequency = frequency) %>%
    dplyr::select(-.data$comment) %>%
    dplyr::filter(!is.na(.data$value))

  # handle varying date column heading formats
  suppressWarnings(
    long_data <- long_data %>%
      dplyr::mutate(
        date = dplyr::case_when(

          # when the date character string contains a "-"
          grepl("-", date) == TRUE ~ as.Date(date, format = "%Y-%m-%d"),

          # otherwise, its an excel character string that needs an origin
          TRUE ~ as.Date(as.numeric(date),  origin = "1899-12-30")

        )
      )
  )

}
