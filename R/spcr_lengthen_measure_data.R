#' Transform the data from wide to long format
#'
#' @param .data data frame. Data in wide format.
#' @param frequency string. Typically "weekly" or "monthly"
#'
#' @returns Data frame in long format
#' @noRd
#'
spcr_lengthen_measure_data <- function(.data, frequency) {
  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = "spcr_lengthen_measure_data: The data must be a data frame."
  )

  assertthat::assert_that(
    is.character(frequency),
    msg = usethis::ui_oops(
    "spcr_lengthen_measure_data: frequency variable (taken from measure_data
    list name) is not a string."))


  ymd_regex <- "^20[0-9]{2}-[:alnum:]{1,2}-[0-9]{1,2}$"


  assertthat::assert_that(
    all(purrr::map_lgl(names(.data),
                       function(x) {
                         x %in% c("ref", "measure_name", "comment") |
                           stringr::str_detect(x, "^[0-9]{5}$") |
                           stringr::str_detect(x, ymd_regex)
                       })
    ),
    msg = "spcr_lengthen_measure_data: The data contains invalid column names.")


  convert_date <- function(x) {
    if (grepl("^[0-9]{5}$", x)) {
      x |>
        as.numeric() |>
        lubridate::as_date(origin = "1899-12-30")
    } else lubridate::ymd(x)
  }


  # pivot incoming measure_data from wide to long,
  # and convert date column to date format
  .data |>
    tidyr::pivot_longer(
      cols = !any_of(c("ref", "measure_name", "comment")),
      names_to = "date"
    ) |>
    dplyr::select(!comment) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(frequency = frequency) |>
    # need to do one row at a time otherwise grepl() throws an error
    # case_when() would be better, but throws warnings:
    # https://community.rstudio.com/t/why-am-i-getting-warnings-in-my-case-when-process/154418
    dplyr::rowwise() |>
    dplyr::mutate(across(date, convert_date)) |>
    dplyr::ungroup()
}
