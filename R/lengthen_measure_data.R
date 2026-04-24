#' Transform aggregated data from wide to long format
#'
#' @param .data data frame. Data frame in wide format
#'
#' @returns data frame. Data frame in long format
#' @noRd
lengthen_measure_data <- function(.data) {
  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = "lengthen_measure_data: The data must be a data frame."
  )

  # Should match date strings of the form 2022-06-01
  ymd_regex <- "^20[0-9]{2}-[0-9]{1,2}-[0-9]{1,2}$"
  init_cols <- c("aggregation", "measure_prefix", "ref", "measure_name", "comment")

  assertthat::assert_that(
    all(purrr::map_lgl(
      names(.data), \(x) x %in% init_cols |
        stringr::str_detect(x, "^[0-9]{5}$") |
        stringr::str_detect(x, ymd_regex)
    )),
    msg = usethis::ui_stop(
      paste(
        "lengthen_measure_data: The measure_data supplied contains",
        "invalid column headings. The only column headings allowed are",
        stringr::str_flatten_comma(paste0("'", init_cols, "'")),
        "and valid date formats.",
        "One invalid column name found is:",
        head(
          stringr::str_subset(
            setdiff(names(.data), init_cols),
            stringr::str_glue("^[0-9]{5}$|{ymd_regex}"),
            negate = TRUE
          ),
          1
        ),
        collapse = " "
      )
    )
  )

  # pivot incoming measure_data from wide to long,
  # and convert date column to date format
  .data |>
    tidyr::pivot_longer(!any_of(init_cols), names_to = "date", values_drop_na = TRUE) |>
    dplyr::mutate(across("date", quietly_convert_date)) |>
    # Sort data from oldest to latest by measure - it should already be sorted
    # (pivot_longer draws from L-R wide data)... but let's make sure
    dplyr::arrange(across(all_of(c("ref", "date"))))
}
