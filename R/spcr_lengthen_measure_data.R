#' Transform the data from wide to long format
#'
#' @param .data Data frame in wide format
#'
#' @returns Data frame in long format
#' @noRd
#'
spcr_lengthen_measure_data <- function(.data) {
  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = "spcr_lengthen_measure_data: The data must be a data frame."
  )

  # should match date strings of the form 2022-06-01
  ymd_regex <- "^20[0-9]{2}-[:alnum:]{1,2}-[0-9]{1,2}$"
  init_cols <- c("ref", "measure_name", "comment")

  assertthat::assert_that(
    all(purrr::map_lgl(names(.data),
                       function(x) {
                         x %in% init_cols |
                           stringr::str_detect(x, "^[0-9]{5}$") |
                           stringr::str_detect(x, ymd_regex)
                       })
    ),
    msg = usethis::ui_stop(
      paste(
        "spcr_lengthen_measure_data: The measure_data supplied contains",
        "invalid column headings. The only column headings allowed are",
        stringr::str_flatten_comma(paste0("'", init_cols, "'")),
        "and valid date formats.",
        "One invalid column name found is:",
        head(
            stringr::str_subset(
              setdiff(names(.data), init_cols),
            stringr::str_glue("^[0-9]{5}$|{ymd_regex}"), negate = TRUE),
          1),
        collapse = " ")
      )
    )


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
      cols = !any_of(init_cols),
      names_to = "date"
    ) |>

    dplyr::filter(!is.na(value)) |>

    # need to do this rowwise otherwise grepl() throws an error
    # case_when() would be better, but lubridate throws warnings:
    # https://community.rstudio.com/t/why-am-i-getting-warnings-in-my-case-when-process/154418
    dplyr::rowwise() |>
    dplyr::mutate(across(date, convert_date)) |>
    # why can't we do `all_of(init_cols)` in group_by yet?
    dplyr::group_by(!!!rlang::syms(init_cols)) |>

    # try to ensure that data is sorted from oldest to latest -
    # it should already be (pivot_longer draws from L-R wide data)
    # but let's try to make sure
    dplyr::group_modify(~ dplyr::arrange(., date)) |>
    dplyr::ungroup()
}
