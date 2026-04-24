#' Get the character representation of the target
#'
#' @param target string/numeric. The target (probably a numeric)
#' @param improvement_direction string. One of "increase", "decrease", or
#'  "neutral"
#' @param unit string. One of "integer", "decimal", or "%"
#'
#' @returns A character string suitable for inclusion in the report
#' @noRd
get_target_text <- function(target, improvement_direction, unit) {
  imp_dir <- tolower(improvement_direction)

  string <- dplyr::case_when(
    is.na(target) ~ "-",
    imp_dir == "neutral" ~ "Neutral",
    unit == "%" ~ paste0(round(target * 100, 1), "%"),
    TRUE ~ as.character(round(target, 2)) # covers decimal and integer
  )

  dplyr::case_when(
    target == 0 & imp_dir == "decrease" ~ string,
    target == 1 & unit == "%" & imp_dir == "increase" ~ string,
    !is.na(target) & imp_dir == "decrease" ~ paste0("\u2264 ", string),
    !is.na(target) & imp_dir == "increase" ~ paste0("\u2265 ", string),
    TRUE ~ string
  )
}


#' Calculate the updated_to date string
#'
#' The `aggregation` parameter is derived from the report config, and should
#' never be blank (NA).
#'
#' @param last_date date.
#' @param aggregation string. e.g. "month"
#'
#' @returns A date in "%d-%b-%Y" (day-month-year) format
#'
#' @noRd
get_updatedto_text <- function(last_date, aggregation) {
  assert_that(
    length(last_date) == 1L,
    msg = "get_updatedto_text: Multiple values for `last_date` provided"
  )
  assert_that(
    length(aggregation) == 1L,
    msg = "get_updatedto_text: Multiple values for `aggregation` provided"
  )

  last_date <- as.Date(last_date) # handles dttm being passed in by mistake

  # Rename "calendar_year" and "none" aggregations to work with ceiling_date()
  agg <- dplyr::case_when(
    aggregation == "calendar_year" ~ "year",
    # aggregation == "financial_year" ~ "3 months", # TODO
    aggregation == "none" ~ "month",
    .default = aggregation
  )

  # allowed values
  assert_that(
    all(agg %in% c("day", "week", "month", "year")),
    msg = glue("get_updatedto_text: invalid aggregation ({agg}) provided")
  )

  # Set start day for week to Monday (1)
  withr::with_options(list(lubridate.week.start = 1), {
    dplyr::case_when(
      # For day aggregation use the day itself
      agg == "day" ~ last_date,
      # For all other levels, use a ceiling_date approach to get the end day of
      # the current period (week, month etc). Event data (agg = "none") is
      # rounded to the month boundary.
      .default = lubridate::ceiling_date(last_date, agg) - days(1),
    ) |>
      format("%d-%b-%Y")
  })
}
