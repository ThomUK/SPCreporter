#' Transform the data from wide to long format
#'
#' @param .data Data frame in wide format
#'
#' @returns Data frame in long format
#' @noRd
lengthen_measure_data <- function(.data) {
  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = "lengthen_measure_data: The data must be a data frame."
  )

  # Should match date strings of the form 2022-06-01
  ymd_regex <- "^20[0-9]{2}-[0-9]{1,2}-[0-9]{1,2}$"
  init_cols <- c("ref", "measure_name", "comment")

  assertthat::assert_that(
    all(purrr::map_lgl(
      names(.data), \(x) x %in% init_cols |
        stringr::str_detect(x, "^[0-9]{5}$") |
        stringr::str_detect(x, ymd_regex))),
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
            stringr::str_glue("^[0-9]{5}$|{ymd_regex}"), negate = TRUE),
          1),
        collapse = " ")))

  # pivot incoming measure_data from wide to long,
  # and convert date column to date format
  .data |>
    tidyr::pivot_longer(!any_of(init_cols), names_to = "date", values_drop_na = TRUE) |>
    dplyr::mutate(across("date", quietly_convert_date)) |>
    # Sort data from oldest to latest by measure - it should already be sorted
    # (pivot_longer draws from L-R wide data)... but let's make sure
    dplyr::arrange(across(all_of(c("ref", "date"))))
}





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
    # \u2264 is: ≤
    !is.na(target) & imp_dir == "decrease" ~ paste0("\u2264 ", string),
    # \u2265 is: ≥
    !is.na(target) & imp_dir == "increase" ~ paste0("\u2265 ", string),
    TRUE ~ string
  )
}




#' calculate the updated_to date string
#'
#' @param last_date date.
#' @param aggregation string. e.g. "month"
#'
#' @returns A date in "%d-%b-%Y" (day-month-year) format
#'
#' @noRd
get_updatedto_text <- function(last_date, aggregation) {
  dplyr::case_when(
    aggregation == "none" ~ lubridate::ceiling_date(last_date, "month") - 1,
    aggregation == "day" ~ lubridate::as_date(last_date),
    aggregation == "week" ~ lubridate::ceiling_date(last_date, "week", week_start = "Mon") - 1,
    aggregation == "month" ~ lubridate::ceiling_date(last_date, "month") - 1,
    TRUE ~ as.Date(NA_character_)) |>
    format("%d-%b-%Y")
}


# This function generates warnings due to the way if_else works with dates
# We will wrap it in a quietly adverb to handle the warnings, which are not
# warnings we need to worry about
convert_date <- function(x) {
  ymd_regex <- "^20[0-9]{2}-[0-9]{1,2}-[0-9]{1,2}$"
  if_else(
    grepl(ymd_regex, x),
    lubridate::ymd(x),
    lubridate::as_date(as.numeric(x), origin = "1899-12-30")
  )
}

quietly_convert_date <- function(...) {
  purrr::quietly(convert_date)(...) |>
    purrr::pluck("result")
}


#' Parse rebase dates
#' Parse dates from the config spreadsheet into a format suitable for use in
#' the SPC calculation function. Only needed as a helper function for
#' `align_rebase_dates()`
#'
#' @param input character. A vector of length 1, containing quoted dates in ymd
#' format, separated with commas eg '"2020-01-01", "2020-03-05"'
#'
#' @returns A vector of dates
#' @noRd
parse_rebase_dates <- function(input) {

  if (is.na(input)) NULL
  else {
    # parse into individual character strings
    vector <- input |>
      stringr::str_split_1("\\s*,\\s*") |>
      stringr::str_remove_all("\\\"") |> # remove internal quotes
      stringr::str_trim() # trim white space

    # wrap the date parsing in tryCatch() to stop()
    # if excel dates are not perfectly formed.
    tryCatch(
      lubridate::ymd(vector),
      error = function(c) stop("error in parse_rebase_dates: ", c),
      warning = function(c) stop(
        "parse_rebase_dates: rebase dates must be in 'YYYY-MM-DD' format."
      )
    )
  }
}



#' Align rebase date to match next data date after rebase, if does not already
#' match a date from the relevant data.
#' This is because plots were not showing rebase changes if the rebase date
#' did not match a date in the data for that measure.
#' https://github.com/ThomUK/SPCreporter/issues/35
#'
#' @inheritParams parse_rebase_dates
#' @param measure_data data frame containing a column of date values
#'
#' @returns a vector of dates, amended as necessary, or NULL if no dates were
#'  present initially
#' @noRd
align_rebase_dates <- function(input, measure_data) {

  dates <- parse_rebase_dates(input)

  pull_closest_date <- function(date, dates_list) {
    later_dates <- dates_list[dates_list >= date]
    if (length(later_dates)) min(later_dates) else date
  }

  if (is.null(dates)) NULL
  else dates |>
    purrr::map_dbl(pull_closest_date, dates_list = measure_data[["date"]]) |>
    lubridate::as_date()
}




#' Find the name of the assurance type
#'
#' @param spc data frame. As returned from the {NHSRplotthedots} SPC package
#' @param improvement_direction string. "Increase", "Decrease", or "Neutral"
#'
#' @returns string. Name of the assurance type
#' @noRd
get_assurance_type <- function(spc, improvement_direction) {
  imp_dir <- tolower(improvement_direction)
  upl <- spc[["upl"]][1]
  lpl <- spc[["lpl"]][1]
  target <- spc[["target"]][1]

  a <- dplyr::case_when(
    imp_dir == "neutral" ~ "Neutral",
    is.na(target) | is.na(lpl) | is.na(upl) ~ "No target",
    dplyr::between(target, lpl, upl) ~ "RND_TARG",
    lpl > target & imp_dir == "increase" ~ "PASS_TARG",
    upl < target & imp_dir == "decrease" ~ "PASS_TARG",
    lpl > target & imp_dir == "decrease" ~ "FAIL_TARG",
    upl < target & imp_dir == "increase" ~ "FAIL_TARG",
    TRUE ~ ""
  )

  if (a == "") {
    usethis::ui_stop(
      "get_assurance_type: Unable to determine SPC assurance type."
    )}
  a
}


#' Find the name of the variation type
#'
#' @param spc data frame. As returned from the {NHSRplotthedots} SPC package
#' @param improvement_direction string. "Increase", "Decrease", or "Neutral"
#'
#' @return string. Name of the variation type
#' @noRd
#'
get_variation_type <- function(spc, improvement_direction) {
  vari <- tail(spc[["point_type"]], 1)
  relative_to_mean <- tail(spc[["relative_to_mean"]], 1)
  # need to provide a default value so the case_when works
  if (is.null(relative_to_mean)) relative_to_mean <- 0
  imp_dir <- tolower(improvement_direction)

  v <- dplyr::case_when(
    vari == "common_cause" ~ "CC",
    vari == "special_cause_improvement" & imp_dir == "increase" ~ "SC_HI_IMP",
    vari == "special_cause_improvement" & imp_dir == "decrease" ~ "SC_LO_IMP",
    vari == "special_cause_concern" & imp_dir == "increase" ~ "SC_LO_CON",
    vari == "special_cause_concern" & imp_dir == "decrease" ~ "SC_HI_CON",
    vari == "special_cause_neutral" & relative_to_mean == -1 ~ "SC_LO_NEUTRAL",
    vari == "special_cause_neutral" & relative_to_mean %in% c(1, 0) ~ "SC_HI_NEUTRAL",
    vari == "special_cause_neutral_low" ~ "SC_LO_NEUTRAL",
    vari == "special_cause_neutral_high" ~ "SC_HI_NEUTRAL",
    TRUE ~ ""
  )

  if (v == "") {
    usethis::ui_stop(
      "get_variation_type: Unable to determine SPC variation type."
    )}
  v
}








#' Check whether data is stale
#'
#' @param updated_to date. The date of the final day the data relates to.
#'  Should be provided in "%d-%b-%Y" format
#' @param lag integer. The number of days of update lag allowable before the
#'  data is stale
#' @param cutoff_dttm POSIXct. The datetime of the data cutoff, usually the end
#'  of the week or month.
#'
#' @returns logical. TRUE if the data is stale.
#' @noRd
calculate_stale_data <- function(updated_to, lag, cutoff_dttm) {

  updated_to <- tryCatch(
    lubridate::dmy(updated_to),
    warning = \(w) "calculate_stale_data: The updated_to date is not in the required '%d-%b-%Y' format."
  )

  assertthat::assert_that(
    !is.na(updated_to),
    inherits(updated_to, "Date"),
    msg = "calculate_stale_data: Unable to convert the updated_to argument text to a valid date."
  )

  assertthat::assert_that(
    all(lag %% 1 == 0),
    msg = "calculate_stale_data: The lag argument must be an integer."
  )

  assertthat::assert_that(
    inherits(cutoff_dttm, "POSIXct"),
    msg = "calculate_stale_data: The cutoff_dttm argument must be a POSIXct."
  )

  lag <- lubridate::days(lag) # convert to a period

  # adjust from 00:00:00 to 23:59:59
  (updated_to + lag + lubridate::hms("23:59:59")) < cutoff_dttm
}
