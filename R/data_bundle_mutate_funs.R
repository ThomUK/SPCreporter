#' Get the character representation of the target
#'
#' @param target string/numeric. The target (probably a numeric)
#' @param improvement_direction string. One of "increase", "decrease", or "neutral"
#' @param unit string. One of "integer", "decimal", or "%"
#'
#' @return A character string suitable for inclusion in the report
#' @noRd
#'
spcr_get_target_text <- function(target, improvement_direction, unit) {

  imp_dir <- tolower(improvement_direction)

  string <- dplyr::case_when(
    is.na(target) ~ "-",
    imp_dir == "neutral" ~ "Neutral",
    unit == "%" ~ paste0(round(target * 100, 1), "%"),
    TRUE ~ as.character(round(target, 2)) # covers decimal and integer
  )

  dplyr::case_when(
    imp_dir == "decrease" ~ paste0("\u2264 ", string), # \u2264 is: ≤
    imp_dir == "increase" ~ paste0("\u2265 ", string), # \u2265 is: ≥
    TRUE ~ string
  )
}




#' calculate the updated_to date string
#'
#' @param last_date date.
#' @param aggregation string. e.g. "month"
#'
#' @noRd
#'
spcr_get_updatedto_text <- function(last_date, aggregation) {
  if (aggregation %in% c("week", "month")) {
    format.Date((lubridate::ceiling_date(
      last_date,
      unit = aggregation,
      week_start = 1) - lubridate::days(1)), "%d-%b-%Y")
  } else NA
}
