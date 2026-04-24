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
