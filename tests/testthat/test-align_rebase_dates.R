make_measure_data <- function(dates) {
  tibble::tibble(date = lubridate::ymd(dates))
}

test_that("align_rebase_dates: rebase date that exactly matches a data date is unchanged", {
  md <- make_measure_data(c("2022-01-01", "2022-02-01", "2022-03-01"))
  expect_equal(
    align_rebase_dates("2022-02-01", md),
    as.Date("2022-02-01")
  )
})


test_that("align_rebase_dates: rebase date between data dates is rounded up to next data date", {
  md <- make_measure_data(c("2022-01-01", "2022-02-01", "2022-03-01"))
  # 2022-01-15 falls between Jan and Feb data points -> rounds up to Feb
  expect_equal(
    align_rebase_dates("2022-01-15", md),
    as.Date("2022-02-01")
  )
})


test_that("align_rebase_dates: rebase date after all data dates is returned as-is", {
  md <- make_measure_data(c("2022-01-01", "2022-02-01", "2022-03-01"))
  # No data date >= 2022-06-01, so the rebase date is returned unchanged
  expect_equal(
    align_rebase_dates("2022-06-01", md),
    as.Date("2022-06-01")
  )
})


test_that("align_rebase_dates: multiple rebase dates are each aligned independently", {
  md <- make_measure_data(paste0("2022-0", 1:6, "-01"))
  # "2022-03-17" -> 2022-04-01 (next data date)
  # "2022-05-01" -> 2022-05-01 (exact match)
  # "2022-06-02" -> 2022-06-02 (after all data, returned as-is)
  expect_equal(
    align_rebase_dates('"2022-03-17", "2022-05-01", "2022-06-02"', md),
    as.Date(c("2022-04-01", "2022-05-01", "2022-06-02"))
  )
})


test_that("align_rebase_dates: NA input returns an empty or NA result", {
  md <- make_measure_data(c("2022-01-01", "2022-02-01"))
  result <- align_rebase_dates(NA_character_, md)
  # parse_rebase_dates(NA) returns NULL; map_vec over NULL gives empty/NA
  expect_true(length(result) == 0 || all(is.na(result)))
})


test_that("align_rebase_dates: rebase date before all data dates rounds up to first data date", {
  md <- make_measure_data(c("2022-01-01", "2022-02-01", "2022-03-01"))
  # 2021-06-01 is before all data -> rounds up to the earliest data date
  expect_equal(
    align_rebase_dates("2021-06-01", md),
    as.Date("2022-01-01")
  )
})


test_that("align_rebase_dates: empty measure_data returns rebase date unchanged", {
  md <- tibble::tibble(date = as.Date(character(0)))
  expect_equal(
    align_rebase_dates("2022-03-15", md),
    as.Date("2022-03-15")
  )
})
