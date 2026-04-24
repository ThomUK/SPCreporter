test_that("it has a happy path", {

  expect_equal(
    parse_rebase_dates("2020-01-01"),
    as.Date("2020-01-01")
  )

  expect_equal(
    parse_rebase_dates('"2020-01-01", "2020-02-01"'),
    as.Date(c("2020-01-01", "2020-02-01"))
  )
})

test_that("invalid dates cause an error", {

  expect_error(
    parse_rebase_dates("01-01-2020"),
    "parse_rebase_dates: rebase dates must be in 'YYYY-MM-DD' format."
  )

  expect_error(
    parse_rebase_dates('"2020-01-01", "01-05-2020"'),
    "parse_rebase_dates: rebase dates must be in 'YYYY-MM-DD' format."
  )
})

test_that("NULL input causes an error", {
  expect_error(parse_rebase_dates(NULL))
})

test_that("empty string returns NA (lubridate::ymd silently returns NA for empty input)", {
  expect_equal(parse_rebase_dates(""), as.Date(NA))
})

test_that("a single quoted date (no comma) is parsed correctly", {
  expect_equal(
    parse_rebase_dates('"2020-01-01"'),
    as.Date("2020-01-01")
  )
})
