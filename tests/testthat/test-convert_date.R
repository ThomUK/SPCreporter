# convert_date uses dplyr::if_else, which evaluates both branches for all
# elements. This means warnings are always generated — quietly_convert_date
# wraps it with purrr::quietly to suppress them. Tests below reflect this.

test_that("YMD string is converted to the correct date", {
  expect_equal(
    suppressWarnings(convert_date("2024-03-15")),
    as.Date("2024-03-15")
  )
})

test_that("Excel numeric date (as character) is converted correctly", {
  # Excel stores dates as days since 1899-12-30
  excel_num <- as.character(as.numeric(as.Date("2024-03-15") - as.Date("1899-12-30")))
  expect_equal(
    suppressWarnings(convert_date(excel_num)),
    as.Date("2024-03-15")
  )
})

test_that("convert_date generates warnings due to if_else evaluating both branches", {
  expect_warning(convert_date("2024-03-15"))
})

test_that("quietly_convert_date returns the correct date without warnings", {
  expect_no_warning(quietly_convert_date("2024-03-15"))
  expect_equal(quietly_convert_date("2024-03-15"), as.Date("2024-03-15"))
})

test_that("quietly_convert_date handles a vector of YMD strings", {
  expect_equal(
    quietly_convert_date(c("2024-01-01", "2024-06-15", "2023-12-31")),
    as.Date(c("2024-01-01", "2024-06-15", "2023-12-31"))
  )
})

test_that("quietly_convert_date handles a mixed YMD and Excel numeric vector", {
  excel_num <- as.character(as.numeric(as.Date("2024-06-15") - as.Date("1899-12-30")))
  mixed <- c("2024-01-01", excel_num)

  expect_no_warning(quietly_convert_date(mixed))
  expect_equal(
    quietly_convert_date(mixed),
    as.Date(c("2024-01-01", "2024-06-15"))
  )
})
