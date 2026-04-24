test_that("day aggregation returns the date itself", {
  expect_equal(
    get_updatedto_text(as.Date("2024-03-15"), "day"),
    "15-Mar-2024"
  )
})

test_that("week aggregation returns the last day of the week (Sunday)", {
  # 2024-03-11 is a Monday; week ends Sunday 2024-03-17
  expect_equal(
    get_updatedto_text(as.Date("2024-03-11"), "week"),
    "17-Mar-2024"
  )
})

test_that("month aggregation returns the last day of the month", {
  expect_equal(
    get_updatedto_text(as.Date("2024-03-15"), "month"),
    "31-Mar-2024"
  )
})

test_that("calendar_year aggregation returns the last day of the year", {
  expect_equal(
    get_updatedto_text(as.Date("2024-06-01"), "calendar_year"),
    "31-Dec-2024"
  )
})

test_that("none aggregation is treated as month", {
  expect_equal(
    get_updatedto_text(as.Date("2024-03-15"), "none"),
    "31-Mar-2024"
  )
})

test_that("datetime input is coerced to date", {
  expect_equal(
    get_updatedto_text(as.POSIXct("2024-03-15 12:34:56"), "month"),
    "31-Mar-2024"
  )
})

test_that("multiple last_date values error", {
  expect_error(
    get_updatedto_text(as.Date(c("2024-03-15", "2024-04-15")), "month"),
    "Multiple values"
  )
})

test_that("multiple aggregation values error", {
  expect_error(
    get_updatedto_text(as.Date("2024-03-15"), c("month", "week")),
    "Multiple values"
  )
})

test_that("invalid aggregation errors", {
  expect_error(
    get_updatedto_text(as.Date("2024-03-15"), "quarter"),
    "invalid aggregation"
  )
})
