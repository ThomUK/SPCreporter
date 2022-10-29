
report_config <- tibble::tibble(
  ref = c("1", "2", "3", "1", "2", "3"),
  measure_name = c("M1", "M2", "M3", "M1", "M2", "M3"),
  domain = c("D1", "D1", "D1", "D2", "D2", "D2"),
  aggregation = c("week", "week", "week", "month", "month", "month")
)

test_that("it coerces refs to character vectors", {
  # create the error by assigning numeric refs
  report_config$ref <- c(1, 2, 3, 1, 2, 3)

  r <- spcr_check_report_config(report_config)

  expect_equal(
    r$ref,
    c("1", "2", "3", "1", "2", "3")
  )
})
