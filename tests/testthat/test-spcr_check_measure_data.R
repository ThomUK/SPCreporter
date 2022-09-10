test_that("it errors if the data is not a list", {

  expect_error(
    spcr_check_measure_data("not a list"),
    "spcr_check_measure_data: The data must be a list."
  )
})

test_that("it contains only allowed items", {

  expect_error(
    spcr_check_measure_data(list("Once in a blue moon" = 1)),
    "spcr_check_measure_data: The list items must be from 'Weekly' or 'Monthly'."
  )
})

test_that("it returns the item passed in", {

  expect_equal(
    spcr_check_measure_data(list("Monthly" = 1)),
    list("Monthly" = 1)
  )
})
