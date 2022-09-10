test_that("it has a happy path", {

  data_in <- tibble::tibble(
    ref = 1,
    measure_name = "a name",
    comment = "a comment",
    '2022-01-01' = 10,
    '2022-02-01' = 20
  )

  expected_out <- tibble::tibble(
    ref = c(1, 1),
    measure_name = c("a name", "a name"),
    date = as.Date(c("2022-01-01", "2022-02-01")),
    value = c(10, 20)
  )

  expect_equal(
    spcr_lengthen_measure_data(data_in),
    expected_out
  )

})

test_that("it only accepts a dataframe", {

  expect_error(
    spcr_lengthen_measure_data("not a dataframe"),
    "spcr_lengthen_measure_data: The data must be a dataframe."
  )

})
