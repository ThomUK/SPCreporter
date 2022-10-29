test_that("it errors if the data is not a list", {
  expect_error(
    spcr_check_measure_data("not a list"),
    "spcr_check_measure_data: The data must be a list."
  )
})

test_that("it contains only allowed items", {
  expect_error(
    spcr_check_measure_data(list("Once in a blue moon" = 1)),
    "spcr_check_measure_data: The list items must be from 'week' or 'month'."
  )
})

measure_data <- list(
  week = tibble::tibble(
    ref = c("1", "2", "3"),
    measure_name = c("M1", "M2", "M3"),
    comment = c("comment", "comment", "comment"),
    `43836` = c(1, 3.2, 0.5),
    `43843` = c(2, 4.2, 0.6),
    `43850` = c(1, 3.2, 0.5),
    `43857` = c(2, 4.2, 0.6),
    `43864` = c(1, 3.2, 0.5),
    `43871` = c(2, 4.2, 0.6),
    `43878` = c(1, 3.2, 0.5),
    `43885` = c(2, 4.2, 0.6),
    `43892` = c(1, 3.2, 0.5),
    `43899` = c(2, 4.2, 0.6),
    `43906` = c(1, 3.2, 0.5),
    `43913` = c(2, 4.2, 0.6)
  ),
  month = tibble::tibble(
    ref = c("1", "2", "3"),
    measure_name = c("M1", "M2", "M3"),
    comment = c("comment", "comment", "comment"),
    `43831` = c(1, 3.2, 0.5),
    `43862` = c(2, 4.2, 0.6),
    `43891` = c(1, 3.2, 0.5),
    `43922` = c(2, 4.2, 0.6),
    `43952` = c(1, 3.2, 0.5),
    `43983` = c(2, 4.2, 0.6),
    `44013` = c(1, 3.2, 0.5),
    `44044` = c(2, 4.2, 0.6),
    `44075` = c(1, 3.2, 0.5),
    `44105` = c(2, 4.2, 0.6),
    `44136` = c(1, 3.2, 0.5),
    `44166` = c(2, 4.2, 0.6)
  )
)

test_that("it coerces refs to character vectors", {
  # create the error by assigning numeric refs
  measure_data[["week"]]$ref <- c(1, 2, 3)
  measure_data[["month"]]$ref <- c(1, 2, 3)


  r <- spcr_check_measure_data(measure_data)

  expect_equal(
    r[["week"]]$ref,
    c("1", "2", "3")
  )
})
