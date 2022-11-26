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

report_config <- tibble::tibble(
  ref = c("1", "2", "3", "1", "2", "3"),
  measure_name = c("M1", "M2", "M3", "M1", "M2", "M3"),
  domain = c("D1", "D1", "D1", "D2", "D2", "D2"),
  aggregation = c("week", "week", "week", "month", "month", "month")
)


"it returns true when all required data is present" |>
  test_that({
    expect_equal(
      spcr_check_dataset_is_complete(report_config, measure_data),
      TRUE
    )
  })

"it errors when insufficient data has been supplied" |>
  test_that({
    # remove the monthly data
    measure_data[["month"]] <- NULL

    expect_error(
      spcr_check_dataset_is_complete(report_config, measure_data),
      "spcr_check_dataset_is_complete: Data is missing for 3 report items. The first is ref 1, M1, monthly."
    )
  })
