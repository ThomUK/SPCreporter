
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

measure_config <- tibble::tibble(
  ref = c("1", "2", "3"),
  measure_name = c("M1", "M2", "M3"),
  data_source = c("S1", "S2", "S3"),
  data_owner = c("O1", "O2", "O3"),
  accountable_person = c("L1", "L2", "L3"),
  unit = c("Integer", "Decimal", "%"),
  improvement_direction = c("Neutral", "Increase", "Decrease"),
  target = c(NA, 10, 0.2),
  target_set_by = c(NA, "T2", "T3"),
  data_quality = c("RRRR", "AAAA", "GGGG"),
  baseline_period = c(12L, 12L, 12L),
  rebase_dates = c(NA, NA, NA),
  rebase_comment = c(NA, NA, NA)
)

report_config <- tibble::tibble(
  ref = c("1", "2", "3", "1", "2", "3"),
  measure_name = c("M1", "M2", "M3", "M1", "M2", "M3"),
  domain = c("D1", "D1", "D1", "D2", "D2", "D2"),
  aggregation = c("week", "week", "week", "month", "month", "month")
)

test_that("it returns a dataframe of the expected size", {
  r <- spcr_make_data_bundle(measure_data, report_config, measure_config)

  expect_equal(
    nrow(r),
    6
  )

  expect_equal(
    ncol(r),
    25
  )
})

test_that("it works when no targets are set", {

  # replace all targets with NA, as if not set
  measure_config$target <- NA

  r <- spcr_make_data_bundle(measure_data, report_config, measure_config)

  expect_equal(
    nrow(r),
    6
  )

  expect_equal(
    ncol(r),
    25
  )
})

"it accepts optional columns from measure_config" |>
test_that({

  # add optional columns
  measure_config$reviewed_at <- "ABC Meeting"
  measure_config$escalated_to <- "XYZ Meeting"

  r <- spcr_make_data_bundle(measure_data, report_config, measure_config)

  expect_equal(
    ncol(r),
    27
  )
  expect_equal(
    "Reviewed_At" %in% names(r),
    TRUE
  )
  expect_equal(
    "Escalated_To" %in% names(r),
    TRUE
  )

})
