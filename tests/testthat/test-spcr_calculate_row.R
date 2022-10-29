
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

# process info to get it into the same shape as spcr_make_data_bundle does
measure_data_long <- spcr_check_measure_data(measure_data) %>%
  purrr::map2_df(.y = names(measure_data), .f = spcr_lengthen_measure_data)

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

# test_that("it returns a dataframe of the expected size", {
#
#   r <- spcr_make_data_bundle(measure_data, measure_config, report_config)
#
#   expect_equal(
#     nrow(r),
#     6
#   )
#
#   expect_equal(
#     ncol(r),
#     23
#   )
#
# })

test_that("it throws a warning if any measure is labelled as integer but has decimals supplied in the data.", {
  # create the decimals for measure 1, which is an integer in the measure_config
  measure_data_decimals <- measure_data

  # process info to get it into the same shape as spcr_make_data_bundle does
  measure_data_decimals <- spcr_check_measure_data(measure_data_decimals) %>%
    purrr::map2_df(.y = names(measure_data_decimals), .f = spcr_lengthen_measure_data) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        measure_name == "M1" ~ value + 0.5,
        TRUE ~ value
      )
    )

  expect_warning(
    spcr_calculate_row("1", "week", measure_data_decimals, measure_config, report_config),
    regexp = "spcr_calculate_row: Measure 1 is configured as an integer, but has been supplied with decimal data."
  )
})

test_that("it returns the first date correctly", {
  expect_equal(
    spcr_calculate_row("1", "week", measure_data_long, measure_config, report_config)$First_Date,
    as.Date("2020-01-06")
  )

  expect_equal(
    spcr_calculate_row("1", "month", measure_data_long, measure_config, report_config)$First_Date,
    as.Date("2020-01-01")
  )
})

test_that("it returns the last date correctly", {
  expect_equal(
    spcr_calculate_row("1", "week", measure_data_long, measure_config, report_config)$Last_Date,
    as.Date("2020-03-23")
  )

  expect_equal(
    spcr_calculate_row("1", "month", measure_data_long, measure_config, report_config)$Last_Date,
    as.Date("2020-12-01")
  )
})

test_that("it returns the 'updated to' string correctly", {
  expect_equal(
    spcr_calculate_row("1", "week", measure_data_long, measure_config, report_config)$Updated_To,
    "29-Mar-2020"
  )

  expect_equal(
    spcr_calculate_row("1", "month", measure_data_long, measure_config, report_config)$Updated_To,
    "31-Dec-2020"
  )
})
