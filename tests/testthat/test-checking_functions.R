
test_that("check_dataset_is_complete: happy path", {

  # this function is called when before the aggregation is manually changed from
  # "events" to "none", so we need to create the renaming here
  measure_data_df <- test_measure_data |>
    dplyr::bind_rows(.id = "aggregation") |>
    dplyr::mutate(aggregation = dplyr::case_when(
      aggregation == "events" ~ "none",
      TRUE ~ aggregation
    ))

  expect_no_error(
    check_dataset_is_complete(
      test_report_config,
      measure_data_df
    )
  )
})

test_that("check_dataset_is_complete: it errors when data is missing", {

  measure_data_df <- test_measure_data |>
    dplyr::bind_rows(.id = "aggregation") |>
    dplyr::mutate(aggregation = dplyr::case_when(
      aggregation == "events" ~ "none",
      TRUE ~ aggregation
    ))

  report_config_plus_one <- test_report_config |>
    tibble::add_row(ref = 9999, measure_name = "test", aggregation = "week")

  # add a single row
  expect_error(
    check_dataset_is_complete(
      report_config_plus_one,
      measure_data_df
    ),
    "Data is missing for 1 report items. The first is ref 9999, 'test', aggregation: week."
  )

  report_config_plus_two <- test_report_config |>
    tibble::add_row(ref = 9998, measure_name = "test", aggregation = "none") |>
    tibble::add_row(ref = 9999, measure_name = "test", aggregation = "week")

  expect_error(
    check_dataset_is_complete(
      report_config_plus_two,
      measure_data_df
    ),
    "Data is missing for 2 report items. The first is ref 9998, 'test', aggregation: none."
  )
})



# check measure config
test_that("check measure config: coerces refs to character vectors", {
  # create the error by assigning numeric refs
  measure_config <- tibble::tibble(
    ref = c(1, 2, 3),
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
    rebase_comment = c(NA, NA, NA),
    allowable_days_lag = NA,
    reviewed_at = NA,
    escalated_to = NA
  )

  r <- check_measure_config(measure_config)

  expect_equal(
    r$ref,
    c("1", "2", "3")
  )
})

test_that("check measure config: errors helpfully when column names are missing or mis-spelled", {

  # create the error by omitting a required column (unit)
  measure_config <- tibble::tibble(
    ref = c("1", "2", "3"),
    measure_name = c("M1", "M2", "M3"),
    data_source = c("S1", "S2", "S3"),
    data_owner = c("O1", "O2", "O3"),
    accountable_person = c("L1", "L2", "L3"),
    # unit = c("Integer", "Decimal", "%"),
    improvement_direction = c("Neutral", "Increase", "Decrease"),
    target = c(NA, 10, 0.2),
    target_set_by = c(NA, "T2", "T3"),
    data_quality = c("RRRR", "AAAA", "GGGG"),
    baseline_period = c(12L, 12L, 12L),
    rebase_dates = c(NA, NA, NA),
    rebase_comment = c(NA, NA, NA)
  )

  expect_error(
    check_measure_config(measure_config),
    "check_for_required_columns: Column 'unit' is missing from the 'measure_config' data frame. Check for typos in the column names."
  )

  # error persists when the column is mis-spelled
  measure_config <- tibble::tibble(
    ref = c("1", "2", "3"),
    measure_name = c("M1", "M2", "M3"),
    data_source = c("S1", "S2", "S3"),
    data_owner = c("O1", "O2", "O3"),
    accountable_person = c("L1", "L2", "L3"),
    Unit = c("Integer", "Decimal", "%"),
    improvement_direction = c("Neutral", "Increase", "Decrease"),
    target = c(NA, 10, 0.2),
    target_set_by = c(NA, "T2", "T3"),
    data_quality = c("RRRR", "AAAA", "GGGG"),
    baseline_period = c(12L, 12L, 12L),
    rebase_dates = c(NA, NA, NA),
    rebase_comment = c(NA, NA, NA)
  )

  expect_error(
    check_measure_config(measure_config),
    "check_for_required_columns: Column 'unit' is missing from the 'measure_config' data frame. Check for typos in the column names."
  )
})




test_that("check measure config: invalid improvement_direction throws an error", {
  measure_config <- tibble::tibble(
    ref = "1", measure_name = "M1", data_source = "S1",
    data_owner = "O1", accountable_person = "L1",
    unit = "integer", improvement_direction = "upward",
    target = NA, target_set_by = NA, data_quality = "GGGG",
    rebase_dates = NA, rebase_comment = NA
  )

  expect_error(
    check_measure_config(measure_config),
    "'improvement_direction' must be one of.*Invalid value\\(s\\): upward"
  )
})

test_that("check measure config: valid improvement_direction values are accepted", {
  for (dir in c("increase", "Increase", "decrease", "Decrease", "neutral", "Neutral")) {
    measure_config <- tibble::tibble(
      ref = "1", measure_name = "M1", data_source = "S1",
      data_owner = "O1", accountable_person = "L1",
      unit = "integer", improvement_direction = dir,
      target = NA, target_set_by = NA, data_quality = "GGGG",
      rebase_dates = NA, rebase_comment = NA
    )
    expect_no_error(check_measure_config(measure_config))
  }
})

test_that("check measure config: invalid unit throws an error", {
  measure_config <- tibble::tibble(
    ref = "1", measure_name = "M1", data_source = "S1",
    data_owner = "O1", accountable_person = "L1",
    unit = "percent", improvement_direction = "increase",
    target = NA, target_set_by = NA, data_quality = "GGGG",
    rebase_dates = NA, rebase_comment = NA
  )

  expect_error(
    check_measure_config(measure_config),
    "'unit' must be one of.*Invalid value\\(s\\): percent"
  )
})

test_that("check measure config: valid unit values are accepted", {
  for (u in c("integer", "Integer", "decimal", "Decimal", "%")) {
    measure_config <- tibble::tibble(
      ref = "1", measure_name = "M1", data_source = "S1",
      data_owner = "O1", accountable_person = "L1",
      unit = u, improvement_direction = "increase",
      target = NA, target_set_by = NA, data_quality = "GGGG",
      rebase_dates = NA, rebase_comment = NA
    )
    expect_no_error(check_measure_config(measure_config))
  }
})



# check measure names
test_that("check measure names: happy path", {

  measure_data <- tibble::tibble(
    ref = "10",
    measure_name = "Measure 10"
  )

  measure_config <- tibble::tibble(
    ref = "10",
    measure_name = "Measure 10"
  )

  expect_no_error(
    check_measure_names(10, measure_data, measure_config)
  )
})

test_that("check measure names: warns when names do not match", {

  measure_data <- tibble::tibble(
    ref = "10",
    measure_name = "Measure 10"
  )

  measure_config <- tibble::tibble(
    ref = "10",
    # create the error
    measure_name = "A different name"
  )

  expect_warning(
    check_measure_names(10, measure_data, measure_config)
  )
})



test_that("check measure names: ignores NAs in the ref column of the measure_config", {

  measure_data <- tibble::tibble(
    ref = "10",
    measure_name = "Measure 10"
  )

  # create the error condition
  measure_config <- tibble::tibble(
    ref = c("10", NA),
    measure_name = c("Measure 10", NA)
  )

  expect_no_error(
    check_measure_names(10, measure_data, measure_config)
  )
})



# check report config
test_that("check report config: coerces refs to character vectors", {

  # assign numeric refs
  report_config <- tibble::tibble(
    ref = c(1, 2, 3, 1, 2, 3),
    measure_name = c("M1", "M2", "M3", "M1", "M2", "M3"),
    domain = c("D1", "D1", "D1", "D2", "D2", "D2"),
    spc_chart_type = c("xmr", "xmr", "xmr", "t", "t", "t"),
    aggregation = c("week", "week", "week", "month", "month", "month"),
    report_comment = NA
  )

  r <- check_report_config(report_config)

  expect_equal(
    r$ref,
    c("1", "2", "3", "1", "2", "3")
  )
})

test_that("check report config: errors helpfully when column names are missing or mis-spelled", {

  # create the error by omitting a required column ('domain')
  report_config <- tibble::tibble(
    ref = c("1", "2", "3", "1", "2", "3"),
    measure_name = c("M1", "M2", "M3", "M1", "M2", "M3"),
    # domain = c("D1", "D1", "D1", "D2", "D2", "D2"),
    spc_chart_type = c("xmr", "xmr", "xmr", "t", "t", "t"),
    aggregation = c("week", "week", "week", "month", "month", "month")
  )

  expect_error(
    check_report_config(report_config),
    "check_for_required_columns: Column 'domain' is missing from the 'report_config' data frame. Check for typos in the column names."
  )

  # error persists when the column is mis-spelled
  report_config <- tibble::tibble(
    ref = c("1", "2", "3", "1", "2", "3"),
    measure_name = c("M1", "M2", "M3", "M1", "M2", "M3"),
    DomainWithABigD = c("D1", "D1", "D1", "D2", "D2", "D2"),
    spc_chart_type = c("xmr", "xmr", "xmr", "t", "t", "t"),
    aggregation = c("week", "week", "week", "month", "month", "month")
  )

  expect_error(
    check_report_config(report_config),
    "check_for_required_columns: Column 'domain' is missing from the 'report_config' data frame. Check for typos in the column names."
  )
})

test_that("check report config: missing optional columns does not throw an error", {

  # assign numeric refs
  report_config <- tibble::tibble(
    ref = c(1, 2, 3, 1, 2, 3),
    measure_name = c("M1", "M2", "M3", "M1", "M2", "M3"),
    domain = c("D1", "D1", "D1", "D2", "D2", "D2"),
    spc_chart_type = c("xmr", "xmr", "xmr", "t", "t", "t"),
    aggregation = c("week", "week", "week", "month", "month", "month"),
    # report_comment = NA # this is an optional column
  )

  expect_message(
    check_report_config(report_config),
    "i check_for_optional_columns: Optional column 'report_comment' is missing. Adding it."
  )

})

test_that("check report config: invalid spc_chart_type throws an error", {
  report_config <- tibble::tibble(
    ref = "1", measure_name = "M1", domain = "D1",
    spc_chart_type = "bar", aggregation = "month"
  )

  expect_error(
    check_report_config(report_config),
    "'spc_chart_type' must be one of.*Invalid value\\(s\\): bar"
  )
})

test_that("check report config: invalid aggregation throws an error", {
  report_config <- tibble::tibble(
    ref = "1", measure_name = "M1", domain = "D1",
    spc_chart_type = "xmr", aggregation = "quarter"
  )

  expect_error(
    check_report_config(report_config),
    "'aggregation' must be one of.*Invalid value\\(s\\): quarter"
  )
})

test_that("check report config: spc_chart_type and aggregation are case-insensitive", {
  report_config <- tibble::tibble(
    ref = "1", measure_name = "M1", domain = "D1",
    spc_chart_type = "XMR", aggregation = "Month"
  )

  expect_no_error(check_report_config(report_config))
})

test_that("check measure_data: happy path", {

  aggregated_datasheet <- tibble::tibble(
    ref = c(1, 2, 3),
    measure_name = c("M1", "M2", "M3"),
    comment = c("comment", "comment", "comment")
  )

  events_datasheet <- tibble::tibble(
    ref = c(1, 2, 3),
    measure_name = c("M1", "M2", "M3"),
    comment = c("comment", "comment", "comment"),
    event_date_or_datetime = "there will be dates here"
  )

  measure_data <- list(
    "week" = aggregated_datasheet,
    "month" = aggregated_datasheet,
    "events" = events_datasheet
  )

  expect_no_error(
    check_measure_data(measure_data)
  )

})

test_that("check measure_data: missing columns throw an error", {

  aggregated_datasheet <- tibble::tibble(
    ref = c(1, 2, 3),
    # measure_name = c("M1", "M2", "M3"), # missing column
    comment = c("comment", "comment", "comment")
  )

  events_datasheet <- tibble::tibble(
    ref = c(1, 2, 3),
    measure_name = c("M1", "M2", "M3"),
    comment = c("comment", "comment", "comment"),
    event_date_or_datetime = "there will be dates here"
  )

  measure_data <- list(
    "week" = aggregated_datasheet,
    "month" = aggregated_datasheet,
    "events" = events_datasheet
  )

  expect_error(
    check_measure_data(measure_data),
    "check_for_required_columns: Column 'measure_name' is missing from the 'week' data frame. Check for typos in the column names."
  )

})

test_that("check_dataset_is_complete: empty report_config passes without error", {
  empty_config <- tibble::tibble(
    ref = character(), measure_name = character(), aggregation = character()
  )

  measure_data_df <- tibble::tibble(
    ref = "1", measure_name = "M1", aggregation = "month"
  )

  expect_no_error(
    check_dataset_is_complete(empty_config, measure_data_df)
  )
})



test_that("check a_data: happy path", {

  datasheet <- tibble::tibble(
    ref = c(1, 2, 3),
    measure_name = c("M1", "M2", "M3"),
    comment = c("comment", "comment", "comment"),
    "2024-01-01" = c(1, 2, 3)
  )

  a_data <- list(
    "week" = datasheet,
    "month" = datasheet
  )

  expect_no_error(
    check_a_data(a_data)
  )

})

test_that("check a_data: non-list input throws an error", {
  expect_error(
    check_a_data(data.frame(ref = 1, measure_name = "M1", comment = "c")),
    "check_a_data: The data must be a list."
  )
})

test_that("check a_data: data frame with no date columns throws an error", {
  datasheet <- tibble::tibble(
    ref = 1, measure_name = "M1", comment = "c"
    # no date columns
  )

  expect_error(
    check_a_data(list(week = datasheet)),
    "No date columns found in the 'week' sheet"
  )
})

test_that("check a_data: missing columns throw an error", {

  datasheet <- tibble::tibble(
    ref = c(1, 2, 3),
    # measure_name = c("M1", "M2", "M3"), # missing column
    comment = c("comment", "comment", "comment")
  )

  a_data <- list(
    "week" = datasheet,
    "month" = datasheet
  )

  expect_error(
    check_a_data(a_data),
    "check_for_required_columns: Column 'measure_name' is missing from the 'week' data frame. Check for typos in the column names."
  )

})

test_that("check e_data: happy path", {

  e_data <- tibble::tibble(
    ref = c(1, 2, 3),
    measure_name = c("M1", "M2", "M3"),
    comment = c("comment", "comment", "comment"),
    event_date_or_datetime = "there will be dates here"
  )

  expect_no_error(
    check_e_data(e_data)
  )

})

test_that("check e_data: missing columns throw an error", {

  e_data <- tibble::tibble(
    ref = c(1, 2, 3),
    measure_name = c("M1", "M2", "M3"),
    comment = c("comment", "comment", "comment"),
    # event_date_or_datetime = "there will be dates here" # missing column
  )

  expect_error(
    check_e_data(e_data),
    "check_for_required_columns: Column 'event_date_or_datetime' is missing from the 'events' data frame. Check for typos in the column names."
  )

})

test_that("check e_data: accepts 'id' column as alias for 'ref'", {
  e_data <- tibble::tibble(
    id = c("1", "2", "3"),
    measure_name = c("M1", "M2", "M3"),
    event_date_or_datetime = "2023-01-01"
  )

  result <- check_e_data(e_data)
  expect_true("ref" %in% names(result))
  expect_false("id" %in% names(result))
  expect_equal(result[["ref"]], c("1", "2", "3"))
})

test_that("check e_data: accepts 'ID' column as alias for 'ref'", {
  e_data <- tibble::tibble(
    ID = c("1", "2", "3"),
    measure_name = c("M1", "M2", "M3"),
    event_date_or_datetime = "2023-01-01"
  )

  result <- check_e_data(e_data)
  expect_true("ref" %in% names(result))
  expect_false("id" %in% names(result))
  expect_equal(result[["ref"]], c("1", "2", "3"))
})

test_that("check e_data: 'ref' takes precedence when both 'ref' and 'id' are present", {
  e_data <- tibble::tibble(
    ref = c("A", "B"),
    id = c("1", "2"),
    measure_name = c("M1", "M2"),
    event_date_or_datetime = "2023-01-01"
  )

  result <- check_e_data(e_data)
  expect_equal(result[["ref"]], c("A", "B"))
})

test_that("check e_data: normalises column names to lowercase", {
  e_data <- tibble::tibble(
    REF = c("1", "2"),
    MEASURE_NAME = c("M1", "M2"),
    EVENT_DATE_OR_DATETIME = "2023-01-01"
  )

  result <- check_e_data(e_data)
  expect_true("ref" %in% names(result))
  expect_true("measure_name" %in% names(result))
})
