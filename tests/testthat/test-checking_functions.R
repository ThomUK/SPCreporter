
"check_dataset_is_complete: happy path" |>
  test_that({

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

"check_dataset_is_complete: it errors when data is missing" |>
  test_that({

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
"check measure config: coerces refs to character vectors" |>
  test_that({
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

"check measure config: errors helpfully when column names are missing or mis-spelled" |>
  test_that({

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




"check measure config: invalid improvement_direction throws an error" |>
  test_that({
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

"check measure config: valid improvement_direction values are accepted" |>
  test_that({
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

"check measure config: invalid unit throws an error" |>
  test_that({
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

"check measure config: valid unit values are accepted" |>
  test_that({
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
"check measure names: happy path" |>
  test_that({

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

"check measure names: warns when names do not match" |>
  test_that({

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



"check measure names: ignores NAs in the ref column of the measure_config" |>
  test_that({

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
"check report config: coerces refs to character vectors" |>
  test_that({

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

"check report config: errors helpfully when column names are missing or mis-spelled" |>
  test_that({

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

"check report config: missing optional columns does not throw an error" |>
  test_that({

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

"check report config: invalid spc_chart_type throws an error" |>
  test_that({
    report_config <- tibble::tibble(
      ref = "1", measure_name = "M1", domain = "D1",
      spc_chart_type = "bar", aggregation = "month"
    )

    expect_error(
      check_report_config(report_config),
      "'spc_chart_type' must be one of.*Invalid value\\(s\\): bar"
    )
  })

"check report config: invalid aggregation throws an error" |>
  test_that({
    report_config <- tibble::tibble(
      ref = "1", measure_name = "M1", domain = "D1",
      spc_chart_type = "xmr", aggregation = "quarter"
    )

    expect_error(
      check_report_config(report_config),
      "'aggregation' must be one of.*Invalid value\\(s\\): quarter"
    )
  })

"check report config: spc_chart_type and aggregation are case-insensitive" |>
  test_that({
    report_config <- tibble::tibble(
      ref = "1", measure_name = "M1", domain = "D1",
      spc_chart_type = "XMR", aggregation = "Month"
    )

    expect_no_error(check_report_config(report_config))
  })

"check measure_data: happy path" |>
  test_that({

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

"check measure_data: missing columns throw an error" |>
  test_that({

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

"check_dataset_is_complete: empty report_config passes without error" |>
  test_that({
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



"check a_data: happy path" |>
  test_that({

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

"check a_data: non-list input throws an error" |>
  test_that({
    expect_error(
      check_a_data(data.frame(ref = 1, measure_name = "M1", comment = "c")),
      "check_a_data: The data must be a list."
    )
  })

"check a_data: data frame with no date columns throws an error" |>
  test_that({
    datasheet <- tibble::tibble(
      ref = 1, measure_name = "M1", comment = "c"
      # no date columns
    )

    expect_error(
      check_a_data(list(week = datasheet)),
      "No date columns found in the 'week' sheet"
    )
  })

"check a_data: missing columns throw an error" |>
  test_that({

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

"check e_data: happy path" |>
  test_that({

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

"check e_data: missing columns throw an error" |>
  test_that({

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
