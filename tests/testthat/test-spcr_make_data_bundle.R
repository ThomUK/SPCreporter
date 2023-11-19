# TODO: Use {withr} package to set usethis.quiet option for tests so that
# optional columns info messages don't clutter the output


"test checked and lengthened data step" |>
  test_that({
    measure_data <- test_measure_data |>
      check_measure_data()
    report_config <- test_report_config |>
      check_report_config()
    measure_config <- test_measure_config |>
      check_measure_config()

    expect_type(measure_data[[1]][["ref"]], "character")
    expect_type(report_config[["ref"]], "character")
    expect_type(measure_config[["ref"]], "character")
    expect_type(measure_config[["target"]], "double")
    expect_type(measure_config[["allowable_days_lag"]], "integer")

    measure_data_long <- measure_data |>
      purrr::map(lengthen_measure_data) |>
      dplyr::bind_rows(.id = "aggregation")

    expect_type(measure_data_long[["ref"]], "character")
    expect_s3_class(measure_data_long[["date"]], "Date")

    test_names <- c("aggregation", "ref", "measure_name",
                    "comment", "date", "value")

    expect_named(measure_data, unique(measure_data_long[["aggregation"]]))
    expect_named(measure_data_long, test_names)
    expect_equal(nrow(measure_data_long), 378)
  })



"test nested data step" |>
  test_that({
    measure_data <- test_measure_data |>
      check_measure_data()
    report_config <- test_report_config |>
      check_report_config()
    measure_config <- test_measure_config |>
      check_measure_config()


    measure_data_long <- measure_data |>
      purrr::map(lengthen_measure_data) |>
      dplyr::bind_rows(.id = "aggregation")

    nested_data <- report_config |>
      dplyr::left_join(measure_config, by = c("ref", "measure_name")) |>
      dplyr::nest_join(measure_data_long,
                       by = c("ref", "aggregation"),
                       name = "measure_data")

    test_names <- c(union(names(report_config), names(measure_config)), "measure_data")

    expect_equal(nrow(nested_data), nrow(report_config))
    expect_type(nested_data[["measure_data"]], "list")
    expect_type(nested_data[["measure_data"]], "list")
    expect_s3_class(nested_data[["measure_data"]][[1]], "data.frame")
    expect_s3_class(nested_data[["measure_data"]][[1]], "tbl_df")
    expect_named(nested_data, test_names)
  })





"test data bundle process" |>
  test_that({
    measure_data <- test_measure_data |>
      check_measure_data()
    report_config <- test_report_config |>
      check_report_config()
    measure_config <- test_measure_config |>
      check_measure_config()

    measure_data_long <- measure_data |>
      purrr::map(lengthen_measure_data) |>
      dplyr::bind_rows(.id = "aggregation")

    data_bundle1 <- report_config |>
      dplyr::left_join(measure_config, by = c("ref", "measure_name")) |>
      dplyr::nest_join(measure_data_long,
                       by = c("ref", "aggregation"),
                       name = "measure_data") |>
      dplyr::rowwise() |>
      dplyr::mutate(
        last_date = max(measure_data[["date"]], na.rm = TRUE),
        last_data_point = dplyr::pull(dplyr::slice_max(measure_data, date), "value")) |>
      dplyr::ungroup()

    expect_s3_class(data_bundle1[["last_date"]], "Date")

    data_bundle2 <- data_bundle1 |>
      dplyr::mutate(
        across("last_data_point", \(x) dplyr::case_when(
          is.na(x) ~ NA_character_,
          x == Inf ~ NA_character_,
          unit == "%" ~ paste0(round(x * 100, 1), "%"),
          unit == "decimal" ~ as.character(round(x, 2)),
          TRUE ~ as.character(round(x))
        )))

    # some spot checks on the above conversion of the last_data_point to the
    # appropriate character format
    expect_true(ifelse(data_bundle1[["last_data_point"]][[1]] == 385 & data_bundle1[["unit"]][[1]] == "integer", data_bundle2[["last_data_point"]][[1]] == "385", FALSE))

    expect_true(ifelse(round(data_bundle1[["last_data_point"]][[2]], 2) == 0.73 & data_bundle1[["unit"]][[2]] == "%", data_bundle2[["last_data_point"]][[2]] == "73%", FALSE))

    expect_true(ifelse(round(data_bundle1[["last_data_point"]][[3]], 2) == 0.46 & data_bundle1[["unit"]][[3]] == "decimal", data_bundle2[["last_data_point"]][[3]] == "0.46", FALSE))
  })


"test the whole thing" |>
  test_that({
    out <- spcr_make_data_bundle(
      measure_data = test_measure_data,
      report_config = test_report_config,
      measure_config = test_measure_config)

    expect_length(out, 24)
    expect_equal(nrow(out), nrow(test_report_config))
    expect_type(out[["ref"]], "character")
    expect_type(out[["target"]], "double")
    expect_type(out[["allowable_days_lag"]], "integer")
    expect_type(out[["measure_data"]], "list")
    expect_s3_class(out[["last_date"]], "Date")
    expect_type(out[["updated_to"]], "character")
    expect_type(out[["domain_heading"]], "logical")

    # set all targets to NA
    test_measure_config2 <- test_measure_config |>
      dplyr::mutate(across("target", \(x) NA_real_))

    expect_no_error(spcr_make_data_bundle(
      measure_data = test_measure_data,
      report_config = test_report_config,
      measure_config = test_measure_config2))

    out2 <- spcr_make_data_bundle(
      measure_data = test_measure_data,
      report_config = test_report_config,
      measure_config = test_measure_config2)

    expect_length(out2, 24)
    expect_equal(nrow(out2), nrow(test_report_config))

  })

# this is more properly a test for the check_measure_names() function
# but it's good to test it as part of the make_bundle() workflow too
"error for name mismatches before other errors" |>
  test_that({

    # a measure_name mismatch in the measure config will throw a warning
    test_measure_config2 <- test_measure_config |>
      dplyr::mutate(across("measure_name", \(x) stringr::str_replace(x, "Attendances", "Attendance")))

    expect_warning(spcr_make_data_bundle(
      measure_data = test_measure_data,
      report_config = test_report_config,
      measure_config = test_measure_config2),
      "check_measure_names: There is a name mismatch for measure ref: 1.\nThe title in the data bundle is 'Attendances'.\nThe title in the measure config is 'Attendance'."
      )


    # a measure_name mismatch in the measure data will throw a warning
    test_measure_data2 <- test_measure_data |>
      purrr::modify_at("month", \(x)
      dplyr::mutate(x, across("measure_name", \(x) stringr::str_replace(x, "Widgets", "widgets"))))

    expect_warning(spcr_make_data_bundle(
      measure_data = test_measure_data2,
      report_config = test_report_config,
      measure_config = test_measure_config),
      "check_measure_names: There is a name mismatch for measure ref: 11.\nThe title in the data bundle is 'widgets'.\nThe title in the measure config is 'Widgets'."
    )

    # but a measure_name change in the report config should not throw an error
    test_report_config2 <- test_report_config |>
      dplyr::mutate(across("measure_name", \(x) stringr::str_replace(x, "Widgets", "widgets")))

    expect_no_error(spcr_make_data_bundle(
      measure_data = test_measure_data,
      report_config = test_report_config2,
      measure_config = test_measure_config)
    )

  })
