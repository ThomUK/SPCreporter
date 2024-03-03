
"spcr_make_data_bundle: happy path" |>
  test_that({

    expect_no_error(
      spcr_make_data_bundle(
        test_measure_data,
        test_report_config,
        test_measure_config
      )
    )
  })

"spcr_make_data_bundle: it accepts a custom cutoff dttm" |>
  test_that({

    expect_no_error(
      spcr_make_data_bundle(
        test_measure_data,
        test_report_config,
        test_measure_config,
        data_cutoff_dttm = as.POSIXct("2023-10-31 23:59:59")
      )
    )
  })

"spcr_make_data_bundle: there is a helpful error if the 'events' worksheet is missing" |>
  test_that({

    measure_data_no_events <- test_measure_data
    measure_data_no_events[["events"]] <- NULL

    expect_error(
      spcr_make_data_bundle(
        measure_data_no_events,
        test_report_config, # note this will still be calling for t charts
        test_measure_config
      ),
      "The 'events' worksheet is missing from 'measure_data'."
    )

  })

"spcr_make_data_bundle: it is possible to make a data_bundle if no event data is supplied" |>
  test_that({

    measure_data_no_events <- test_measure_data
    measure_data_no_events[["events"]] <- tibble::tibble(
      "ref" = numeric(),
      "measure_name" = character(),
      "comment" = character(),
      "event_date_or_datetime" = date()
    )

    report_config <- test_report_config |>
      dplyr::filter(spc_chart_type != "t") # event data needed for t charts

    expect_no_error(
      spcr_make_data_bundle(
        measure_data_no_events,
        report_config,
        test_measure_config
      )
    )

  })



"test data bundle process" |>
  test_that({

    # stub out the Sys.time call with a repeating value
    mockery::stub(spcr_make_data_bundle, "Sys.time", as.POSIXct("2023-12-04 21:25:25"))

    db <- spcr_make_data_bundle(
      test_measure_data,
      test_report_config,
      test_measure_config
    )

    # some spot checks on the above conversion of the last_data_point to the
    # appropriate character format
    expect_equal(db[["last_data_point"]][[1]], "222")
    expect_equal(db[["last_data_point"]][[2]], "73%")
    expect_equal(db[["last_data_point"]][[3]], "0.46")
    expect_equal(db[["last_data_point"]][[9]], "430d")

  })


"test the whole thing" |>
  test_that({
    out <- spcr_make_data_bundle(
      measure_data = test_measure_data,
      report_config = test_report_config,
      measure_config = test_measure_config)

    expect_length(out, 26)
    expect_equal(nrow(out), nrow(test_report_config))
    expect_type(out[["ref"]], "character")
    expect_type(out[["target"]], "double")
    expect_type(out[["allowable_days_lag"]], "integer")
    expect_type(out[["measure_data"]], "list")
    expect_s3_class(out[["last_date"]], "POSIXct")
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

    expect_length(out2, 26)
    expect_equal(nrow(out2), nrow(test_report_config))

  })

# this is more properly a test for the check_measure_names() function
# but it's good to test it as part of the make_bundle() workflow too
"error for name mismatches before other errors" |>
  test_that({

    # a measure_name mismatch in the measure config will throw a warning
    test_measure_config2 <- test_measure_config |>
      dplyr::mutate(across("measure_name", \(x) stringr::str_replace(x, "Capacity", "Capaciteeee")))

    expect_warning(
      spcr_make_data_bundle(
        measure_data = test_measure_data,
        report_config = test_report_config,
        measure_config = test_measure_config2
      ),
      "check_measure_names: There is a name mismatch for measure ref: 5. The title in the data bundle is 'Capacity'. The title in the measure config is 'Capaciteeee'."
    )

    # a measure_name mismatch in the measure data will throw a warning
    test_measure_data2 <- test_measure_data |>
      purrr::modify_at("month", \(x)
      dplyr::mutate(x, across("measure_name", \(x) stringr::str_replace(x, "Widgets", "widgets"))))

    expect_warning(
      spcr_make_data_bundle(
        measure_data = test_measure_data2,
        report_config = test_report_config,
        measure_config = test_measure_config
      ),
      "check_measure_names: There is a name mismatch for measure ref: 11. The title in the data bundle is 'widgets'. The title in the measure config is 'Widgets'."
    )

    # but a measure_name change in the report config should not throw an error
    test_report_config2 <- test_report_config |>
      dplyr::mutate(across("measure_name", \(x) stringr::str_replace(x, "Widgets", "widgets")))

    expect_no_error(
      spcr_make_data_bundle(
        measure_data = test_measure_data,
        report_config = test_report_config2,
        measure_config = test_measure_config
        )
      )

  })
