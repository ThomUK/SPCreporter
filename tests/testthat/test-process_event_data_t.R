"process_event_data_t: happy path" |>
  test_that({

    e_data <- tibble::tibble(
      "ref" = c(123, 123, 123),
      "measure_name" = "Name",
      "event_date_or_datetime" = as.POSIXct(c("2020-01-01", "2020-01-03", "2020-01-13"))
    )

    cutoff_dttm <- as.POSIXct("2020-01-31 23:59:59")

    expect_no_error(
      result <- process_event_data_t(e_data, cutoff_dttm)
    )

    expect_equal(
      names(result),
      c("aggregation", "ref", "measure_name", "date", "value")
    )

    expect_equal(result[["date"]], as.POSIXct(c("2020-01-03 00:00:00", "2020-01-13 00:00:00", "2020-01-31 23:59:59")))
    expect_equal(result[["value"]], c(2, 10, 18))

  })

"process_event_data_t: passing in an empty event list returns NULL" |>
  test_that({

    events <- tibble::tibble(
      "ref" = numeric(),
      "measure_name" = character(),
      "event_date_or_datetime" = lubridate::POSIXct()
   )

    expect_equal(
      process_event_data_t(events),
      NULL
    )

  })
