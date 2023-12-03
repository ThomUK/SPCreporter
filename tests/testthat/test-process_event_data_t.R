"process_event_data_t: happy path" |>
  test_that({

    e_data <- tibble::tibble(
      "ref" = c(123, 123, 123),
      "measure_name" = "Name",
      "comment" = "comment",
      "event_date_or_datetime" = as.Date(c("2020-01-01", "2020-01-03", "2020-01-13"))
    )

    expect_no_error(
      result <- process_event_data_t(e_data)
    )

    expect_equal(
      names(result),
      c("aggregation", "ref", "measure_name", "comment", "date", "value")
    )

    expect_equal(result[["date"]], as.Date(c("2020-01-03", "2020-01-13")))
    expect_equal(result[["value"]], c(2, 10))
    expect_equal(result[["comment"]], c(NA, NA)) # comments removed deliberately

  })

"process_event_data_t: passing in an empty event list returns NULL" |>
  test_that({

    events <- tibble::tibble(
      "ref" = numeric(),
      "measure_name" = character(),
      "comment" = character(),
      "event_date_or_datetime" = date()
    )

    expect_equal(
      process_event_data_t(events),
      NULL
    )

  })
