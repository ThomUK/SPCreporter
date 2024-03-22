"updatedto_text handles dttms correctly 1" |>
  test_that({
    # failing test for current behaviour (11 March 2024)

    d1 <- lubridate::as_date("2024-02-01")
    d2 <- lubridate::as_datetime("2024-02-01")

    desired_result <- lubridate::as_date("2024-02-29")
    unwanted_result <- lubridate::as_datetime("2024-01-31 23:59:59")

    aggregation <- "month"

    o1 <- lubridate::ceiling_date(d1, aggregation) - 1
    o2 <- lubridate::ceiling_date(d2, aggregation) - 1

    expect_equal(o1, desired_result)
    expect_false(identical(o2, desired_result))
    expect_equal(o2, unwanted_result)
  })


"updatedto_text handles dttms correctly 2" |>
  test_that({
    # failing test for current behaviour (11 March 2024)

    d1 <- lubridate::as_date("2024-02-01")
    d2 <- lubridate::as_datetime("2024-02-01")

    # the function needs to operate on a date not a datetime
    d1 <- as.Date(d1)
    d2 <- as.Date(d2)

    desired_result <- lubridate::as_date("2024-02-29")

    aggregation <- "month"

    o1 <- lubridate::ceiling_date(d1, aggregation) - lubridate::days(1)
    o2 <- lubridate::ceiling_date(d2, aggregation) - lubridate::days(1)

    expect_equal(o1, desired_result)
    expect_equal(o2, desired_result)
  })


"general input/output test for get_updatedto_text() part 1" |>
  test_that({
    d1 <- lubridate::as_date("2024-02-01") # A Thursday
    exp_out <- "29-Feb-2024" # character not date

    od1 <- get_updatedto_text(d1, "none")
    expect_identical(od1, exp_out)

    od2 <- get_updatedto_text(d1, "month")
    expect_identical(od2, exp_out)

    od3 <- get_updatedto_text(d1, "day")
    expect_identical(od3, "01-Feb-2024") # character not date

    od4 <- get_updatedto_text(d1, "calendar_year")
    expect_identical(od4, "31-Dec-2024") # character not date

    od5 <- get_updatedto_text(d1, "week")
    # Should give us the following Sunday (4 Feb)
    expect_identical(od5, "04-Feb-2024") # character not date
  })


"general input/output test for get_updatedto_text() part 2" |>
  test_that({
    d1 <- lubridate::as_date("2024-01-01") # A Monday
    exp_out <- "31-Jan-2024" # character not date

    od1 <- get_updatedto_text(d1, "none")
    expect_identical(od1, exp_out)

    od2 <- get_updatedto_text(d1, "month")
    expect_identical(od2, exp_out)

    od3 <- get_updatedto_text(d1, "day")
    expect_identical(od3, "01-Jan-2024") # character not date

    od4 <- get_updatedto_text(d1, "calendar_year")
    expect_identical(od4, "31-Dec-2024") # character not date

    od5 <- get_updatedto_text(d1, "week")
    # Should give us the following Sunday (7 Jan)
    expect_identical(od5, "07-Jan-2024") # character not date

    # financial year still to be implemented


    # try some errors
    expect_error(
      get_updatedto_text(d1, "quarter"), # not implemented
      "get_updatedto_text: invalid aggregation (quarter) provided",
      fixed = TRUE
    )

    expect_error(
      get_updatedto_text(d1, NA),
      "get_updatedto_text: invalid aggregation (NA) provided",
      fixed = TRUE
    )

    expect_error(
      get_updatedto_text(d1, c("week", "month")),
      "get_updatedto_text: Multiple values for `aggregation` provided",
      fixed = TRUE
    )
  })
