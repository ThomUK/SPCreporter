# These two tests document the dttm coercion bug fixed in March 2024.
# ceiling_date() on a datetime gives a different result than on a date,
# so the function must coerce to Date first.

"updatedto_text handles dttms correctly 1" |>
  test_that({
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


"get_updatedto_text: all aggregation types - Thursday date" |>
  test_that({
    d1 <- lubridate::as_date("2024-02-01") # A Thursday

    expect_identical(get_updatedto_text(d1, "none"),          "29-Feb-2024")
    expect_identical(get_updatedto_text(d1, "month"),         "29-Feb-2024")
    expect_identical(get_updatedto_text(d1, "day"),           "01-Feb-2024")
    expect_identical(get_updatedto_text(d1, "calendar_year"), "31-Dec-2024")
    expect_identical(get_updatedto_text(d1, "week"),          "04-Feb-2024") # following Sunday
  })


"get_updatedto_text: all aggregation types - Monday date" |>
  test_that({
    d1 <- lubridate::as_date("2024-01-01") # A Monday

    expect_identical(get_updatedto_text(d1, "none"),          "31-Jan-2024")
    expect_identical(get_updatedto_text(d1, "month"),         "31-Jan-2024")
    expect_identical(get_updatedto_text(d1, "day"),           "01-Jan-2024")
    expect_identical(get_updatedto_text(d1, "calendar_year"), "31-Dec-2024")
    expect_identical(get_updatedto_text(d1, "week"),          "07-Jan-2024") # following Sunday
  })


"get_updatedto_text: error cases" |>
  test_that({
    d1 <- lubridate::as_date("2024-01-01")

    expect_error(
      get_updatedto_text(d1, "quarter"),
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
    expect_error(
      get_updatedto_text(as.Date(c("2024-01-01", "2024-02-01")), "month"),
      "get_updatedto_text: Multiple values for `last_date` provided",
      fixed = TRUE
    )
  })
