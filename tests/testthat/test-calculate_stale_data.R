

"it has a happy path" |>
  test_that({

    updated_to <- "31-Jan-2020"
    lag <- 0
    cutoff_dttm <- as.POSIXct("2020-01-31 23:59:59")

    expect_false(
      calculate_stale_data(updated_to, lag, cutoff_dttm)
    )
  })

"updated_to must be a simple date string" |>
  test_that({

    lag <- 0
    cutoff_dttm <- as.POSIXct("2020-01-31 23:59:59")

    # introduce the error
    updated_to <- as.POSIXct("2020-01-31")

    # this will generate a warning message due to the incorrect date format
    expect_error(
      calculate_stale_data(updated_to, lag, cutoff_dttm),
      "calculate_stale_data: Unable to convert the updated_to argument text to a valid date."
    )
  })

"the lag must be an integer (complete days only)" |>
  test_that({

    updated_to <- "31-Jan-2020"
    # introduce an error
    lag <- 0.1
    cutoff_dttm <- as.POSIXct("2020-01-31 23:59:59")

    expect_error(
      calculate_stale_data(updated_to, lag, cutoff_dttm),
      "calculate_stale_data: The lag argument must be an integer."
    )
  })

"cutoff_dttm must be a POSIXct" |>
  test_that({

    updated_to <- "31-Jan-2020"
    lag <- 0
    cutoff_dttm <- as.POSIXct("2020-01-31 23:59:59")
    # introduce an error
    cutoff_dttm <- as.Date(cutoff_dttm)

    expect_error(
      calculate_stale_data(updated_to, lag, cutoff_dttm),
      "calculate_stale_data: The cutoff_dttm argument must be a POSIXct."
    )
  })

"adding an allowable lag enables reporting in arrears" |>
  test_that({

    updated_to <- "31-Jan-2020"
    lag <- 0
    # report one month later
    cutoff_dttm <- as.POSIXct("2020-02-28 23:59:59")

    expect_true(
      calculate_stale_data(updated_to, lag, cutoff_dttm)
    )

    expect_false(
      calculate_stale_data(updated_to, 30, cutoff_dttm)
    )
  })
