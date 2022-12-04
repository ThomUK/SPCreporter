# set up basic info
updated_to <- as.Date("2020-01-31") # the date the measure is updated to
lag <- 0 # the allowable lag in days
cutoff_dttm <- as.POSIXct("2020-01-31 23:59:59")# the cutoff date time for the report

"it has a happy path" |>
  test_that({

    expect_false(
      spcr_calculate_stale_data(updated_to, lag, cutoff_dttm)
    )

    cutoff_dttm <- as.POSIXct("2020-02-01 00:00:00")# the cutoff date time for the report
    expect_true(
      spcr_calculate_stale_data(updated_to, lag, cutoff_dttm)
    )
  })

"updated_to must be a date" |>
  test_that({

    # introduce the error
    updated_to <- as.POSIXct(updated_to)

    expect_error(
      spcr_calculate_stale_data(updated_to, lag, cutoff_dttm),
      "spcr_calculate_stale_data: The updated_to argument must be a date."
    )
  })

"the lag must be an integer (complete days only)" |>
  test_that({

    # introduce the error
    lag <- 0.1

    expect_error(
      spcr_calculate_stale_data(updated_to, lag, cutoff_dttm),
      "spcr_calculate_stale_data: The lag argument must be an integer."
    )
  })

"cutoff_dttm must be a POSIXct" |>
  test_that({

    # introduce the error
    cutoff_dttm <- as.Date(cutoff_dttm)

    expect_error(
      spcr_calculate_stale_data(updated_to, lag, cutoff_dttm),
      "spcr_calculate_stale_data: The cutoff_dttm argument must be a POSIXct."
    )
  })

"adding an allowable lag enables reporting in arrears" |>
  test_that({

    updated_to <- as.Date("2020-01-31") # the date the measure is updated to
    lag <- 0 # the allowable lag in days
    cutoff_dttm <- as.POSIXct("2020-01-31 23:59:59") # the cutoff date time for the report

    expect_false(
      spcr_calculate_stale_data(updated_to, lag, cutoff_dttm)
    )

    # report one month later
    cutoff_dttm <- as.POSIXct("2020-02-28 23:59:59")
    expect_true(
      spcr_calculate_stale_data(updated_to, lag, cutoff_dttm)
    )

    expect_false(
      spcr_calculate_stale_data(updated_to, 30, cutoff_dttm)
    )

  })
