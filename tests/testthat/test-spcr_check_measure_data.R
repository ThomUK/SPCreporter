"it errors if the data is not a list" |>
  test_that({
    expect_error(
      spcr_check_measure_data(tibble::tibble(this_is = "not a list")),
      "spcr_check_measure_data: The data must be a list."
    )
  })

"list contains at least one of the required items" |>
  test_that({
    expect_error(
      spcr_check_measure_data(list("Once in a blue moon" = 1)),
      "spcr_check_measure_data: Data for either 'week' or 'month' is required."
    )
  })

"list containing extra elements is allowed" |>
  test_that({
    expect_no_error(
      list(
        week = data.frame(ref = 1, measure_name = "M1", comment = NA),
        month = data.frame(ref = 2, measure_name = "M2", comment = NA),
        asdf = data.frame(some = "other data") # extra element
      ) |>
        spcr_check_measure_data()
    )
  })

"list containing either 'week' or 'month' is allowed" |>
  test_that({

    expect_no_error(
      list(
        week = data.frame(ref = 1, measure_name = "M1", comment = NA)
        # month list item is not provided
      ) |>
        spcr_check_measure_data()
    )

    expect_no_error(
      list(
        # week list item is not provided
        month = data.frame(ref = 2, measure_name = "M2", comment = NA)
      ) |>
        spcr_check_measure_data()
    )
  })

"capitalised list items are allowed" |>
  test_that({
    expect_no_error(
      list(
        Week = data.frame(ref = 1, measure_name = "M1", comment = NA) # Week not week
      ) |>
        spcr_check_measure_data()
    )
  })

measure_data <- list(
  week = tibble::tibble(
    ref = c("1", "2", "3"),
    measure_name = c("M1", "M2", "M3"),
    comment = c("comment", "comment", "comment"),
    `43836` = c(1, 3.2, 0.5),
    `43843` = c(2, 4.2, 0.6),
    `43850` = c(1, 3.2, 0.5),
    `43857` = c(2, 4.2, 0.6),
    `43864` = c(1, 3.2, 0.5),
    `43871` = c(2, 4.2, 0.6),
    `43878` = c(1, 3.2, 0.5),
    `43885` = c(2, 4.2, 0.6),
    `43892` = c(1, 3.2, 0.5),
    `43899` = c(2, 4.2, 0.6),
    `43906` = c(1, 3.2, 0.5),
    `43913` = c(2, 4.2, 0.6)
  ),
  month = tibble::tibble(
    ref = c("1", "2", "3"),
    measure_name = c("M1", "M2", "M3"),
    comment = c("comment", "comment", "comment"),
    `43831` = c(1, 3.2, 0.5),
    `43862` = c(2, 4.2, 0.6),
    `43891` = c(1, 3.2, 0.5),
    `43922` = c(2, 4.2, 0.6),
    `43952` = c(1, 3.2, 0.5),
    `43983` = c(2, 4.2, 0.6),
    `44013` = c(1, 3.2, 0.5),
    `44044` = c(2, 4.2, 0.6),
    `44075` = c(1, 3.2, 0.5),
    `44105` = c(2, 4.2, 0.6),
    `44136` = c(1, 3.2, 0.5),
    `44166` = c(2, 4.2, 0.6)
  )
)

"it coerces refs to character vectors" |>
  test_that({
    # create the error by assigning numeric refs
    measure_data[["week"]]$ref <- c(1, 2, 3)
    measure_data[["month"]]$ref <- c(1, 2, 3)

    r <- spcr_check_measure_data(measure_data)

    expect_equal(
      r[["week"]]$ref,
      c("1", "2", "3")
    )
  })

"it errors helpfully when column names are missing or mis-spelled" |>
  test_that({
    # create the error by removing a required column
    measure_data[["week"]]$ref <- NULL

    expect_error(
      spcr_check_measure_data(measure_data),
      "spcr_check_for_required_columns: Column ref is missing from the measure_data. Check for typos in the column names."
    )

    # error persists when the column is mis-spelled
    measure_data[["week"]]$Reference <- c(1, 2, 3)

    expect_error(
      spcr_check_measure_data(measure_data),
      "spcr_check_for_required_columns: Column ref is missing from the measure_data. Check for typos in the column names."
    )
  })
