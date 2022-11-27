measure_data <- tibble::tibble(
  ref = "10",
  measure_name = "Measure 10"
)

measure_config <- tibble::tibble(
  ref = "10",
  measure_name = "Measure 10"
)

"it has a happy path" |>
  test_that({
    expect_no_error(
      spcr_check_measure_names(10, measure_data, measure_config)
    )
  })

"it errors when names do not match" |>
  test_that({
    # create the error
    measure_config$measure_name <- "A different name"

    expect_error(
      spcr_check_measure_names(10, measure_data, measure_config),
      "spcr_check_measure_names: There is a name mismatch for measure ref: 10. Check for typos or mismatching refs or data."
    )
  })

"it ignores NAs in the ref column of the measure_config" |>
  test_that({
    # create the error condition
    measure_config <- measure_config |>
      dplyr::bind_rows(
        tibble::tibble(
          ref = NA,
          measure_name = NA
        )
      )

    expect_no_error(
      spcr_check_measure_names(10, measure_data, measure_config)
    )
  })
