"happy path" |>
  test_that({
    .data <- tibble::tibble(
      ref = 1,
      measure_name = "a name",
      comment = "a comment",
      "2022-01-01" = 10,
      "2022-02-01" = 20
    )

    expect_true(inherits(.data, "data.frame"))

    expected_out <- tibble::tibble(
      ref = c(1, 1),
      measure_name = c("a name", "a name"),
      comment = c("a comment", "a comment"),
      date = lubridate::ymd(c("2022-01-01", "2022-02-01")),
      value = c(10, 20)
    )

    expect_equal(
      lengthen_measure_data(.data),
      expected_out
    )
  })




"data frame input" |>
  test_that({
    expect_error(
      lengthen_measure_data("not a data frame"),
      "lengthen_measure_data: The data must be a data frame."
    )
  })




"check input names" |>
  test_that({
    ymd_regex <- "^20[0-9]{2}-[0-9]{1,2}-[0-9]{1,2}$"
    init_cols <- c("ref", "measure_name", "comment")

    df_names <- c("ref", "measure_name", "2022-01-01", "90210")

    expect_true(
      all(purrr::map_lgl(
        df_names, \(x) x %in% init_cols |
          stringr::str_detect(x, "^[0-9]{5}$") |
          stringr::str_detect(x, ymd_regex)))
    )

    df_names <- c(df_names, "other")

    expect_false(
      all(purrr::map_lgl(
      df_names, \(x) x %in% init_cols |
        stringr::str_detect(x, "^[0-9]{5}$") |
        stringr::str_detect(x, ymd_regex)))
    )
  })


"check_pipeline" |>
  test_that({

    init_cols <- c("ref", "measure_name", "comment")

    .data <- tibble::tibble(
      ref = c(1, 2),
      measure_name = "a name",
      comment = "a comment",
      "2022-01-01" = c(10L, 12L),
      "2022-02-01" = c(NA_integer_, 20L)
    )

    out1 <- .data |>
      tidyr::pivot_longer(!any_of(init_cols), names_to = "date", values_drop_na = TRUE)

    tibble::tibble(
      ref = c(1, 2, 2),
      measure_name = "a name",
      comment = "a comment",
      date = c("2022-01-01", "2022-01-01", "2022-02-01"),
      value = c(10, 12, 20)
    ) |>
      expect_equal(out1)


    out2 <- .data |>
      tidyr::pivot_longer(!any_of(init_cols), names_to = "date", values_drop_na = TRUE) |>
      dplyr::mutate(across("date", quietly_convert_date))

    tibble::tibble(
      ref = c(1, 2, 2),
      measure_name = "a name",
      comment = "a comment",
      date = lubridate::ymd(c("2022-01-01", "2022-01-01", "2022-02-01")),
      value = c(10, 12, 20)
    ) |>
      expect_equal(out2)
  })

