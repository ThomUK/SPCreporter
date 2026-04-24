"it has a happy path" |>
  test_that({

    expect_equal(
      parse_rebase_dates("2020-01-01"),
      as.Date("2020-01-01")
    )

    expect_equal(
      parse_rebase_dates('"2020-01-01", "2020-02-01"'),
      as.Date(c("2020-01-01", "2020-02-01"))
    )
  })

"invalid dates cause an error" |>
  test_that({

    expect_error(
      parse_rebase_dates("01-01-2020"),
      "parse_rebase_dates: rebase dates must be in 'YYYY-MM-DD' format."
    )

    expect_error(
      parse_rebase_dates('"2020-01-01", "01-05-2020"'),
      "parse_rebase_dates: rebase dates must be in 'YYYY-MM-DD' format."
    )
  })
