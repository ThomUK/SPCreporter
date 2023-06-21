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


"dates are amended to match data dates" |>
  test_that({

    measure_data <- tibble::tibble(
      date = lubridate::ymd(paste0("2022-0", 1:6, "-01"))
    )

    dates <- parse_rebase_dates('"2022-03-17", "2022-05-01", "2022-06-02"')

    expect_length(dates, 3)

    pull_closest_date <- function(date, dates_list) {
      later_dates <- dates_list[dates_list >= date]
      if (length(later_dates)) min(later_dates) else date
    }

    dates_list <- measure_data$date

    later_dates <- dates_list[dates_list >= dates[1]]
    expect_length(later_dates, 3)

    later_dates <- dates_list[dates_list >= dates[2]]
    expect_length(later_dates, 2)

    later_dates <- dates_list[dates_list >= dates[3]]
    expect_length(later_dates, 0)

    expect_equal(pull_closest_date(dates[1], dates_list), as.Date("2022-04-01"))
    expect_equal(pull_closest_date(dates[2], dates_list), as.Date("2022-05-01"))
    expect_equal(pull_closest_date(dates[3], dates_list), as.Date("2022-06-02"))

    out <- dates |>
      purrr::map_vec(pull_closest_date, dates_list = dates_list)

    expect_length(out, 3)

    expect_s3_class(out[1], "Date")
    expect_s3_class(out[2], "Date")
    expect_s3_class(out[3], "Date")

  })
