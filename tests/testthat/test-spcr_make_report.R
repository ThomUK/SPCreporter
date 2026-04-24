# Integration tests for spcr_make_report().
# These tests actually render the Rmd and write files to a temp directory.
# They are skipped on CI because rendering takes ~30 seconds.

"spcr_make_report: html and csv files are created" |>
  test_that({
    skip_on_ci()
    skip_if(
      system.file("Rmd", "Report.Rmd", package = "SPCreporter") == "",
      "Report.Rmd not found — run devtools::load_all() first"
    )

    mockery::stub(spcr_make_report, "utils::browseURL", invisible(NULL))
    mockery::stub(spcr_make_report, "beepr::beep", invisible(NULL))

    db <- spcr_make_data_bundle(
      test_measure_data,
      test_report_config,
      test_measure_config
    )

    withr::with_tempdir({
      result <- spcr_make_report(
        data_bundle = db,
        output_directory = ".",
        output_type = c("html", "csv")
      )

      html_files <- list.files(".", pattern = "\\.html$", full.names = TRUE)
      csv_files  <- list.files(".", pattern = "\\.csv$",  full.names = TRUE)

      expect_true(result)
      expect_length(html_files, 1)
      expect_length(csv_files,  1)
      expect_gt(file.size(html_files[[1]]), 0)
      expect_gt(file.size(csv_files[[1]]),  0)
    })
  })


"spcr_make_report: html-only output creates no csv" |>
  test_that({
    skip_on_ci()
    skip_if(
      system.file("Rmd", "Report.Rmd", package = "SPCreporter") == "",
      "Report.Rmd not found — run devtools::load_all() first"
    )

    mockery::stub(spcr_make_report, "utils::browseURL", invisible(NULL))
    mockery::stub(spcr_make_report, "beepr::beep", invisible(NULL))

    db <- spcr_make_data_bundle(
      test_measure_data,
      test_report_config,
      test_measure_config
    )

    withr::with_tempdir({
      spcr_make_report(
        data_bundle = db,
        output_directory = ".",
        output_type = "html"
      )

      expect_length(list.files(".", pattern = "\\.html$"), 1)
      expect_length(list.files(".", pattern = "\\.csv$"),  0)
    })
  })


"spcr_make_report: returns invisible TRUE" |>
  test_that({
    skip_on_ci()
    skip_if(
      system.file("Rmd", "Report.Rmd", package = "SPCreporter") == "",
      "Report.Rmd not found — run devtools::load_all() first"
    )

    mockery::stub(spcr_make_report, "utils::browseURL", invisible(NULL))
    mockery::stub(spcr_make_report, "beepr::beep", invisible(NULL))

    db <- spcr_make_data_bundle(
      test_measure_data,
      test_report_config,
      test_measure_config
    )

    withr::with_tempdir({
      result <- withVisible(spcr_make_report(
        data_bundle = db,
        output_directory = ".",
        output_type = "html"
      ))

      expect_true(result$value)
      expect_false(result$visible)
    })
  })
