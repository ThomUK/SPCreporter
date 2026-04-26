test_that("inst/extdata contains the expected example files", {
  extdata_path <- system.file("extdata", package = "SPCreporter")
  skip_if(extdata_path == "", "package not installed")

  expect_true(file.exists(file.path(extdata_path, "data.xlsx")))
  expect_true(file.exists(file.path(extdata_path, "measure_config.xlsx")))
  expect_true(file.exists(file.path(extdata_path, "report_config.xlsx")))
})

test_that("inst/example_data no longer exists (replaced by inst/extdata)", {
  old_path <- system.file("example_data", package = "SPCreporter")
  skip_if(system.file("extdata", package = "SPCreporter") == "", "package not installed")

  expect_equal(old_path, "")
})
