
measure_config <- tibble::tibble(
  ref = c("1", "2", "3"),
  measure_name = c("M1", "M2", "M3"),
  data_source = c("S1", "S2", "S3"),
  data_owner = c("O1", "O2", "O3"),
  accountable_person = c("L1", "L2", "L3"),
  unit = c("Integer", "Decimal", "%"),
  improvement_direction = c("Neutral", "Increase", "Decrease"),
  target = c(NA, 10, 0.2),
  target_set_by = c(NA, "T2", "T3"),
  data_quality = c("RRRR", "AAAA", "GGGG"),
  baseline_period = c(12L, 12L, 12L),
  rebase_dates = c(NA, NA, NA),
  rebase_comment = c(NA, NA, NA)
)

test_that("it coerces refs to character vectors", {

  # create the error by assigning numeric refs
  measure_config$ref <- c(1,2,3)

  r <- spcr_check_measure_config(measure_config)

  expect_equal(
    r$ref,
    c("1", "2", "3")
  )

})
