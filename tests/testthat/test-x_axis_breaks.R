
test_that("x_axis_break_months returns 1 for n <= 40", {
  expect_equal(x_axis_break_months(1L), 1L)
  expect_equal(x_axis_break_months(40L), 1L)
})

test_that("x_axis_break_months returns 2 for 41 <= n <= 80", {
  expect_equal(x_axis_break_months(41L), 2L)
  expect_equal(x_axis_break_months(80L), 2L)
})

test_that("x_axis_break_months returns 3 for 81 <= n <= 120", {
  expect_equal(x_axis_break_months(81L), 3L)
  expect_equal(x_axis_break_months(120L), 3L)
})

test_that("x_axis_break_months returns 6 for 121 <= n <= 240", {
  expect_equal(x_axis_break_months(121L), 6L)
  expect_equal(x_axis_break_months(240L), 6L)
})

test_that("x_axis_break_months returns 12 for n > 240", {
  expect_equal(x_axis_break_months(241L), 12L)
  expect_equal(x_axis_break_months(500L), 12L)
})

test_that("x_axis_break_dates returns NULL for 1-month breaks", {
  spc_data <- data.frame(x = as.Date(c("2020-01-01", "2022-12-01")))
  expect_null(x_axis_break_dates(spc_data, 1L))
})

test_that("x_axis_break_dates with 2-month breaks returns only Jan/Mar/May/Jul/Sep/Nov dates", {
  spc_data <- data.frame(x = seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month"))
  breaks <- x_axis_break_dates(spc_data, 2L)
  expect_true(all(as.integer(format(breaks, "%m")) %in% c(1, 3, 5, 7, 9, 11)))
})

test_that("x_axis_break_dates with 3-month breaks returns only Jan/Apr/Jul/Oct dates", {
  spc_data <- data.frame(x = seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month"))
  breaks <- x_axis_break_dates(spc_data, 3L)
  expect_true(all(as.integer(format(breaks, "%m")) %in% c(1, 4, 7, 10)))
})

test_that("x_axis_break_dates with 6-month breaks returns only Jan/Jul dates", {
  spc_data <- data.frame(x = seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month"))
  breaks <- x_axis_break_dates(spc_data, 6L)
  expect_true(all(as.integer(format(breaks, "%m")) %in% c(1, 7)))
})

test_that("x_axis_break_dates with 12-month breaks returns only January dates", {
  spc_data <- data.frame(x = seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month"))
  breaks <- x_axis_break_dates(spc_data, 12L)
  expect_true(all(as.integer(format(breaks, "%m")) == 1L))
})

test_that("x_axis_break_dates returns only dates within the data range", {
  spc_data <- data.frame(x = seq(as.Date("2020-03-01"), as.Date("2022-10-01"), by = "month"))
  for (interval in c(2L, 3L, 6L, 12L)) {
    breaks <- x_axis_break_dates(spc_data, interval)
    if (!is.null(breaks) && length(breaks) > 0) {
      expect_true(all(breaks >= min(spc_data$x) & breaks <= max(spc_data$x)))
    }
  }
})

test_that("x_axis_n_effective returns n_months for monthly aggregation", {
  expect_equal(x_axis_n_effective("month", 40L, 40L), 40L)
  expect_equal(x_axis_n_effective("month", 10L, 50L), 10L)
})

test_that("x_axis_n_effective returns n_points for weekly aggregation", {
  expect_equal(x_axis_n_effective("week", 10L, 40L), 40L)
  expect_equal(x_axis_n_effective("week", 10L, 50L), 50L)
})

test_that("weekly data with > 40 points produces thinned x-axis labels", {
  # 12 months of weekly data ≈ 52 points; n_months ≈ 12 (no thinning with old logic)
  # new logic uses n_points = 52 which exceeds threshold of 40 → thinning expected
  expect_gt(x_axis_n_effective("week", 12L, 52L), .x_axis_label_threshold)
})

test_that("weekly data with <= 40 points is not thinned", {
  expect_lte(x_axis_n_effective("week", 8L, 35L), .x_axis_label_threshold)
})

test_that("monthly data behaviour is unchanged by x_axis_n_effective", {
  # 40 months → effective = 40 → no thinning
  expect_equal(x_axis_break_months(x_axis_n_effective("month", 40L, 40L)), 1L)
  # 41 months → effective = 41 → thinning
  expect_equal(x_axis_break_months(x_axis_n_effective("month", 41L, 41L)), 2L)
})
