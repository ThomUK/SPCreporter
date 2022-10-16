test_that("it returns dash when no target is set", {

  expect_equal(
    spcr_get_target_text(NA, "increase", "decimal"),
    "-"
  )

})

test_that("it returns Neutral when improvement dirn is neutral", {

  expect_equal(
    spcr_get_target_text(100, "neutral", "decimal"),
    "Neutral"
  )

  # but a missing target takes priority
  expect_equal(
    spcr_get_target_text(NA, "neutral", "decimal"),
    "-"
  )

})

test_that("it correctly handles less than and more than symbols", {

  # <= symbol
  expect_equal(
    spcr_get_target_text(100, "decrease", "decimal"),
    "\u2264 100"
  )

  # >= symbol
  expect_equal(
    spcr_get_target_text(100, "increase", "decimal"),
    "\u2265 100"
  )

})

test_that("it rounds and appends % to percentages", {

  # <= symbol
  expect_equal(
    spcr_get_target_text(0.9544, "decrease", "%"),
    "\u2264 95.4%"
  )

  # >= symbol
  expect_equal(
    spcr_get_target_text(0.9566, "increase", "%"),
    "\u2265 95.7%"
  )

})

test_that("it rounds decimals", {

  # <= symbol
  expect_equal(
    spcr_get_target_text(0.9544444, "decrease", "decimal"),
    "\u2264 0.95"
  )

  # >= symbol
  expect_equal(
    spcr_get_target_text(0.9566, "increase", "decimal"),
    "\u2265 0.96"
  )

})
