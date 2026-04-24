test_that("it returns dash when no target is set", {
  expect_equal(
    get_target_text(NA, "increase", "decimal"),
    "-"
  )
})

test_that("it returns Neutral when improvement dirn is neutral", {
  expect_equal(
    get_target_text(100, "neutral", "decimal"),
    "Neutral"
  )

  # but a missing target takes priority
  expect_equal(
    get_target_text(NA, "neutral", "decimal"),
    "-"
  )
})

test_that("it correctly handles less than and more than symbols", {
  # <= symbol
  expect_equal(
    get_target_text(100, "decrease", "decimal"),
    "\u2264 100"
  )

  # >= symbol
  expect_equal(
    get_target_text(100, "increase", "decimal"),
    "\u2265 100"
  )
})

test_that("it rounds and appends % to percentages", {
  # <= symbol
  expect_equal(
    get_target_text(0.9544, "decrease", "%"),
    "\u2264 95.4%"
  )

  # >= symbol
  expect_equal(
    get_target_text(0.9566, "increase", "%"),
    "\u2265 95.7%"
  )
})

test_that("it rounds decimals", {
  # <= symbol
  expect_equal(
    get_target_text(0.9544444, "decrease", "decimal"),
    "\u2264 0.95"
  )

  # >= symbol
  expect_equal(
    get_target_text(0.9566, "increase", "decimal"),
    "\u2265 0.96"
  )
})

test_that("target of 0 with decrease direction omits the <= symbol", {
  # A target of zero with "decrease" is a floor: adding \u2264 0 would be
  # misleading, so the code intentionally returns the bare value.
  expect_equal(
    get_target_text(0, "decrease", "decimal"),
    "0"
  )
})

test_that("target of 100% with increase direction omits the >= symbol", {
  # A target of 1 (100%) with "increase" is a ceiling: adding \u2265 100% would
  # be misleading, so the code intentionally returns the bare value.
  expect_equal(
    get_target_text(1, "increase", "%"),
    "100%"
  )
})
